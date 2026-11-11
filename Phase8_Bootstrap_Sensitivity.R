# ============================================================================
# PHASE 8: Bootstrap CIs on headline burn-cost numbers + Rest premium sensitivity
# ----------------------------------------------------------------------------
# Two robustness pieces for the article's headline numbers (§10 worked examples):
#
#   A. Bootstrap 95% confidence intervals
#      - Cluster bootstrap at the game level, 1000 iterations.
#      - Resamples Phase 5 qualifying drives and Phase 6c TO-call states
#        jointly (same resampled games drive both subsets), so within-game
#        correlation between P(use kth) and cond_value is preserved.
#      - Phase 4 P(qualifying drive) and Phase 6 P(survives) GAMs are treated
#        as fixed (big-sample, low variance). Phase 1 EP GAM and the EP→WP
#        slope are also fixed. This is the "targeted bootstrap" — captures
#        the high-variance empirical pieces while keeping runtime tractable.
#        Reported CIs are therefore slightly narrow vs a full-pipeline refit;
#        documented as a limitation.
#
#   B. Rest premium sensitivity
#      - Rest premium enters compute_timeout_value() as a scalar add (see
#        Phase6c_CallWeightedValue.R line ~114). Current central value is
#        +0.048 EPA (CI -0.034 to +0.130, p=0.263 — not stat-sig individually).
#      - We report each headline scenario under three rest premium values:
#          rest = 0.000  (conservative: drop the not-stat-sig EPA bonus)
#          rest = 0.048  (central estimate; current pipeline)
#          rest = 0.130  (upper 95% CI bound)
#      - We do NOT report the lower CI bound (-0.034) because the pipeline
#        floors rest premium at 0 on football-theory grounds (a TO causing
#        negative EPA bonus is incoherent).
#      - Implementation: cond_value_ep_at_call already includes +0.048, so
#        cond_value_no_rest = cond_value_ep_at_call - 0.048. Add desired rest
#        premium, multiply by EP→WP slope.
#
# Output: phase8_bootstrap_sensitivity.rds + console table.
# Does NOT modify Phase 6 or any prior RDS files.
# ============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, nflfastR, mgcv, glue)

rds_dir <- file.path(getwd(), "build")
set.seed(42)  # match Phase 6c train/test split


# --- Section 1: Load Phase 1 outputs + raw pbp ------------------------------
message("Loading Phase 1 outputs...")
h1_final                <- readRDS(file.path(rds_dir, "phase1_h1_final.rds"))
empirical_dt_table      <- readRDS(file.path(rds_dir, "phase1_empirical_dt.rds"))
p_turnover_by_play_type <- readRDS(file.path(rds_dir, "phase1_p_turnover_by_play_type.rds"))
turnover_cost_table     <- readRDS(file.path(rds_dir, "phase1_turnover_cost_table.rds"))
ep_wp_calib             <- readRDS(file.path(rds_dir, "phase1_ep_wp_calib.rds"))
clock_saving_pairs      <- readRDS(file.path(rds_dir, "phase1_clock_saving_pairs.rds"))
rest_premium_obj        <- readRDS(file.path(rds_dir, "phase1_rest_premium.rds"))
rest_premium_central    <- rest_premium_obj$rest_premium_epa
message(glue("Central rest premium loaded: {round(rest_premium_central, 4)} EPA"))

# Sanity check — should be ~0.048
if (abs(rest_premium_central - 0.048) > 0.02) {
  warning("Rest premium central value (", round(rest_premium_central, 4),
          ") differs notably from documented 0.048 — sensitivity grid uses ",
          "literal 0 / 0.048 / 0.130; results may shift if pipeline value drifts.")
}

message("Loading nflfastR PBP data 2017-2024 (REG only)...")
source("load_play_by_play.R")
pbp_raw <- load_play_by_play(2017:2024)
pbp <- pbp_raw %>%
  filter(season_type == "REG", !is.na(play_type), !is.na(posteam),
         !is.na(game_seconds_remaining), !is.na(yardline_100),
         qtr %in% c(1, 2)) %>%
  arrange(game_id, play_id)


# --- Section 2: Train EP GAM (matches Phase 6c spec) ------------------------
train_idx <- sample(nrow(h1_final), floor(0.8 * nrow(h1_final)))
ep_train  <- h1_final[train_idx, ]
message("Training EP GAM (Phase 2/6c spec)...")
ep_model <- gam(
  target_net_h1_points ~
    s(yardline_100,           bs = "cr", k = 8) +
    s(game_seconds_remaining, bs = "cr", k = 8) +
    s(score_differential,     bs = "cr", k = 8) +
    s(ydstogo,                bs = "cr", k = 6) +
    ti(game_seconds_remaining, yardline_100,       k = c(6, 6)) +
    ti(game_seconds_remaining, score_differential, k = c(6, 6)) +
    defteam_timeouts_remaining      +
    drive_start_rating_differential +
    is_clock_running                +
    as.factor(down)                 +
    pass_share_in_drive_so_far      +
    had_pass_in_drive_so_far        +
    n_plays_in_drive_so_far         +
    s(advance_yards_in_drive_so_far, bs = "cr", k = 6) +
    drive_start_posteam_timeouts    +
    drive_start_yardline_100        +
    drive_start_game_seconds        +
    drive_start_score_differential  +
    timeouts_used_in_drive,
  data   = ep_train,
  method = "REML"
)


# --- Section 3: Phase 2 helper functions ------------------------------------
# Exact copies of the Phase 6c implementation, with rest_premium passed as arg
# so we can vary it for sensitivity later.
predict_ep_adjusted <- function(state_df, play_type_val = "pass") {
  raw_ep <- as.numeric(predict(ep_model, newdata = state_df))
  p_to <- p_turnover_by_play_type %>%
    filter(play_type == play_type_val) %>% pull(p_turnover)
  if (length(p_to) == 0) p_to <- 0
  cost <- sapply(state_df$game_seconds_remaining, function(gsr) {
    bucket <- case_when(gsr > 1860 ~ "> 60 sec",
                        gsr > 1830 ~ "30-60 sec",
                        TRUE       ~ "10-30 sec")
    val <- turnover_cost_table %>% filter(time_bucket == bucket) %>% pull(avg_turnover_cost)
    if (length(val) == 0) 0 else val
  })
  raw_ep - p_to * cost
}

compute_timeout_value <- function(state_df, play_type_val = "pass",
                                  rest_premium_epa = rest_premium_central) {
  n_to <- state_df$posteam_timeouts_remaining
  if (is.na(n_to) || n_to == 0L) return(NA_real_)
  row <- empirical_dt_table %>% filter(posteam_timeouts == !!n_to)
  if (nrow(row) == 0 || is.na(row$empirical_dt)) return(0)

  dt       <- row$empirical_dt
  p_last   <- row$p_last_play
  avg_gain <- row$avg_pass_gain_yds
  avg_dur  <- row$avg_play_duration

  state_clock_save <- state_df
  state_clock_save$game_seconds_remaining <- min(state_df$game_seconds_remaining + dt, 1920)
  ep_clock_save <- predict_ep_adjusted(state_clock_save, play_type_val) -
                   predict_ep_adjusted(state_df,         play_type_val)

  state_one_play <- state_df
  state_one_play$yardline_100              <- max(state_df$yardline_100 - avg_gain, 1)
  state_one_play$game_seconds_remaining    <- max(state_df$game_seconds_remaining - avg_dur, 1800)
  state_one_play$n_plays_in_drive_so_far   <- state_df$n_plays_in_drive_so_far + 1L
  state_one_play$advance_yards_in_drive_so_far <- state_df$advance_yards_in_drive_so_far + avg_gain
  ep_last_play <- predict_ep_adjusted(state_one_play, play_type_val) -
                  predict_ep_adjusted(state_df,       play_type_val)

  (1 - p_last) * ep_clock_save + p_last * ep_last_play + rest_premium_epa
}


# --- Section 4: Build per-call data (Phase 6c logic) ------------------------
# call_states = h1_final ∩ clock_saving_pairs, restricted to posteam_to >= 1.
# Compute cond_value_ep per row ONCE with central rest premium baked in.
message("Linking clock-saving pairs to h1_final and computing per-row cond_values...")
call_states <- h1_final %>%
  inner_join(
    clock_saving_pairs %>% select(game_id, play_id) %>% distinct(),
    by = c("game_id", "play_id")
  ) %>%
  filter(posteam_timeouts_remaining >= 1)

message(glue("Per-call sample: {nrow(call_states)} TO-call states"))

# This is the slow step (~2-3 min). Per-row compute_timeout_value at central rest.
call_states$cond_value_ep <- sapply(seq_len(nrow(call_states)), function(i) {
  compute_timeout_value(call_states[i, ], "pass",
                        rest_premium_epa = rest_premium_central)
})

# EP→WP slope (global, per user direction)
ep_to_wp_slope <- as.numeric(coef(lm(vegas_wp ~ ep, data = ep_wp_calib))["ep"])
message(glue("EP->WP global slope: {round(ep_to_wp_slope, 4)} (1 EP = {round(ep_to_wp_slope*100, 2)}% WP)"))

call_states$cond_value_wp_pct <- call_states$cond_value_ep * ep_to_wp_slope * 100
# tos_at_call = posteam_timeouts_remaining at the call moment
call_states$tos_at_call <- as.integer(call_states$posteam_timeouts_remaining)


# --- Section 5: Build per-drive data (Phase 5 logic) ------------------------
message("Building per-drive Phase 5 data...")
drive_starts <- pbp %>%
  filter(!is.na(down), !is.na(posteam_timeouts_remaining)) %>%
  group_by(game_id, fixed_drive) %>%
  slice_min(play_id, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(min_req = 1800 + if_else(yardline_100 > 45,
                                  40 * ((yardline_100 - 45)/45)^1.5, 0),
         qualifies = game_seconds_remaining <= 1920 &
                     game_seconds_remaining > 1800 &
                     game_seconds_remaining >= min_req) %>%
  filter(qualifies) %>%
  select(game_id, fixed_drive,
         drive_start_to = posteam_timeouts_remaining)

drive_to_usage <- pbp %>%
  semi_join(drive_starts, by = c("game_id", "fixed_drive")) %>%
  filter(!is.na(posteam_timeouts_remaining)) %>%
  group_by(game_id, fixed_drive) %>%
  summarise(to_end = min(posteam_timeouts_remaining), .groups = "drop") %>%
  inner_join(drive_starts, by = c("game_id", "fixed_drive")) %>%
  mutate(to_used = pmax(drive_start_to - to_end, 0L)) %>%
  filter(drive_start_to %in% 1:3)

message(glue("Per-drive sample: {nrow(drive_to_usage)} qualifying drives with 1-3 starting TOs"))


# --- Section 6: Load Phase 4 + Phase 6 GAMs (fixed) -------------------------
message("Loading fixed GAMs (Phase 4 P(drive), Phase 6 survival)...")
pos_prob_model <- readRDS(file.path(rds_dir, "phase4_pos_prob_model.rds"))
def_prob_model <- readRDS(file.path(rds_dir, "phase4_def_prob_model.rds"))
m_geq1 <- readRDS(file.path(rds_dir, "phase6_surv_geq1.rds"))
m_geq2 <- readRDS(file.path(rds_dir, "phase6_surv_geq2.rds"))
m_geq3 <- readRDS(file.path(rds_dir, "phase6_surv_geq3.rds"))

# Fixed-prediction helpers
P_DRIVE_BEFORE_9MIN <- 0.55
MODEL_CUTOFF_GSR    <- 2340

p_qualifying_drive <- function(gsr, yardline = 50, score_diff = 0,
                               perspective = "posteam",
                               current_to = 3L) {
  if (gsr > MODEL_CUTOFF_GSR) return(P_DRIVE_BEFORE_9MIN)
  model <- if (perspective == "posteam") pos_prob_model else def_prob_model
  newdata <- data.frame(
    game_seconds_remaining     = gsr,
    yardline_100               = yardline,
    score_differential         = score_diff,
    down                       = 1L,
    ydstogo                    = 10,
    posteam_timeouts_remaining = as.integer(current_to),
    defteam_timeouts_remaining = 3L,
    rating_differential        = 0
  )
  as.numeric(predict(model, newdata = newdata, type = "response"))
}

p_to_survives <- function(kth_to, current_to, gsr, score_diff = 0) {
  if (kth_to > current_to) return(0)
  newdata <- data.frame(
    game_seconds_remaining     = gsr,
    score_differential         = score_diff,
    posteam_timeouts_remaining = as.integer(current_to)
  )
  model <- switch(as.character(kth_to), "1" = m_geq1, "2" = m_geq2, "3" = m_geq3)
  as.numeric(predict(model, newdata = newdata, type = "response"))
}


# --- Section 7: Define scenarios from HANDOFF §10 ---------------------------
# All 8 worked examples. burn one => kth_to = current_to => tos_at_call = 1.
scenarios <- tibble(
  label      = c("Q1 start, 3 TOs -> burn one (loses 3rd)",
                 "End of Q1, 3 TOs -> burn one (loses 3rd)",
                 "Q2 ~10:00, 3 TOs -> burn one (loses 3rd)",
                 "Q2 ~8:00, 3 TOs -> burn one (loses 3rd)",
                 "Q2 ~5:00, 3 TOs -> burn one (loses 3rd)",
                 "Q1 start, 2 TOs -> burn one (loses 2nd)",
                 "Q2 ~5:00, 2 TOs -> burn one (loses 2nd)",
                 "Q2 ~5:00, 1 TO -> burn one (loses 1st)"),
  gsr        = c(3500, 2700, 2400, 2280, 2100, 3500, 2100, 2100),
  current_to = c(3,    3,    3,    3,    3,    2,    2,    1),
  kth_to     = c(3,    3,    3,    3,    3,    2,    2,    1),
  perspective= "posteam",
  yardline   = 50,
  score_diff = 0
) %>%
  mutate(tos_at_call = current_to - kth_to + 1L)

# Pre-compute fixed P(drive) and P(survives) for each scenario
scenarios <- scenarios %>%
  rowwise() %>%
  mutate(
    p_drive = p_qualifying_drive(gsr, yardline, score_diff,
                                 perspective, current_to),
    p_surv  = p_to_survives(kth_to, current_to, gsr, score_diff)
  ) %>%
  ungroup()

cat("\n--- Scenarios with fixed P(drive) and P(survives) ---\n")
print(scenarios %>% select(label, gsr, current_to, kth_to, tos_at_call,
                           p_drive, p_surv))


# --- Section 8: Point-estimate burn cost ------------------------------------
# Using the FULL (non-resampled) per-call and per-drive data.
point_p_use <- drive_to_usage %>%
  group_by(drive_start_to) %>%
  summarise(p_use_kth = mean(to_used >= drive_start_to), .groups = "drop") %>%
  arrange(drive_start_to)
point_p_use_vec <- setNames(point_p_use$p_use_kth, as.character(point_p_use$drive_start_to))

point_cond_ep <- call_states %>%
  group_by(tos_at_call) %>%
  summarise(mean_ep = mean(cond_value_ep), .groups = "drop") %>%
  arrange(tos_at_call)
point_cond_ep_vec <- setNames(point_cond_ep$mean_ep, as.character(point_cond_ep$tos_at_call))

cat("\n--- Point estimates ---\n")
cat("P(use kth | start=k):\n"); print(point_p_use_vec)
cat("cond_value EP at tos_at_call (with central rest +", round(rest_premium_central,3), "):\n")
print(point_cond_ep_vec)

# Burn-cost calculator given p_use_vec (length 3, keyed "1","2","3") and
# cond_ep_vec (length 3, keyed "1","2","3" by tos_at_call), at a chosen rest.
# Subtracts the rest premium baked into cond_ep_vec and adds the requested one.
compose_burn_cost_wp <- function(scn_row, p_use_vec, cond_ep_vec,
                                 rest_baked_in = rest_premium_central,
                                 rest_used     = rest_premium_central) {
  p_use <- as.numeric(p_use_vec[as.character(scn_row$kth_to)])
  base_ep_no_rest <- as.numeric(cond_ep_vec[as.character(scn_row$tos_at_call)]) - rest_baked_in
  cond_ep <- base_ep_no_rest + rest_used
  cond_wp_pct <- cond_ep * ep_to_wp_slope * 100
  scn_row$p_drive * scn_row$p_surv * p_use * cond_wp_pct
}

scenarios$point_wp_central <- sapply(seq_len(nrow(scenarios)), function(i) {
  compose_burn_cost_wp(scenarios[i, ], point_p_use_vec, point_cond_ep_vec)
})

cat("\n--- Point estimate burn costs (central rest = ",
    round(rest_premium_central, 3), ") ---\n")
print(scenarios %>% select(label, point_wp_central) %>%
        mutate(point_wp_central = round(point_wp_central, 4)))


# --- Section 9: Cluster bootstrap at game level -----------------------------
# Build the universe of game_ids from BOTH call_states and drive_to_usage,
# so a resampled game that has no qual drive or no TO call simply contributes
# nothing to that table (correctly).
all_game_ids <- unique(c(call_states$game_id, drive_to_usage$game_id))
n_games <- length(all_game_ids)
message(glue("\nBootstrap universe: {n_games} unique games contribute to ",
             "Phase 5 drives or Phase 6c calls"))

N_BOOT <- 1000L
boot_results <- matrix(NA_real_, nrow = N_BOOT, ncol = nrow(scenarios))
colnames(boot_results) <- scenarios$label

# Pre-split for speed
calls_by_game  <- split(call_states[, c("tos_at_call", "cond_value_ep")],
                        call_states$game_id)
drives_by_game <- split(drive_to_usage[, c("drive_start_to", "to_used")],
                        drive_to_usage$game_id)

message(glue("Running {N_BOOT} bootstrap iterations..."))
t0 <- Sys.time()
for (b in seq_len(N_BOOT)) {
  game_sample <- sample(all_game_ids, size = n_games, replace = TRUE)

  # Resampled call_states
  call_chunks <- calls_by_game[game_sample]
  call_chunks <- call_chunks[!sapply(call_chunks, is.null)]
  if (length(call_chunks) == 0) next
  boot_calls <- do.call(rbind, call_chunks)

  # Resampled drive_to_usage
  drive_chunks <- drives_by_game[game_sample]
  drive_chunks <- drive_chunks[!sapply(drive_chunks, is.null)]
  if (length(drive_chunks) == 0) next
  boot_drives <- do.call(rbind, drive_chunks)

  # Bootstrap P(use kth | start=k)
  p_use_boot <- boot_drives %>%
    group_by(drive_start_to) %>%
    summarise(p_use_kth = mean(to_used >= drive_start_to), .groups = "drop")
  p_use_vec_boot <- setNames(p_use_boot$p_use_kth,
                             as.character(p_use_boot$drive_start_to))

  # Bootstrap cond_value mean by tos_at_call
  cond_boot <- boot_calls %>%
    group_by(tos_at_call) %>%
    summarise(mean_ep = mean(cond_value_ep), .groups = "drop")
  cond_vec_boot <- setNames(cond_boot$mean_ep, as.character(cond_boot$tos_at_call))

  # Compose burn cost for each scenario
  for (i in seq_len(nrow(scenarios))) {
    boot_results[b, i] <- compose_burn_cost_wp(
      scenarios[i, ], p_use_vec_boot, cond_vec_boot
    )
  }
  if (b %% 100 == 0) {
    elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    message(glue("  iter {b}/{N_BOOT}, elapsed {round(elapsed,1)}s"))
  }
}
t1 <- Sys.time()
message(glue("Bootstrap complete in {round(as.numeric(difftime(t1,t0,units='secs')),1)} sec"))

# Compute CIs
ci_lo <- apply(boot_results, 2, function(x) quantile(x, 0.025, na.rm = TRUE))
ci_hi <- apply(boot_results, 2, function(x) quantile(x, 0.975, na.rm = TRUE))
ci_med <- apply(boot_results, 2, function(x) quantile(x, 0.50,  na.rm = TRUE))
n_valid <- apply(boot_results, 2, function(x) sum(!is.na(x)))

scenarios$boot_median  <- ci_med
scenarios$boot_ci_lo   <- ci_lo
scenarios$boot_ci_hi   <- ci_hi
scenarios$boot_n_valid <- n_valid

cat("\n=== Bootstrap 95% CIs (game-level cluster, N=", N_BOOT, ") ===\n")
print(scenarios %>%
        mutate(across(c(point_wp_central, boot_median, boot_ci_lo, boot_ci_hi),
                      ~ round(.x, 4))) %>%
        select(label, point = point_wp_central, boot_median,
               ci_lo = boot_ci_lo, ci_hi = boot_ci_hi))


# --- Section 10: Rest premium sensitivity -----------------------------------
# Point estimates only — three rest values per scenario.
REST_GRID <- c(zero = 0.000, central = 0.048, upper_ci = 0.130)

rest_sens <- sapply(REST_GRID, function(rest_val) {
  sapply(seq_len(nrow(scenarios)), function(i) {
    compose_burn_cost_wp(scenarios[i, ], point_p_use_vec, point_cond_ep_vec,
                         rest_baked_in = rest_premium_central,
                         rest_used     = rest_val)
  })
})
colnames(rest_sens) <- paste0("wp_rest_", names(REST_GRID))
scenarios <- bind_cols(scenarios, as_tibble(rest_sens))

cat("\n=== Rest premium sensitivity (point estimates, three rest values) ===\n")
print(scenarios %>%
        mutate(across(starts_with("wp_rest_"), ~ round(.x, 4))) %>%
        select(label, wp_rest_zero, wp_rest_central, wp_rest_upper_ci))

cat("\n  Interpretation: every scenario's burn cost shifts by the same scalar\n")
cat("  amount when rest premium changes, because rest premium enters as a flat\n")
cat("  scalar add on the cond_value before composition. Relative shifts vary\n")
cat("  because the base cond_value differs by tos_at_call.\n")


# --- Section 11: Save outputs ------------------------------------------------
phase8_output <- list(
  scenarios            = scenarios,
  point_p_use          = point_p_use_vec,
  point_cond_ep        = point_cond_ep_vec,
  ep_to_wp_slope       = ep_to_wp_slope,
  rest_premium_central = rest_premium_central,
  rest_grid            = REST_GRID,
  n_boot               = N_BOOT,
  n_games_universe     = n_games,
  n_call_states        = nrow(call_states),
  n_drives             = nrow(drive_to_usage),
  boot_raw             = boot_results,  # full 1000 x 8 matrix for ad-hoc diagnostics
  note = paste(
    "Targeted game-level cluster bootstrap (N=1000) of the small-sample",
    "empirical pieces (Phase 5 P(use) and Phase 6c cond_value at tos_at_call).",
    "Phase 4 P(drive), Phase 6 P(survives), Phase 1 EP GAM, and EP->WP slope",
    "treated as fixed. CIs are slightly narrow vs a full-pipeline refit.",
    "Rest premium sensitivity reports point estimates at rest in {0, +0.048,",
    "+0.130}; the lower CI bound -0.034 is omitted because the pipeline",
    "floors rest at 0 on football-theory grounds."
  )
)
saveRDS(phase8_output, file.path(rds_dir, "phase8_bootstrap_sensitivity.rds"))
message(glue("\nSaved phase8_bootstrap_sensitivity.rds to {rds_dir}"))
message("Phase 8 complete.")
