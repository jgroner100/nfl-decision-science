# ============================================================================
# PHASE 6b: State-conditional TO value (replacing flat 0.30 EP / 1.30% WP)
# ----------------------------------------------------------------------------
# Phase 2 already computes a TO's value as a function of game state. Phase 6's
# composition has been using a single representative number (0.30 EP) for
# downstream multiplication. That's a midpoint of the Phase 2 base_scenarios
# table, but the actual distribution of TO calls happens across many states
# with conditional values ranging ~0.15–0.55 EP.
#
# This script computes the EXPECTED conditional TO value by:
#   1. Running Phase 2's EP GAM (re-fit here for standalone use)
#   2. Computing compute_timeout_value() at every play in the qualifying-drive
#      training data (h1_final), weighted by play frequency
#   3. Averaging across all plays where the team has ≥1 TO
#
# This is the right population because TO calls happen at PLAY states during
# the drill, not just at drive starts — drive starts cluster near gsr=1900
# (more time) and would over-state the value vs. realistic call states.
# Phase 6 multiplies P(qual drive) × P(survives) × P(use) × cond_value,
# so cond_value should reflect "what's a TO worth on a random play in the
# drill given the team uses it" — which is exactly what we compute here.
# ============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, mgcv, glue)

rds_dir <- file.path(getwd(), "build")


# --- Section 1: Load Phase 1 outputs ----------------------------------------
message("Loading Phase 1 outputs...")
h1_final                <- readRDS(file.path(rds_dir, "phase1_h1_final.rds"))
empirical_dt_table      <- readRDS(file.path(rds_dir, "phase1_empirical_dt.rds"))
p_turnover_by_play_type <- readRDS(file.path(rds_dir, "phase1_p_turnover_by_play_type.rds"))
turnover_cost_table     <- readRDS(file.path(rds_dir, "phase1_turnover_cost_table.rds"))
ep_wp_calib             <- readRDS(file.path(rds_dir, "phase1_ep_wp_calib.rds"))
rest_premium_obj        <- tryCatch(
  readRDS(file.path(rds_dir, "phase1_rest_premium.rds")),
  error = function(e) list(rest_premium_epa = 0)
)
rest_premium_epa <- rest_premium_obj$rest_premium_epa

message(glue("h1_final: {nrow(h1_final)} rows | rest_premium: {round(rest_premium_epa, 4)} EPA"))


# --- Section 2: Train Phase 2 EP GAM (identical to Phase2_EP_Timeout.R) -----
set.seed(42)
train_idx <- sample(nrow(h1_final), floor(0.8 * nrow(h1_final)))
ep_train  <- h1_final[train_idx, ]

message("Training EP GAM (same spec as Phase 2)...")
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
message("GAM trained.")


# --- Section 3: Replicate predict_ep_adjusted and compute_timeout_value -----
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

compute_timeout_value <- function(state_df, play_type_val = "pass") {
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


# --- Section 4: Compute conditional value across all qualifying-drive plays -
# Population: every play in h1_final where the team holds at least 1 TO.
# (TOs can only be called by teams that have them.) This averages the
# conditional value across the actual distribution of states at which a TO
# might be called during the drill.
plays_with_to <- h1_final %>% filter(posteam_timeouts_remaining >= 1)
message(glue("Computing conditional value at {nrow(plays_with_to)} plays..."))

# Slow per-row loop is fine here — runs once and saves to RDS
cond_values <- sapply(seq_len(nrow(plays_with_to)), function(i) {
  compute_timeout_value(plays_with_to[i, ], "pass")
})
plays_with_to$cond_value_ep <- cond_values


# --- Section 5: EP → WP slope (from Phase 3 calibration) --------------------
# Use the global EP→WP slope from ep_wp_calib. Phase 3 stratifies by score
# bucket, but for a single representative number the global slope is fine.
ep_to_wp_slope <- coef(lm(vegas_wp ~ ep, data = ep_wp_calib))["ep"]
message(glue("EP→WP slope: {round(ep_to_wp_slope, 4)} ({round(ep_to_wp_slope * 100, 2)}% per 1 EP)"))

plays_with_to$cond_value_wp_pct <- plays_with_to$cond_value_ep * ep_to_wp_slope * 100


# --- Section 6: Summary statistics ------------------------------------------
mean_ep   <- mean(plays_with_to$cond_value_ep,    na.rm = TRUE)
median_ep <- median(plays_with_to$cond_value_ep,  na.rm = TRUE)
mean_wp   <- mean(plays_with_to$cond_value_wp_pct, na.rm = TRUE)
median_wp <- median(plays_with_to$cond_value_wp_pct, na.rm = TRUE)

cat("\n=== State-conditional expected TO value (across all qualifying-drive plays) ===\n")
cat(sprintf("  Mean   conditional EP: %.4f  (vs. old flat 0.30 → ratio %.2fx)\n",
            mean_ep,   mean_ep / 0.30))
cat(sprintf("  Median conditional EP: %.4f\n", median_ep))
cat(sprintf("  Mean   conditional WP%%: %.4f  (vs. old flat 1.30%% → ratio %.2fx)\n",
            mean_wp,   mean_wp / 1.30))
cat(sprintf("  Median conditional WP%%: %.4f\n", median_wp))

cat("\n=== By starting TO count ===\n")
by_to_count <- plays_with_to %>%
  group_by(posteam_timeouts_remaining) %>%
  summarise(n        = n(),
            mean_ep  = mean(cond_value_ep, na.rm = TRUE),
            mean_wp  = mean(cond_value_wp_pct, na.rm = TRUE),
            .groups  = "drop")
print(by_to_count)

cat("\n=== By gsr bucket ===\n")
by_gsr <- plays_with_to %>%
  mutate(gsr_bin = cut(game_seconds_remaining, c(1800, 1820, 1850, 1880, 1920))) %>%
  group_by(gsr_bin) %>%
  summarise(n = n(), mean_ep = mean(cond_value_ep), mean_wp = mean(cond_value_wp_pct),
            .groups = "drop")
print(by_gsr)

cat("\n=== By yardline bucket ===\n")
by_yd <- plays_with_to %>%
  mutate(yd_bin = cut(yardline_100, c(0, 20, 40, 60, 80, 100))) %>%
  group_by(yd_bin) %>%
  summarise(n = n(), mean_ep = mean(cond_value_ep), mean_wp = mean(cond_value_wp_pct),
            .groups = "drop")
print(by_yd)


# --- Section 7: Save outputs ------------------------------------------------
phase6b_value <- list(
  representative_ep        = mean_ep,
  representative_wp        = mean_wp,
  median_ep                = median_ep,
  median_wp                = median_wp,
  by_starting_to           = by_to_count,
  ep_to_wp_slope_used      = as.numeric(ep_to_wp_slope),
  n_plays_used             = nrow(plays_with_to),
  note = paste(
    "Expected conditional TO value E[Phase2 value | TO call in qualifying drive].",
    "Computed by averaging compute_timeout_value() across all plays in h1_final",
    "with at least 1 TO, using the Phase 2 EP GAM and Phase 1 rest_premium / dt.",
    "Replaces the flat 0.30 EP / 1.30% WP previously used in Phase 6 composition."
  )
)
saveRDS(phase6b_value, file.path(rds_dir, "phase6b_state_cond_value.rds"))
message(glue("\nSaved phase6b_state_cond_value.rds to {rds_dir}"))
message("Phase 6b complete.")
