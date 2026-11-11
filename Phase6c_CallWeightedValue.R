# ============================================================================
# PHASE 6c: Call-weighted state-conditional TO value
# ----------------------------------------------------------------------------
# Phase 6b averaged conditional TO value uniformly across all qualifying-drive
# plays. That treats every play as equally likely to be the moment a TO is
# called. In reality coaches call TOs strategically — at higher-leverage
# moments — so the realized E[TO value | a TO is actually called] is higher
# than the play-uniform average.
#
# This script computes the conditional value at ONLY the plays where a
# clock-saving TO was actually called in the data (clock_saving_pairs from
# Phase 1), and averages those. The result is the empirical expected value
# of an offensive clock-saving TO call in the H1 2-min drill — the cleanest
# answer to "in practice, what is a TO call worth?"
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
clock_saving_pairs      <- readRDS(file.path(rds_dir, "phase1_clock_saving_pairs.rds"))
rest_premium_obj        <- tryCatch(
  readRDS(file.path(rds_dir, "phase1_rest_premium.rds")),
  error = function(e) list(rest_premium_epa = 0)
)
rest_premium_epa <- rest_premium_obj$rest_premium_epa

message(glue("h1_final: {nrow(h1_final)} rows"))
message(glue("clock_saving_pairs: {nrow(clock_saving_pairs)} TO-call moments"))
message(glue("rest_premium: {round(rest_premium_epa, 4)} EPA"))


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


# --- Section 3: Replicate Phase 2 functions ---------------------------------
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


# --- Section 4: Link clock_saving_pairs to h1_final state -------------------
# clock_saving_pairs has the play-N row (the play BEFORE the TO was called).
# We want the state at that play, with the Phase 2 GAM-feature set. Join on
# (game_id, play_id). Some clock-saving plays may be from drives excluded
# from h1_final (turnover drives) — those are dropped by the inner_join.
call_states <- h1_final %>%
  inner_join(
    clock_saving_pairs %>% select(game_id, play_id) %>% distinct(),
    by = c("game_id", "play_id")
  ) %>%
  filter(posteam_timeouts_remaining >= 1)

message(glue("TO-call states linked to h1_final: {nrow(call_states)} ",
             "(of {nrow(clock_saving_pairs)} total clock-saving pairs)"))


# --- Section 5: Compute conditional value at each call state ----------------
message("Computing conditional value at TO-call states...")
call_states$cond_value_ep <- sapply(seq_len(nrow(call_states)), function(i) {
  compute_timeout_value(call_states[i, ], "pass")
})

# EP→WP slope
ep_to_wp_slope <- as.numeric(coef(lm(vegas_wp ~ ep, data = ep_wp_calib))["ep"])
call_states$cond_value_wp_pct <- call_states$cond_value_ep * ep_to_wp_slope * 100


# --- Section 6: Summary statistics ------------------------------------------
mean_ep   <- mean(call_states$cond_value_ep,    na.rm = TRUE)
median_ep <- median(call_states$cond_value_ep,  na.rm = TRUE)
mean_wp   <- mean(call_states$cond_value_wp_pct, na.rm = TRUE)
median_wp <- median(call_states$cond_value_wp_pct, na.rm = TRUE)

cat("\n=== Call-weighted conditional TO value (at actual TO-call states) ===\n")
cat(sprintf("  Mean   EP: %.4f  (vs Phase 6b uniform 0.314 → ratio %.2fx; vs old flat 0.30 → %.2fx)\n",
            mean_ep, mean_ep / 0.314, mean_ep / 0.30))
cat(sprintf("  Median EP: %.4f\n", median_ep))
cat(sprintf("  Mean   WP%%: %.4f  (vs Phase 6b 1.44%% → %.2fx; vs old flat 1.30%% → %.2fx)\n",
            mean_wp, mean_wp / 1.44, mean_wp / 1.30))
cat(sprintf("  Median WP%%: %.4f\n", median_wp))

cat("\n=== By posteam_timeouts at call (per-TO-of-drive proxy) ===\n")
by_to_count <- call_states %>%
  group_by(posteam_timeouts_remaining) %>%
  summarise(n        = n(),
            mean_ep  = mean(cond_value_ep, na.rm = TRUE),
            mean_wp  = mean(cond_value_wp_pct, na.rm = TRUE),
            .groups  = "drop")
print(by_to_count)

cat("\n=== By gsr bucket ===\n")
by_gsr <- call_states %>%
  mutate(gsr_bin = cut(game_seconds_remaining, c(1800, 1820, 1850, 1880, 1920))) %>%
  group_by(gsr_bin) %>%
  summarise(n = n(), mean_ep = mean(cond_value_ep),
            mean_wp = mean(cond_value_wp_pct), .groups = "drop")
print(by_gsr)

cat("\n=== By yardline bucket ===\n")
by_yd <- call_states %>%
  mutate(yd_bin = cut(yardline_100, c(0, 20, 40, 60, 80, 100))) %>%
  group_by(yd_bin) %>%
  summarise(n = n(), mean_ep = mean(cond_value_ep),
            mean_wp = mean(cond_value_wp_pct), .groups = "drop")
print(by_yd)


# --- Section 7: Save outputs ------------------------------------------------
phase6c_value <- list(
  representative_ep        = mean_ep,
  representative_wp        = mean_wp,
  median_ep                = median_ep,
  median_wp                = median_wp,
  by_starting_to           = by_to_count,
  ep_to_wp_slope_used      = ep_to_wp_slope,
  n_call_states            = nrow(call_states),
  note = paste(
    "Call-weighted expected conditional TO value, computed at the empirical",
    "states of actual clock-saving TO calls (clock_saving_pairs ∩ h1_final).",
    "More honest than Phase 6b (which weighted plays uniformly) because real",
    "coaches call TOs at higher-leverage moments. Replaces the flat 0.30 EP /",
    "1.30% WP previously used in Phase 6 composition."
  )
)
saveRDS(phase6c_value, file.path(rds_dir, "phase6c_call_weighted_value.rds"))
message(glue("\nSaved phase6c_call_weighted_value.rds to {rds_dir}"))
message("Phase 6c complete.")
