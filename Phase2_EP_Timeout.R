if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, mgcv, glue)

# --- Load Phase 1 outputs ----------------------------------------------------
message("Loading Phase 1 outputs...")
rds_dir <- file.path(getwd(), "build")
h1_final                <- readRDS(file.path(rds_dir, "phase1_h1_final.rds"))
turnover_cost_table     <- readRDS(file.path(rds_dir, "phase1_turnover_cost_table.rds"))
p_turnover_by_play_type <- readRDS(file.path(rds_dir, "phase1_p_turnover_by_play_type.rds"))
ep_wp_calib             <- readRDS(file.path(rds_dir, "phase1_ep_wp_calib.rds"))
empirical_dt_table      <- readRDS(file.path(rds_dir, "phase1_empirical_dt.rds"))
rest_premium_obj        <- tryCatch(readRDS(file.path(rds_dir, "phase1_rest_premium.rds")),
                                    error = function(e) list(rest_premium_epa = 0))
rest_premium_epa        <- rest_premium_obj$rest_premium_epa
message(glue("h1_final: {nrow(h1_final)} rows"))
message(glue("Rest premium loaded: {round(rest_premium_epa, 4)} EPA"))
message(glue("EP→WP calib: {nrow(ep_wp_calib)} rows"))
message("Empirical dt table:"); print(empirical_dt_table)
message("Turnover cost table:"); print(turnover_cost_table)
message("P(turnover) by play type:"); print(p_turnover_by_play_type)


# --- Train / test split ------------------------------------------------------
set.seed(42)
train_idx <- sample(nrow(h1_final), floor(0.8 * nrow(h1_final)))
ep_train  <- h1_final[train_idx, ]
ep_test   <- h1_final[-train_idx, ]
message(glue("Train: {nrow(ep_train)} | Test: {nrow(ep_test)}"))


# --- GAM EP model ------------------------------------------------------------
message("Training GAM...")
ep_model <- gam(
  target_net_h1_points ~
    # --- Main smooth effects ---
    s(yardline_100,           bs = "cr", k = 8) +
    s(game_seconds_remaining, bs = "cr", k = 8) +
    s(score_differential,     bs = "cr", k = 8) +
    s(ydstogo,                bs = "cr", k = 6) +
    # --- Tensor product interactions ---
    # These make EP(gsr+dt) - EP(gsr) vary by field position and score,
    # so timeout value depends on WHERE and WHEN you are, not just time left.
    ti(game_seconds_remaining, yardline_100,      k = c(6, 6)) +
    ti(game_seconds_remaining, score_differential, k = c(6, 6)) +
    # --- Parametric terms ---
    defteam_timeouts_remaining      +
    drive_start_rating_differential +  # rating_differential removed — same column
    is_clock_running                +
    as.factor(down)                 +
    pass_share_in_drive_so_far      +
    had_pass_in_drive_so_far        +
    n_plays_in_drive_so_far         +
    s(advance_yards_in_drive_so_far, bs = "cr", k = 6) +
    # --- Survivorship controls ---
    drive_start_posteam_timeouts    +
    drive_start_yardline_100        +
    drive_start_game_seconds        +
    drive_start_score_differential  +
    timeouts_used_in_drive,
  data   = ep_train,
  method = "REML"
)

print(summary(ep_model))


# --- Diagnostics -------------------------------------------------------------
ep_test$pred_ep <- as.numeric(predict(ep_model, newdata = ep_test))
rmse <- sqrt(mean((ep_test$target_net_h1_points - ep_test$pred_ep)^2, na.rm = TRUE))
message(glue("Test RMSE: {round(rmse, 4)}"))

base_state <- data.frame(
  yardline_100 = 40, game_seconds_remaining = 1815, score_differential = 0,
  defteam_timeouts_remaining = 2L, ydstogo = 10, down = 1L,
  rating_differential = 0, is_clock_running = FALSE,
  pass_share_in_drive_so_far = 0.5, had_pass_in_drive_so_far = TRUE,
  n_plays_in_drive_so_far = 2L, advance_yards_in_drive_so_far = 5,
  drive_start_posteam_timeouts = 3L, drive_start_yardline_100 = 60,
  drive_start_game_seconds = 1880, drive_start_score_differential = 0,
  drive_start_rating_differential = 0, timeouts_used_in_drive = 0L
)
more_time_state <- base_state
more_time_state$game_seconds_remaining <- 1860

ep_15 <- as.numeric(predict(ep_model, newdata = base_state))
ep_60 <- as.numeric(predict(ep_model, newdata = more_time_state))
message(glue("EP at 15 sec: {round(ep_15,3)} | EP at 60 sec: {round(ep_60,3)} | More time = more EP: {ep_60 > ep_15}"))
if (ep_60 <= ep_15) warning("Time curve is inverted!")


# --- Turnover-adjusted prediction --------------------------------------------
predict_ep_adjusted <- function(state_df, play_type_val = "pass") {
  raw_ep <- as.numeric(predict(ep_model, newdata = state_df))
  p_to <- p_turnover_by_play_type %>% filter(play_type == play_type_val) %>% pull(p_turnover)
  if (length(p_to) == 0) p_to <- 0
  cost <- sapply(state_df$game_seconds_remaining, function(gsr) {
    bucket <- case_when(gsr > 1860 ~ "> 60 sec", gsr > 1830 ~ "30-60 sec", TRUE ~ "10-30 sec")
    val <- turnover_cost_table %>% filter(time_bucket == bucket) %>% pull(avg_turnover_cost)
    if (length(val) == 0) 0 else val
  })
  raw_ep - p_to * cost
}


# --- Timeout value via time-delta approach -----------------------------------
seconds_saved <- function(timeouts_remaining) {
  row <- empirical_dt_table %>% filter(posteam_timeouts == !!timeouts_remaining)
  if (nrow(row) == 0 || is.na(row$empirical_dt)) return(0)
  row$empirical_dt
}

# IMPORTANT: do NOT name the local variable `n` here. `n` is a column in
# `empirical_dt_table` (sample-size counts), and dplyr's data mask resolves
# `n` inside filter() to that column, not the function-local `n`. The filter
# then silently matches nothing and the function returns 0. Use `n_to` plus
# `!!` to force unambiguous evaluation in the caller environment.
compute_timeout_value <- function(state_df, play_type_val = "pass") {
  n_to <- state_df$posteam_timeouts_remaining
  if (is.na(n_to) || n_to == 0L) return(NA_real_)

  row <- empirical_dt_table %>% filter(posteam_timeouts == !!n_to)
  if (nrow(row) == 0 || is.na(row$empirical_dt)) return(0)

  dt       <- row$empirical_dt
  p_last   <- row$p_last_play
  avg_gain <- row$avg_pass_gain_yds
  avg_dur  <- row$avg_play_duration

  # Component 1: within-drive clock saving
  state_clock_save <- state_df
  state_clock_save$game_seconds_remaining <- min(state_df$game_seconds_remaining + dt, 1920)
  ep_clock_save <- predict_ep_adjusted(state_clock_save, play_type_val) -
                   predict_ep_adjusted(state_df,         play_type_val)

  # Component 2: last-play opportunity (one more play from current position)
  state_one_play <- state_df
  state_one_play$yardline_100              <- max(state_df$yardline_100 - avg_gain, 1)
  state_one_play$game_seconds_remaining    <- max(state_df$game_seconds_remaining - avg_dur, 1800)
  state_one_play$n_plays_in_drive_so_far   <- state_df$n_plays_in_drive_so_far + 1L
  state_one_play$advance_yards_in_drive_so_far <- state_df$advance_yards_in_drive_so_far + avg_gain
  ep_last_play <- predict_ep_adjusted(state_one_play, play_type_val) -
                  predict_ep_adjusted(state_df,       play_type_val)

  # Rest premium: the play immediately after a timeout has slightly higher
  # expected EPA than a play in hurry-up after a clock-running prior play.
  # Empirical estimate from Phase 1 (rest GAM, H1+H2 two-min drill sample).
  # Applied additively in both scenarios — both end with a "rested" next play.
  (1 - p_last) * ep_clock_save + p_last * ep_last_play + rest_premium_epa
}


# --- Example timeout value table ---------------------------------------------
base_scenarios <- expand.grid(
  yardline_100                    = c(35, 55, 75),
  game_seconds_remaining          = c(1815, 1840, 1870),
  score_differential              = c(-7, 0, 7),
  defteam_timeouts_remaining      = 2L,
  ydstogo                         = 10,
  down                            = 1L,
  rating_differential             = 0,
  is_clock_running                = FALSE,
  pass_share_in_drive_so_far      = 0.5,
  had_pass_in_drive_so_far        = TRUE,
  n_plays_in_drive_so_far         = 2L,
  advance_yards_in_drive_so_far   = 5,
  drive_start_posteam_timeouts    = 3L,
  drive_start_yardline_100        = 65,
  drive_start_game_seconds        = 1880,
  drive_start_score_differential  = 0,
  drive_start_rating_differential = 0,
  timeouts_used_in_drive          = 0L,
  stringsAsFactors = FALSE
)

for (n in 1:3) {
  scenarios_n <- base_scenarios
  scenarios_n$posteam_timeouts_remaining <- n
  base_scenarios[[paste0("to_", n, "_ep")]] <- sapply(
    seq_len(nrow(scenarios_n)),
    function(i) compute_timeout_value(scenarios_n[i, ], "pass")
  )
}

message("\n--- Incremental Timeout Value (Expected Points, pass plays) ---")
print(
  base_scenarios %>%
    select(yardline_100, game_seconds_remaining, score_differential,
           to_1_ep, to_2_ep, to_3_ep) %>%
    arrange(game_seconds_remaining, score_differential, yardline_100)
)

message("\nPhase 2 complete.")


# ==============================================================================
#   PHASE 3: Convert EP Timeout Values to Win Probability Change
# ==============================================================================
# Method: estimate local dWP/dEP slope from nflfastR's vegas_wp and ep columns
# in the H1 two-minute window. Stratified by score bucket because WP is more
# sensitive to EP swings when the game is close.
#
# Validation target: Burke (2010) and Pelechrinis put timeouts at ~1.5–3% WP
# in the second half. H1 window values should be in a comparable range.
# ==============================================================================


# --- Phase 3, Section 1: Estimate dWP/dEP slope by score bucket --------------
score_breaks <- c(-Inf, -10, -4, 4, 10, Inf)
score_labels <- c("down10+", "down4-10", "tied±4", "up4-10", "up10+")

ep_to_wp_by_score <- ep_wp_calib %>%
  mutate(score_bucket = cut(score_differential, breaks = score_breaks, labels = score_labels)) %>%
  group_by(score_bucket) %>%
  summarise(
    ep_to_wp_slope = coef(lm(vegas_wp ~ ep))["ep"],
    n              = n(),
    .groups        = "drop"
  )

ep_to_wp_global <- coef(lm(vegas_wp ~ ep, data = ep_wp_calib))["ep"]

message(glue("\nGlobal EP→WP slope: {round(ep_to_wp_global, 4)} ({round(ep_to_wp_global * 100, 2)}% WP per 1 EP)"))
message("EP→WP slope by score bucket:")
print(ep_to_wp_by_score)


# --- Phase 3, Section 2: Apply WP conversion to timeout value table ----------
timeout_wp_table <- base_scenarios %>%
  mutate(score_bucket = cut(score_differential, breaks = score_breaks, labels = score_labels)) %>%
  left_join(ep_to_wp_by_score %>% select(score_bucket, ep_to_wp_slope),
            by = "score_bucket") %>%
  mutate(
    to_1_wp_pct = round(to_1_ep * ep_to_wp_slope * 100, 2),
    to_2_wp_pct = round(to_2_ep * ep_to_wp_slope * 100, 2),
    to_3_wp_pct = round(to_3_ep * ep_to_wp_slope * 100, 2)
  )

message("\n--- Timeout Value: Expected Points and Win Probability % ---")
print(
  timeout_wp_table %>%
    select(yardline_100, game_seconds_remaining, score_differential,
           to_1_ep, to_1_wp_pct,
           to_2_ep, to_2_wp_pct,
           to_3_ep, to_3_wp_pct) %>%
    arrange(game_seconds_remaining, score_differential, yardline_100)
)

message(glue(
  "\nTimeout 1 WP range: {round(min(timeout_wp_table$to_1_wp_pct), 1)}% – ",
  "{round(max(timeout_wp_table$to_1_wp_pct), 1)}%"
))
message(glue(
  "Timeout 2 WP range: {round(min(timeout_wp_table$to_2_wp_pct), 1)}% – ",
  "{round(max(timeout_wp_table$to_2_wp_pct), 1)}%"
))
message(glue(
  "Timeout 3 WP range: {round(min(timeout_wp_table$to_3_wp_pct), 1)}% – ",
  "{round(max(timeout_wp_table$to_3_wp_pct), 1)}%"
))

message("\nPhase 3 complete.")
