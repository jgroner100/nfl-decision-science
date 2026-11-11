if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, nflfastR, glue)

# Generates phase1_ep_wp_calib.rds AND phase1_empirical_dt.rds without
# re-running all of Phase 1. Run this once if you already have the other
# Phase 1 RDS files. Output goes to the current working directory.

message("Loading nflfastR PBP data for 2017-2024 (REG season only)...")
source("load_play_by_play.R")
pbp_raw <- load_play_by_play(2017:2024)

pbp <- pbp_raw %>%
  filter(
    season_type == "REG",
    !is.na(play_type),
    !is.na(posteam),
    !is.na(game_seconds_remaining),
    !is.na(yardline_100),
    qtr %in% c(1, 2)
  ) %>%
  mutate(
    half = 1L,
    rating_differential = ifelse(
      !is.na(spread_line),
      spread_line,
      qnorm(pmin(pmax(vegas_wp, 1e-6), 1 - 1e-6)) * 14
    )
  )

# Identify qualifying drives (same Rule 1 logic as Phase 1)
drive_starts <- pbp %>%
  filter(!is.na(down), !is.na(posteam_timeouts_remaining)) %>%
  group_by(game_id, fixed_drive) %>%
  slice_min(play_id, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(game_id, fixed_drive,
         drive_start_game_seconds = game_seconds_remaining,
         drive_start_yardline_100 = yardline_100)

drive_starts <- drive_starts %>%
  mutate(
    min_seconds_required = 1800 + if_else(
      drive_start_yardline_100 > 45,
      40 * ((drive_start_yardline_100 - 45) / 45) ^ 1.5,
      0
    ),
    drive_qualifies = drive_start_game_seconds <= 1920 &
      drive_start_game_seconds > 1800 &
      drive_start_game_seconds >= min_seconds_required
  )

qualifying_keys <- drive_starts %>%
  filter(drive_qualifies) %>%
  select(game_id, fixed_drive)

message(glue("Qualifying drives: {nrow(qualifying_keys)}"))

# Pull ep and vegas_wp from qualifying plays in H1 window
ep_wp_calib <- pbp %>%
  semi_join(qualifying_keys, by = c("game_id", "fixed_drive")) %>%
  filter(
    !is.na(down),
    play_type %in% c("run", "pass"),
    !is.na(ep),
    !is.na(vegas_wp)
  ) %>%
  select(ep, vegas_wp, score_differential, game_seconds_remaining)

message(glue("EP→WP calibration rows: {nrow(ep_wp_calib)}"))

# Quick sanity check: slope should be positive and ~0.03–0.06
slope <- coef(lm(vegas_wp ~ ep, data = ep_wp_calib))["ep"]
message(glue("EP→WP slope: {round(slope, 4)} ({round(slope * 100, 2)}% WP per 1 EP)"))
if (slope <= 0) warning("Slope is not positive — check data.")

rds_dir <- file.path(getwd(), "build")
saveRDS(ep_wp_calib, file.path(rds_dir, "phase1_ep_wp_calib.rds"))
message(glue("Saved phase1_ep_wp_calib.rds to {rds_dir}"))

# --- Empirical timeout clock savings -----------------------------------------
# Detect offensive timeouts by checking if posteam_timeouts_remaining decreases
# between consecutive snaps within the same drive — no timeout-row lookup needed.

# Detect penalty/no-play rows between consecutive run/pass snaps so we can
# exclude pairs where the clock was stopped by something other than a timeout.
# A no_play row sitting between two snaps means a penalty (or other dead-ball
# event) stopped the clock and contaminated delta_t downward.
noplay_between <- pbp %>%
  semi_join(qualifying_keys, by = c("game_id", "fixed_drive")) %>%
  filter(play_type == "no_play") %>%
  select(game_id, fixed_drive, noplay_play_id = play_id)

snap_pairs <- pbp %>%
  semi_join(qualifying_keys, by = c("game_id", "fixed_drive")) %>%
  filter(!is.na(down), play_type %in% c("run", "pass")) %>%
  group_by(game_id, fixed_drive) %>%
  arrange(play_id) %>%
  mutate(
    next_play_id          = lead(play_id),
    next_gsr              = lead(game_seconds_remaining),
    next_posteam_timeouts = lead(posteam_timeouts_remaining),
    next_defteam_timeouts = lead(defteam_timeouts_remaining),
    delta_t               = game_seconds_remaining - next_gsr,
    pre_to_timeouts       = posteam_timeouts_remaining,
    had_off_timeout       = !is.na(next_posteam_timeouts) &
                            next_posteam_timeouts < posteam_timeouts_remaining,
    had_def_timeout       = !is.na(next_defteam_timeouts) &
                            next_defteam_timeouts < defteam_timeouts_remaining,
    play_ended_clock_running =
      (play_type == "run"  & coalesce(out_of_bounds, 0L) == 0L) |
      (play_type == "pass" & coalesce(complete_pass, 0L) == 1L &
                             coalesce(out_of_bounds, 0L) == 0L) |
      (play_type == "pass" & coalesce(sack, 0L) == 1L &
                             coalesce(out_of_bounds, 0L) == 0L)
  ) %>%
  filter(!is.na(next_gsr), delta_t >= 0, delta_t <= 60) %>%
  ungroup() %>%
  left_join(
    noplay_between %>%
      group_by(game_id, fixed_drive) %>%
      summarise(noplay_ids = list(noplay_play_id), .groups = "drop"),
    by = c("game_id", "fixed_drive")
  ) %>%
  mutate(
    had_noplay_between = mapply(
      function(pid, npid, ids) {
        if (is.null(ids) || length(ids) == 0) return(FALSE)
        any(ids > pid & ids < npid)
      },
      play_id, next_play_id, noplay_ids
    )
  ) %>%
  select(-noplay_ids)

# --- Pull timeout rows directly from pbp_raw -----------------------------
# TO rows have posteam == NA and so get filtered out by the main `pbp`
# pipeline above. Pull them from `pbp_raw` instead. Each TO row records
# game_seconds_remaining at the moment of the TO call, which lets us measure
# the lag from play snap to TO call — the key to separating clock-saving
# timeouts from strategic late-call timeouts (e.g., bleeding clock before
# a planned FG attempt).
to_rows <- pbp_raw %>%
  semi_join(qualifying_keys, by = c("game_id", "fixed_drive")) %>%
  filter(coalesce(timeout, 0L) == 1L, !is.na(timeout_team), !is.na(game_seconds_remaining)) %>%
  select(game_id, fixed_drive,
         to_play_id = play_id,
         to_gsr     = game_seconds_remaining,
         to_team    = timeout_team)

# Match each with-TO snap pair to its actual TO row (offensive TOs only)
drive_posteams <- snap_pairs %>%
  distinct(game_id, fixed_drive, posteam) %>%
  rename(drive_posteam = posteam)

snap_pairs_with_to_lag <- snap_pairs %>%
  filter(had_off_timeout, play_ended_clock_running) %>%
  left_join(drive_posteams, by = c("game_id", "fixed_drive")) %>%
  left_join(to_rows,        by = c("game_id", "fixed_drive"),
            relationship = "many-to-many") %>%
  filter(to_team == drive_posteam,
         to_play_id > play_id, to_play_id < next_play_id) %>%
  group_by(game_id, fixed_drive, play_id, next_play_id) %>%
  slice_min(to_play_id, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(snap_to_to_lag = game_seconds_remaining - to_gsr)

# A timeout is "clock-saving" when called within ~2 sec of the play whistle.
# Plays vary in length — catch-and-run can take 10+ sec — so a 12-sec cap on
# snap_to_to_lag is the threshold that captures a real clock-saving TO across
# the realistic play-duration distribution (assumes up to ~10 sec play +
# ~2 sec to whistle the TO). Lags above this are strategic late calls (e.g.,
# bleeding clock before a planned FG); they don't reflect the clock-saving
# value we want to estimate. We also exclude pairs where the DEFENSE called
# a timeout in the same window, since their TO already stopped the clock
# and the offensive TO would have zero clock-saving value.
CLOCK_SAVING_LAG_MAX <- 12

clock_saving_pairs <- snap_pairs_with_to_lag %>%
  filter(snap_to_to_lag <= CLOCK_SAVING_LAG_MAX, !had_def_timeout)

message(glue(
  "Snap pairs: {nrow(snap_pairs)} | ",
  "With off-TO: {sum(snap_pairs$had_off_timeout)} | ",
  "TOs matched to row: {nrow(snap_pairs_with_to_lag)} | ",
  "Clock-saving TOs (lag <= {CLOCK_SAVING_LAG_MAX}s): {nrow(clock_saving_pairs)} ",
  "({round(100 * nrow(clock_saving_pairs) / nrow(snap_pairs_with_to_lag), 1)}%)"
))

# Baseline: clean clock-running pairs where the ONLY thing happening between
# snaps was the hurry-up offense — no offensive or defensive timeout, and no
# penalty (no_play row) in between. delta_t >= 10 sec floor rules out the
# remaining contamination (sub-10 sec gaps are mechanically impossible in a
# true clock-running hurry-up: play_duration + spot/snap >= ~10 sec).
#
# Yard-mix selection-bias correction: teams disproportionately call TOs after
# certain play outcomes (more after sacks/losses and big gains, fewer after
# routine short gains). Those different play outcomes have different baseline
# delta_t — a play with a 25-yard gain takes longer between snaps because the
# line of scrimmage moved further. To control for this, we bin baseline pairs
# by yards_gained, then weight each bucket's baseline by the with-TO sample's
# share in that bucket. This yields a counterfactual baseline matched to the
# play-outcome distribution of plays that actually get timeouts.
YDS_BREAKS <- c(-Inf, 0, 3, 7, 15, 30, Inf)
YDS_LABELS <- c("neg", "0-3", "3-7", "7-15", "15-30", "30+")

baseline_pairs <- snap_pairs %>%
  filter(!had_off_timeout, !had_def_timeout, !had_noplay_between,
         play_ended_clock_running, delta_t >= 10) %>%
  mutate(yds_bin = cut(yards_gained, breaks = YDS_BREAKS, labels = YDS_LABELS,
                       include.lowest = TRUE))

baseline_by_yds <- baseline_pairs %>%
  group_by(yds_bin) %>%
  summarise(base_mean_dt = mean(delta_t, na.rm = TRUE),
            n_base       = n(),
            .groups      = "drop")

# Helper: re-weight per-bin baseline by a target sample's yard-bucket mix.
matched_baseline <- function(target_subset) {
  mix <- target_subset %>%
    mutate(yds_bin = cut(yards_gained, breaks = YDS_BREAKS, labels = YDS_LABELS,
                         include.lowest = TRUE)) %>%
    count(yds_bin) %>%
    mutate(weight = n / sum(n))

  combined <- mix %>% left_join(baseline_by_yds, by = "yds_bin")
  # Fallback: any bin with no baseline observations gets the global mean
  global_base <- mean(baseline_pairs$delta_t, na.rm = TRUE)
  combined$base_mean_dt[is.na(combined$base_mean_dt)] <- global_base
  sum(combined$weight * combined$base_mean_dt)
}

# Naive (unmatched) baseline — kept for reporting/diagnostic comparison only
dt_no_timeout_naive <- mean(baseline_pairs$delta_t, na.rm = TRUE)
# Matched baseline using overall with-TO yard mix (for the overall_savings line)
dt_no_timeout       <- matched_baseline(clock_saving_pairs)

# With-TO: only the clock-saving subset, so we measure realistic savings
# rather than averaging across late strategic TOs.
dt_with_timeout <- mean(clock_saving_pairs$delta_t, na.rm = TRUE)

overall_savings <- dt_no_timeout - dt_with_timeout
message(glue(
  "Naive baseline: {round(dt_no_timeout_naive, 2)} sec | ",
  "Yard-mix-matched baseline: {round(dt_no_timeout, 2)} sec | ",
  "With timeout: {round(dt_with_timeout, 2)} sec | ",
  "Savings (matched): {round(overall_savings, 2)} sec"
))

empirical_dt_table <- clock_saving_pairs %>%
  filter(pre_to_timeouts %in% 1:3) %>%
  group_by(posteam_timeouts = pre_to_timeouts) %>%
  group_modify(~ tibble(
    avg_delta_t_with_to = mean(.x$delta_t, na.rm = TRUE),
    n_obs               = nrow(.x),
    matched_baseline    = matched_baseline(.x)
  )) %>%
  ungroup() %>%
  mutate(
    empirical_dt = matched_baseline - avg_delta_t_with_to,
    empirical_dt = if_else(is.na(empirical_dt) | empirical_dt <= 0,
                           overall_savings, empirical_dt)
  ) %>%
  arrange(posteam_timeouts)

# Last-play timeout proportion
total_tos_in_drives <- pbp %>%
  semi_join(qualifying_keys, by = c("game_id", "fixed_drive")) %>%
  filter(!is.na(posteam_timeouts_remaining)) %>%
  group_by(game_id, fixed_drive) %>%
  summarise(
    to_start = first(posteam_timeouts_remaining),
    to_end   = last(posteam_timeouts_remaining),
    .groups  = "drop"
  ) %>%
  mutate(tos_used = pmax(to_start - to_end, 0L)) %>%
  summarise(total = sum(tos_used)) %>%
  pull(total)

n_within_drive_tos <- sum(snap_pairs$had_off_timeout)
n_last_play_tos    <- max(total_tos_in_drives - n_within_drive_tos, 0)
p_last_play        <- if (total_tos_in_drives > 0) n_last_play_tos / total_tos_in_drives else 0

message(glue(
  "Total timeouts: {total_tos_in_drives} | ",
  "Within-drive: {n_within_drive_tos} | ",
  "Last-play: {n_last_play_tos} ({round(p_last_play * 100, 1)}%)"
))

avg_pass_gain_yds <- pbp %>%
  semi_join(qualifying_keys, by = c("game_id", "fixed_drive")) %>%
  filter(!is.na(down), play_type == "pass", !is.na(yards_gained)) %>%
  summarise(avg = mean(yards_gained, na.rm = TRUE)) %>%
  pull(avg)

avg_play_duration_sec <- snap_pairs %>%
  filter(play_type == "pass", !play_ended_clock_running, !had_off_timeout) %>%
  summarise(avg = mean(delta_t, na.rm = TRUE)) %>%
  pull(avg)

message(glue(
  "Avg yards/pass: {round(avg_pass_gain_yds, 1)} | ",
  "Avg play duration: {round(avg_play_duration_sec, 1)} sec"
))

empirical_dt_table <- empirical_dt_table %>%
  mutate(
    p_last_play       = p_last_play,
    avg_pass_gain_yds = avg_pass_gain_yds,
    avg_play_duration = avg_play_duration_sec
  )

message("Empirical dt table:")
print(empirical_dt_table)

saveRDS(empirical_dt_table, file.path(rds_dir, "phase1_empirical_dt.rds"))
message(glue("Saved phase1_empirical_dt.rds to {rds_dir}"))

# Also save clock_saving_pairs — needed by Phase 6c (call-weighted state-cond
# TO value). Contains every play in qualifying drives where a clock-saving
# offensive TO was called within CLOCK_SAVING_LAG_MAX sec.
saveRDS(clock_saving_pairs, file.path(rds_dir, "phase1_clock_saving_pairs.rds"))
message(glue("Saved phase1_clock_saving_pairs.rds to {rds_dir}"))


# --- EPA rest premium -------------------------------------------------------
# A timeout doesn't just freeze the clock — it gives the offense rest, time to
# pick the optimal play, and visibility into defensive alignment. This block
# estimates the EPA bonus on the play AFTER a clock-saving timeout vs the
# play after a no-TO clock-running prior play, controlling for game state.
#
# Sample: BOTH 2-min drills (H1 and H2 windows) for statistical precision.
# Even with 1100+ rest observations, the per-play EPA variance is large enough
# that the coefficient is not significant at p < 0.05. Point estimate is
# stable and modest (~0.05 EPA) across all specifications tried (raw,
# linear with controls, GAM with smooth controls, pass-only). Saving the
# central estimate as a scalar that Phase 2 can add to its timeout value.
pacman::p_load(mgcv)

pbp_2min <- pbp_raw %>%
  filter(season_type == "REG", !is.na(play_type), !is.na(posteam),
         !is.na(game_seconds_remaining), !is.na(yardline_100)) %>%
  arrange(game_id, play_id)

drive_starts_2min <- pbp_2min %>%
  filter(!is.na(down), !is.na(posteam_timeouts_remaining)) %>%
  group_by(game_id, fixed_drive) %>%
  slice_min(play_id, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    half = if_else(qtr <= 2, 1L, 2L),
    h1_q = half == 1 & game_seconds_remaining <= 1920 & game_seconds_remaining > 1800 &
           game_seconds_remaining >= 1800 + if_else(yardline_100 > 45,
                                                    40 * ((yardline_100 - 45)/45)^1.5, 0),
    h2_q = half == 2 & game_seconds_remaining <= 120  & game_seconds_remaining > 0 &
           game_seconds_remaining >= if_else(yardline_100 > 45,
                                              40 * ((yardline_100 - 45)/45)^1.5, 0),
    qual = h1_q | h2_q
  ) %>%
  filter(qual) %>%
  select(game_id, fixed_drive)

sp_2min <- pbp_2min %>%
  semi_join(drive_starts_2min, by = c("game_id", "fixed_drive")) %>%
  filter(!is.na(down), play_type %in% c("run","pass")) %>%
  group_by(game_id, fixed_drive) %>% arrange(play_id) %>%
  mutate(
    npid2 = lead(play_id),
    ng2   = lead(game_seconds_remaining),
    npt2  = lead(posteam_timeouts_remaining),
    ndt2  = lead(defteam_timeouts_remaining),
    dt2   = game_seconds_remaining - ng2,
    hot2  = !is.na(npt2) & npt2 < posteam_timeouts_remaining,
    hdt2  = !is.na(ndt2) & ndt2 < defteam_timeouts_remaining,
    cr2   = (play_type == "run"  & coalesce(out_of_bounds, 0L) == 0L) |
            (play_type == "pass" & coalesce(complete_pass, 0L) == 1L &
                                   coalesce(out_of_bounds, 0L) == 0L) |
            (play_type == "pass" & coalesce(sack, 0L) == 1L &
                                   coalesce(out_of_bounds, 0L) == 0L)
  ) %>%
  filter(!is.na(ng2), dt2 >= 0, dt2 <= 60) %>% ungroup()

to_rows_2min <- pbp_raw %>%
  semi_join(drive_starts_2min, by = c("game_id", "fixed_drive")) %>%
  filter(coalesce(timeout, 0L) == 1L, !is.na(timeout_team), !is.na(game_seconds_remaining)) %>%
  select(game_id, fixed_drive, to_play_id = play_id,
         to_gsr = game_seconds_remaining, to_team = timeout_team)

drive_posteam_2min <- sp_2min %>% distinct(game_id, fixed_drive, posteam) %>%
  rename(drive_posteam = posteam)

sp_to_2min <- sp_2min %>% filter(hot2, cr2) %>%
  left_join(drive_posteam_2min, by = c("game_id", "fixed_drive")) %>%
  left_join(to_rows_2min, by = c("game_id", "fixed_drive"),
            relationship = "many-to-many") %>%
  filter(to_team == drive_posteam, to_play_id > play_id, to_play_id < npid2) %>%
  group_by(game_id, fixed_drive, play_id, npid2) %>%
  slice_min(to_play_id, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(snap_to_to_lag2 = game_seconds_remaining - to_gsr)

next_plays_2min <- pbp_2min %>%
  semi_join(drive_starts_2min, by = c("game_id", "fixed_drive")) %>%
  filter(!is.na(down), play_type %in% c("run","pass"), !is.na(epa)) %>%
  select(game_id, fixed_drive, next_play_id = play_id,
         next_epa = epa, next_yardline_100 = yardline_100,
         next_ydstogo = ydstogo, next_down = down,
         next_score_diff = score_differential,
         next_gsr = game_seconds_remaining,
         next_posteam_to = posteam_timeouts_remaining,
         next_play_type = play_type)

noplay_2min <- pbp_2min %>%
  semi_join(drive_starts_2min, by = c("game_id", "fixed_drive")) %>%
  filter(play_type == "no_play") %>%
  select(game_id, fixed_drive, bid = play_id)

sp_2min <- sp_2min %>%
  left_join(noplay_2min %>% group_by(game_id, fixed_drive) %>%
              summarise(ids = list(bid), .groups = "drop"),
            by = c("game_id", "fixed_drive")) %>%
  mutate(had_noplay2 = mapply(function(p, n, i) {
    if (is.null(i) || length(i) == 0) return(FALSE); any(i > p & i < n)
  }, play_id, npid2, ids)) %>% select(-ids)

rest_pairs <- sp_to_2min %>% filter(snap_to_to_lag2 <= CLOCK_SAVING_LAG_MAX, !hdt2) %>%
  inner_join(next_plays_2min %>% rename(npid2 = next_play_id),
             by = c("game_id", "fixed_drive", "npid2")) %>%
  mutate(group = "rest")

tired_pairs <- sp_2min %>% filter(!hot2, !hdt2, !had_noplay2, cr2, dt2 >= 10) %>%
  inner_join(next_plays_2min %>% rename(npid2 = next_play_id),
             by = c("game_id", "fixed_drive", "npid2")) %>%
  mutate(group = "tired")

rest_combined <- bind_rows(
  rest_pairs  %>% select(next_epa, next_yardline_100, next_ydstogo, next_down,
                         next_score_diff, next_gsr, next_posteam_to, next_play_type, group),
  tired_pairs %>% select(next_epa, next_yardline_100, next_ydstogo, next_down,
                         next_score_diff, next_gsr, next_posteam_to, next_play_type, group)
) %>% filter(!is.na(next_epa), !is.na(next_yardline_100), !is.na(next_ydstogo),
             !is.na(next_down), !is.na(next_score_diff), !is.na(next_gsr),
             !is.na(next_posteam_to)) %>%
  mutate(is_rest = as.integer(group == "rest"),
         next_down_f = factor(next_down))

rest_gam <- gam(next_epa ~ is_rest +
                  s(next_yardline_100, bs = "cr", k = 8) +
                  s(next_ydstogo,      bs = "cr", k = 6) +
                  next_down_f +
                  s(next_score_diff,   bs = "cr", k = 8) +
                  s(next_gsr,          bs = "cr", k = 8) +
                  next_posteam_to,
                data = rest_combined, method = "REML")

rest_cf <- summary(rest_gam)$p.table["is_rest", ]
rest_premium_estimate <- as.numeric(rest_cf["Estimate"])
rest_premium_se       <- as.numeric(rest_cf["Std. Error"])

# Floor at 0 to avoid implausibly negative premium from noise — the literature
# and intuition both support a non-negative rest effect.
rest_premium_used <- max(rest_premium_estimate, 0)

message(glue(
  "Rest premium GAM: estimate={round(rest_premium_estimate, 4)} EPA  ",
  "SE={round(rest_premium_se, 4)}  ",
  "95% CI=({round(rest_premium_estimate - 1.96*rest_premium_se, 4)}, ",
  "{round(rest_premium_estimate + 1.96*rest_premium_se, 4)})  | ",
  "Used: {round(rest_premium_used, 4)} EPA  ",
  "(n_rest={nrow(rest_pairs)}, n_tired={nrow(tired_pairs)})"
))

rest_premium <- list(
  rest_premium_epa = rest_premium_used,
  estimate         = rest_premium_estimate,
  std_error        = rest_premium_se,
  n_rest           = nrow(rest_pairs),
  n_tired          = nrow(tired_pairs),
  ci_lower         = rest_premium_estimate - 1.96 * rest_premium_se,
  ci_upper         = rest_premium_estimate + 1.96 * rest_premium_se,
  note             = paste("Estimated from H1+H2 2-min drill plays. Point",
                           "estimate is small (~0.05 EPA) and not statistically",
                           "significant individually, but stable across specs.",
                           "Phase 2 adds this as an additive bonus to TO value.")
)

saveRDS(rest_premium, file.path(rds_dir, "phase1_rest_premium.rds"))
message(glue("Saved phase1_rest_premium.rds to {rds_dir}"))
message("Now run Phase2_EP_Timeout.R to get EP + WP timeout values.")
