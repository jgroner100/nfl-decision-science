# ============================================================================
# Phase 1: Build H1 Timeout Value Dataset
# ----------------------------------------------------------------------------
# Goal: Produce a per-play modeling dataset of qualifying H1 plays. Each row
# is a pre-snap state with state features, survivorship controls, intent
# signals, and the net-halftime-points target. This dataset feeds the EP
# regression in Phase 2.
#
# Definitions:
#   - H1 2-min window: game_seconds_remaining in (1800, 1920].
#   - Drive qualifies (Rule 1) if its first offensive snap is in the window
#     AND game_seconds_remaining at drive start >= min_time, where
#     min_time = 0.07 * yardline_100^1.3 if yardline_100 > 45, else 0.
#   - Soft Rule 2: exclude obvious clock-burning plays (kneels and runs that
#     are clearly bleeding clock) from the per-play observation set.
#   - Target: net halftime points from this state forward, posteam perspective.
#       target = (posteam_score_at_h1_end - posteam_score_at_state)
#              - (defteam_score_at_h1_end - defteam_score_at_state)
# ============================================================================


# --- Section 0: Setup -------------------------------------------------------
if (!require("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(tidyverse, nflfastR, glue)

set.seed(123)


# --- Section 1: Load nflfastR PBP data --------------------------------------
message("Loading nflfastR PBP data for 2017-2024...")
source("load_play_by_play.R")
pbp_raw <- load_play_by_play(2017:2024)
message(glue("Loaded {nrow(pbp_raw)} plays."))


# --- Section 2: Initial filter and half assignment --------------------------
pbp <- pbp_raw %>%
  filter(
    season_type == "REG",
    !is.na(play_type),
    !is.na(posteam),
    !is.na(game_seconds_remaining),
    !is.na(yardline_100),
    qtr %in% c(1, 2, 3, 4)            # exclude OT for H1 work
  ) %>%
  mutate(
    half = if_else(qtr <= 2, 1L, 2L),
    rating_differential = ifelse(
      !is.na(spread_line),
      spread_line,
      qnorm(pmin(pmax(vegas_wp, 1e-6), 1 - 1e-6)) * 14
    )
  ) %>%
  arrange(game_id, play_id)


# --- Section 3: Compute end-of-H1 scores per game ---------------------------
# Score at start of H2's first play == score at end of H1.
h1_end_scores <- pbp %>%
  filter(half == 2) %>%
  group_by(game_id) %>%
  slice_min(play_id, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  select(
    game_id,
    home_score_at_h1_end = total_home_score,
    away_score_at_h1_end = total_away_score
  )

message(glue("Computed end-of-H1 scores for {nrow(h1_end_scores)} games."))


# --- Section 4: Restrict to H1 plays ----------------------------------------
h1 <- pbp %>%
  filter(half == 1) %>%
  inner_join(h1_end_scores, by = "game_id")


# --- Section 5: Identify drive starts (first OFFENSIVE snap of each drive) --
# A drive's first offensive snap is the first play with a non-NA `down`.
drive_starts <- h1 %>%
  filter(!is.na(down), !is.na(posteam_timeouts_remaining)) %>%
  group_by(game_id, fixed_drive) %>%
  slice_min(play_id, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(
    game_id, fixed_drive,
    drive_start_play_id           = play_id,
    drive_start_posteam           = posteam,
    drive_start_game_seconds      = game_seconds_remaining,
    drive_start_yardline_100      = yardline_100,
    drive_start_score_differential= score_differential,
    drive_start_posteam_timeouts  = posteam_timeouts_remaining,
    drive_start_defteam_timeouts  = defteam_timeouts_remaining
  )


# --- Section 6: Apply Rule 1 (window + feasibility) -------------------------
drive_starts <- drive_starts %>%
  mutate(
    min_seconds_remaining_required = 1800 + if_else(
      drive_start_yardline_100 > 45,
      40 * ((drive_start_yardline_100 - 45) / 45) ^ 1.5,
      0
    ),
    drive_start_in_window   = drive_start_game_seconds <= 1920 &
      drive_start_game_seconds > 1800,
    drive_meets_feasibility = drive_start_game_seconds >= min_seconds_remaining_required,
    drive_qualifies_rule1   = drive_start_in_window & drive_meets_feasibility
  )

qualifying_drive_keys <- drive_starts %>%
  filter(drive_qualifies_rule1) %>%
  select(game_id, fixed_drive)

message(glue(
  "Drives meeting Rule 1: {nrow(qualifying_drive_keys)} ",
  "across {dplyr::n_distinct(qualifying_drive_keys$game_id)} games."
))


# --- Section 7: Restrict to plays in qualifying drives ----------------------
h1_qualifying <- h1 %>%
  semi_join(qualifying_drive_keys, by = c("game_id", "fixed_drive")) %>%
  filter(!is.na(down)) %>%                       # offensive snaps only
  arrange(game_id, play_id) %>%
  left_join(
    drive_starts %>% select(
      game_id, fixed_drive,
      drive_start_yardline_100,
      drive_start_game_seconds,
      drive_start_score_differential,
      drive_start_posteam_timeouts
    ),
    by = c("game_id", "fixed_drive")
  )


# --- Section 8: Compute play-level state features ---------------------------
h1_qualifying <- h1_qualifying %>%
  group_by(game_id, fixed_drive) %>%
  arrange(play_id) %>%
  mutate(
    # Previous-play characteristics (within drive) for clock-running derivation
    prev_play_type      = lag(play_type),
    prev_out_of_bounds  = lag(coalesce(out_of_bounds, 0L)) == 1L,
    prev_complete_pass  = lag(coalesce(complete_pass, 0L)) == 1L,
    prev_sack           = lag(coalesce(sack, 0L)) == 1L,
    prev_timeout_called = lag(coalesce(timeout, 0L)) == 1L,

    # Clock running at the snap?
    # TRUE if previous play kept the clock running and no stoppage intervened.
    is_clock_running = !is.na(prev_play_type) &
      !prev_timeout_called &
      !prev_out_of_bounds &
      (
        (prev_play_type == "run") |
          (prev_play_type == "pass" & (prev_complete_pass | prev_sack))
      ),
    is_clock_running = if_else(is.na(is_clock_running), FALSE, is_clock_running),
    
    # Within-drive cumulative intent signals (PRE-SNAP, lagged so the current
    # play does not leak into its own features).
    n_plays_in_drive_so_far    = row_number() - 1L,
    n_passes_in_drive_so_far   = lag(cumsum(play_type == "pass"), default = 0L),
    n_runs_in_drive_so_far     = lag(cumsum(play_type == "run"),  default = 0L),
    pass_share_in_drive_so_far = if_else(
      (n_passes_in_drive_so_far + n_runs_in_drive_so_far) > 0,
      n_passes_in_drive_so_far / (n_passes_in_drive_so_far + n_runs_in_drive_so_far),
      0
    ),
    had_pass_in_drive_so_far    = n_passes_in_drive_so_far > 0,
    advance_yards_in_drive_so_far = drive_start_yardline_100 - yardline_100
  ) %>%
  ungroup()

# Survivorship-control: how many timeouts have been used in this drive so far.
h1_qualifying <- h1_qualifying %>%
  mutate(
    timeouts_used_in_drive = drive_start_posteam_timeouts - posteam_timeouts_remaining
  )


# --- Section 9: Compute the target ------------------------------------------
# total_home_score / total_away_score in nflfastR are pre-snap (state at the
# start of the play). h*_score_at_h1_end uses the first H2 play's pre-snap
# score, which equals the score at the end of H1.
h1_qualifying <- h1_qualifying %>%
  mutate(
    posteam_score_at_state  = if_else(posteam == home_team, total_home_score,    total_away_score),
    defteam_score_at_state  = if_else(posteam == home_team, total_away_score,    total_home_score),
    posteam_score_at_h1_end = if_else(posteam == home_team, home_score_at_h1_end, away_score_at_h1_end),
    defteam_score_at_h1_end = if_else(posteam == home_team, away_score_at_h1_end, home_score_at_h1_end),
    
    target_net_h1_points = (posteam_score_at_h1_end - posteam_score_at_state) -
      (defteam_score_at_h1_end - defteam_score_at_state)
  )


# --- Section 10: Soft Rule 2 (flag obvious clock-burning plays) -------------
# Layered by time remaining: as time shrinks, only deeper-in-opponent-territory
# runs survive. Kneels are always excluded.
h1_qualifying <- h1_qualifying %>%
  mutate(
    is_obvious_clock_burn = (
      play_type == "qb_kneel" |
        (play_type == "run" & game_seconds_remaining < 1850 & yardline_100 > 90) |
        (play_type == "run" & game_seconds_remaining < 1830 & yardline_100 > 80) |
        (play_type == "run" & game_seconds_remaining < 1820 & yardline_100 > 70) |
        (play_type == "run" & game_seconds_remaining < 1815 & yardline_100 > 60)
    ),
    is_obvious_clock_burn = if_else(is.na(is_obvious_clock_burn), FALSE, is_obvious_clock_burn)
  )


# --- Section 11: Build modeling-ready per-play observation set --------------
h1_observations <- h1_qualifying %>%
  filter(
    !is_obvious_clock_burn,
    play_type %in% c("run", "pass", "qb_spike"),
    !is.na(yardline_100),
    !is.na(game_seconds_remaining),
    !is.na(posteam_timeouts_remaining),
    !is.na(score_differential),
    !is.na(down),
    !is.na(ydstogo),
    !is.na(target_net_h1_points)
  )

message(glue("Modeling rows after soft Rule 2 and clean-up: {nrow(h1_observations)}"))


# --- Section 11.5: Turnover Separation, Cost Estimation, and P(Turnover) ----
# Goal: Remove drives ending in turnovers (with > 10 sec remaining) from the
# training set. Compute average turnover cost by time bucket and P(turnover)
# by play type. These feed into the Phase 2 EP adjustment at prediction time:
#   adjusted_EP = model(state) - P(turnover | play_type) * turnover_cost(time_bucket)
# NOTE: verify exact fixed_drive_result strings by running:
#   table(h1_qualifying$fixed_drive_result)
# ----------------------------------------------------------------------------

turnover_drive_keys <- h1_qualifying %>%
  filter(play_type %in% c("run", "pass")) %>%
  mutate(
    had_turnover = coalesce(interception, 0L) == 1L |
                   coalesce(fumble_lost,  0L) == 1L
  ) %>%
  filter(had_turnover, game_seconds_remaining > 1810) %>%
  distinct(game_id, fixed_drive)

message(glue("Turnover drives identified: {nrow(turnover_drive_keys)}"))

# Split h1_observations into clean (training) and turnover (cost estimation) sets
h1_turnover <- h1_observations %>%
  semi_join(turnover_drive_keys, by = c("game_id", "fixed_drive"))

h1_clean <- h1_observations %>%
  anti_join(turnover_drive_keys, by = c("game_id", "fixed_drive"))

message(glue("Clean drives for training:  {dplyr::n_distinct(paste(h1_clean$game_id, h1_clean$fixed_drive))}"))
message(glue("Turnover drives removed:    {dplyr::n_distinct(paste(h1_turnover$game_id, h1_turnover$fixed_drive))}"))

# Average turnover cost by time remaining bucket.
# target_net_h1_points on a turnover drive is negative — offense scores 0,
# opponent scores whatever they can in the remaining time.
turnover_cost_table <- h1_turnover %>%
  mutate(
    time_bucket = case_when(
      game_seconds_remaining > 1860 ~ "> 60 sec",
      game_seconds_remaining > 1830 ~ "30-60 sec",
      game_seconds_remaining > 1810 ~ "10-30 sec",
      TRUE                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(time_bucket)) %>%
  group_by(time_bucket) %>%
  summarise(
    avg_turnover_cost = mean(target_net_h1_points, na.rm = TRUE),
    n_plays           = n(),
    .groups = 'drop'
  )

message("\nTurnover cost by time bucket (negative = bad for offense):")
print(turnover_cost_table)

# P(turnover) by play type from all qualifying H1 plays (pre-split).
# Excludes obvious clock-burn plays since those aren't real scoring attempts.
p_turnover_by_play_type <- h1_qualifying %>%
  filter(
    play_type %in% c("run", "pass"),
    !is_obvious_clock_burn
  ) %>%
  mutate(
    had_turnover = coalesce(interception, 0L) == 1L |
                   coalesce(fumble_lost,  0L) == 1L
  ) %>%
  group_by(play_type) %>%
  summarise(
    p_turnover = mean(had_turnover, na.rm = TRUE),
    n_plays    = n(),
    .groups = 'drop'
  )

message("\nP(turnover) by play type:")
print(p_turnover_by_play_type)


# --- Section 11.75: Empirical Timeout Clock Savings --------------------------
# Instead of assuming how many seconds a timeout saves, measure it from data.
#
# For every pair of consecutive offensive snaps (run/pass) in qualifying H1
# drives, compute delta_t = gsr(snap_n) - gsr(snap_{n+1}). An offensive
# timeout between snaps is detected when posteam_timeouts_remaining decreases
# from snap_n to snap_{n+1} within the same drive — no timeout-row lookup
# needed, fully robust to nflfastR's representation of timeout events.
#
# empirical savings = avg delta_t (no timeout between) - avg delta_t (timeout between)
# Stratified by posteam_timeouts_remaining at snap_n for diminishing returns.
# ----------------------------------------------------------------------------

# Detect no_play rows (penalties / dead-ball events) between consecutive
# run/pass snaps so we can exclude pairs where the clock was stopped by
# something other than a timeout. Without this, penalty-induced clock stops
# pull the no-timeout baseline downward and shrink the measured savings.
noplay_between <- h1 %>%
  semi_join(qualifying_drive_keys, by = c("game_id", "fixed_drive")) %>%
  filter(play_type == "no_play") %>%
  select(game_id, fixed_drive, noplay_play_id = play_id)

snap_pairs <- h1 %>%
  semi_join(qualifying_drive_keys, by = c("game_id", "fixed_drive")) %>%
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
    # Clock still running after this play — the only situation where a timeout
    # is useful. Run in bounds, complete in-bounds pass, or in-bounds sack.
    # Sacks at the sideline (OOB) stop the clock — exclude those too.
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
# TO rows have posteam == NA so they're dropped by the `pbp` pipeline at the
# top. Pull them from `pbp_raw` instead. Each TO row records
# game_seconds_remaining at the moment of the TO call, which lets us measure
# the lag from play snap to TO call — the key to separating clock-saving
# timeouts from strategic late-call timeouts (e.g., bleeding clock before
# a planned FG attempt).
to_rows <- pbp_raw %>%
  semi_join(qualifying_drive_keys, by = c("game_id", "fixed_drive")) %>%
  filter(coalesce(timeout, 0L) == 1L, !is.na(timeout_team), !is.na(game_seconds_remaining)) %>%
  select(game_id, fixed_drive,
         to_play_id = play_id,
         to_gsr     = game_seconds_remaining,
         to_team    = timeout_team)

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
  "With def-TO: {sum(snap_pairs$had_def_timeout)} | ",
  "No-play between: {sum(snap_pairs$had_noplay_between)} | ",
  "Clock-running: {sum(snap_pairs$play_ended_clock_running)} | ",
  "TOs matched to row: {nrow(snap_pairs_with_to_lag)} | ",
  "Clock-saving TOs (lag <= {CLOCK_SAVING_LAG_MAX}s): {nrow(clock_saving_pairs)} ",
  "({round(100 * nrow(clock_saving_pairs) / nrow(snap_pairs_with_to_lag), 1)}%)"
))

# Fix #1: Use clock-running-only baseline — the correct counterfactual.
# Incomplete passes and OOB runs stop the clock; their delta_t (~4-6 sec) is
# just the play duration, not the full hurry-up interval. Mixing them into
# the baseline pulls savings estimates way down.
#
# Fix #2: Also exclude pairs contaminated by something OTHER than an offensive
# timeout stopping the clock: defensive timeouts (had_def_timeout), penalties
# between snaps (had_noplay_between), and pairs with delta_t < 10 sec (which
# is mechanically impossible in a true clock-running hurry-up — play duration
# plus get-to-line plus snap is always >= ~10 sec).
#
# Fix #3: Yard-mix selection-bias correction. Teams disproportionately call
# TOs after sacks/losses and big gains, both of which have longer baseline
# delta_t than routine 3-7 yard plays (the line of scrimmage moved further,
# so post-play setup time is longer). Without correcting for this, we
# compare the with-TO sample against a global baseline that under-represents
# the heavy-setup play outcomes the with-TO sample is over-represented in.
# Solution: bin baseline by yards_gained, then weight each bucket's baseline
# by the with-TO sample's share in that bucket. This yields a counterfactual
# baseline matched to the play-outcome distribution of TO'd plays.
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
  global_base <- mean(baseline_pairs$delta_t, na.rm = TRUE)
  combined$base_mean_dt[is.na(combined$base_mean_dt)] <- global_base
  sum(combined$weight * combined$base_mean_dt)
}

dt_no_timeout_naive <- mean(baseline_pairs$delta_t, na.rm = TRUE)
dt_no_timeout       <- matched_baseline(clock_saving_pairs)

# With-TO: only the clock-saving subset, so we measure realistic savings
# rather than averaging across late strategic TOs that are bleeding clock
# on purpose (which would mechanically deflate measured savings).
dt_with_timeout <- mean(clock_saving_pairs$delta_t, na.rm = TRUE)

overall_savings <- dt_no_timeout - dt_with_timeout
message(glue(
  "Clock-running baseline: {round(dt_no_timeout, 1)} sec | ",
  "With timeout: {round(dt_with_timeout, 1)} sec | ",
  "Savings: {round(overall_savings, 1)} sec"
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

# Fix #2: Detect last-play timeouts — timeouts called after the final offensive
# snap of a drive to enable a FG attempt or one more play. These don't appear
# in snap_pairs (no "next snap" in the same drive) and are missing entirely
# from the clock-savings model. Estimate their proportion from total drive
# timeout usage vs. the within-drive count above, then model their value in
# Phase 2 as the EP gain from running one more play from the current position.

total_tos_in_drives <- h1 %>%
  semi_join(qualifying_drive_keys, by = c("game_id", "fixed_drive")) %>%
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
  "Total timeouts in qualifying drives: {total_tos_in_drives} | ",
  "Within-drive: {n_within_drive_tos} | ",
  "Last-play: {n_last_play_tos} ({round(p_last_play * 100, 1)}%)"
))

# Average yards per pass play (including incompletions) — for "one more play" EP
avg_pass_gain_yds <- h1 %>%
  semi_join(qualifying_drive_keys, by = c("game_id", "fixed_drive")) %>%
  filter(!is.na(down), play_type == "pass", !is.na(yards_gained)) %>%
  summarise(avg = mean(yards_gained, na.rm = TRUE)) %>%
  pull(avg)

# Play duration proxy: delta_t after a clock-stopping pass = time elapsed is
# just the play itself (clock stopped immediately after), so no hurry-up time.
avg_play_duration_sec <- snap_pairs %>%
  filter(play_type == "pass", !play_ended_clock_running, !had_off_timeout) %>%
  summarise(avg = mean(delta_t, na.rm = TRUE)) %>%
  pull(avg)

message(glue(
  "Avg yards/pass: {round(avg_pass_gain_yds, 1)} | ",
  "Avg play duration: {round(avg_play_duration_sec, 1)} sec"
))

# Attach last-play parameters to empirical_dt_table — Phase 2 reads them here
empirical_dt_table <- empirical_dt_table %>%
  mutate(
    p_last_play       = p_last_play,
    avg_pass_gain_yds = avg_pass_gain_yds,
    avg_play_duration = avg_play_duration_sec
  )

message("\nEmpirical dt table:")
print(empirical_dt_table)


# --- Section 12: Select final columns ---------------------------------------
h1_final <- h1_clean %>%
  transmute(
    # --- Identifiers ---
    season, week, game_id, posteam, defteam,
    play_id, fixed_drive,
    
    # --- Pre-snap state features (model inputs) ---
    yardline_100,
    game_seconds_remaining,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining,
    down = as.integer(down),
    ydstogo,
    score_differential,
    rating_differential,
    is_clock_running,
    play_type,

    # --- Within-drive intent signals (pre-snap, lagged) ---
    n_plays_in_drive_so_far,
    pass_share_in_drive_so_far,
    had_pass_in_drive_so_far,
    advance_yards_in_drive_so_far,

    # --- Survivorship controls ---
    drive_start_posteam_timeouts,
    drive_start_yardline_100,
    drive_start_game_seconds,
    drive_start_score_differential,
    drive_start_rating_differential = rating_differential,
    timeouts_used_in_drive,
    
    # --- Target ---
    target_net_h1_points,
    
    # --- Diagnostic columns (NOT model inputs, useful for sanity-checking) ---
    posteam_score_at_state,
    defteam_score_at_state,
    posteam_score_at_h1_end,
    defteam_score_at_h1_end
  )


# --- Section 13: Sanity checks ----------------------------------------------
message("\n--- Sanity Checks ---")
message(glue("Final rows: {nrow(h1_final)}"))
message(glue("Unique games: {dplyr::n_distinct(h1_final$game_id)}"))
message(glue("Unique drives: {dplyr::n_distinct(paste(h1_final$game_id, h1_final$fixed_drive))}"))

message("\nTarget summary (net H1 points from state):")
print(summary(h1_final$target_net_h1_points))

message("\nposteam_timeouts_remaining distribution:")
print(table(h1_final$posteam_timeouts_remaining))

message("\ngame_seconds_remaining distribution (should all be in 1800-1920):")
print(summary(h1_final$game_seconds_remaining))

message("\nplay_type breakdown:")
print(table(h1_final$play_type))


# --- Section 14: Export ------------------------------------------------------
output_path <- file.path("build", "h1_timeout_dataset.csv")
write_csv(h1_final, output_path)
message(glue("\nSaved {nrow(h1_final)} rows to {output_path}"))
message("Phase 1 complete. Next: train EP regression with monotone constraints.")

# Save Phase 1 outputs so Phase 2 can run standalone without reloading nflfastR
saveRDS(h1_final,                file.path("build", "phase1_h1_final.rds"))
saveRDS(turnover_cost_table,     file.path("build", "phase1_turnover_cost_table.rds"))
saveRDS(p_turnover_by_play_type, file.path("build", "phase1_p_turnover_by_play_type.rds"))
message("Phase 1 objects saved to .rds files.")

# Save EP→WP calibration data (for Phase 3 WP conversion)
# Uses nflfastR's own ep and vegas_wp columns from the same H1 window we model,
# so the conversion slope is calibrated to this exact context.
ep_wp_calib <- h1_qualifying %>%
  filter(!is.na(ep), !is.na(vegas_wp), play_type %in% c("run", "pass")) %>%
  select(ep, vegas_wp, score_differential, game_seconds_remaining)
saveRDS(ep_wp_calib, file.path("build", "phase1_ep_wp_calib.rds"))
message(glue("EP→WP calibration data saved: {nrow(ep_wp_calib)} rows to phase1_ep_wp_calib.rds"))

saveRDS(empirical_dt_table, file.path("build", "phase1_empirical_dt.rds"))
message("Empirical timeout dt table saved to phase1_empirical_dt.rds")

# Also save clock_saving_pairs — needed by Phase 6c (call-weighted state-cond
# TO value). Contains every play in qualifying drives where a clock-saving
# offensive TO was called within CLOCK_SAVING_LAG_MAX sec.
saveRDS(clock_saving_pairs, file.path("build", "phase1_clock_saving_pairs.rds"))
message("Clock-saving pairs saved to phase1_clock_saving_pairs.rds")


# --- EPA rest premium -------------------------------------------------------
# A timeout doesn't just freeze the clock — it gives the offense rest, time to
# pick the optimal play, and visibility into defensive alignment. This block
# estimates the EPA bonus on the play AFTER a clock-saving timeout vs the
# play after a no-TO clock-running prior play, controlling for game state.
#
# Sample: BOTH 2-min drills (H1 and H2 windows) for statistical precision.
# Even with 1100+ rest observations, per-play EPA variance is large enough
# that the coefficient is not significant at p < 0.05. Point estimate is
# stable and modest (~0.05 EPA) across specifications. Phase 2 adds the
# estimate as an additive bonus to timeout value.
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

saveRDS(rest_premium, file.path("build", "phase1_rest_premium.rds"))
message("Rest premium saved to phase1_rest_premium.rds")




# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================
#
#   PHASE 2: Expected Points Regression + Timeout Value Estimation
#
# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================
#
# Goal: Train a smooth EP model on h1_final that predicts target_net_h1_points.
# Use it to estimate the incremental value of each timeout by comparing
# EP(state, N timeouts) vs EP(state, N-1 timeouts), adjusted for turnover risk.
#
# Model choice — GAM (mgcv) over XGBoost:
#   XGBoost with NFL data tends to overfit and produce jumpy predictions,
#   especially as the number of predictors grows. GAMs are smooth by design
#   (penalized splines), support monotone constraints, and produce directly
#   interpretable marginal effects — which is exactly what timeout value needs.
#
#   XGBoost + LOESS alternative: if you prefer XGBoost, train it with
#   monotone_constraints, then for each row compute
#   delta_EP = EP(N timeouts) - EP(N-1 timeouts) and fit a LOESS smoother
#   on those delta values:
#     loess(timeout_value ~ game_seconds_remaining * yardline_100, data = results)
#   This smooths the noisy XGBoost deltas into an interpretable surface.
#   GAM does this implicitly and more cleanly, so it is the default here.
# ==============================================================================


# --- Phase 2, Section 1: Additional packages ---------------------------------
pacman::p_load(mgcv)


# --- Phase 2, Section 2: Train / test split ----------------------------------
set.seed(42)
train_idx <- sample(nrow(h1_final), floor(0.8 * nrow(h1_final)))
ep_train  <- h1_final[train_idx, ]
ep_test   <- h1_final[-train_idx, ]

message(glue("EP train rows: {nrow(ep_train)} | EP test rows: {nrow(ep_test)}"))


# --- Phase 2, Section 3: GAM EP model ----------------------------------------
# KEY DESIGN DECISION: posteam_timeouts_remaining is NOT a model feature.
#
# Why: including it caused negative timeout values because observational data
# has a confound — teams that still have timeouts at a given state got there
# via a different path than teams who used them. Survivorship controls reduce
# this but don't eliminate it, and the interaction terms needed to make the
# effect vary by game state were numerically unstable.
#
# Instead, timeout VALUE is computed in Section 6 as the EP gain from having
# dt extra seconds, where dt is calibrated to the actual time a timeout saves
# (with diminishing returns for the 2nd and 3rd timeout). This is always
# positive by construction and varies naturally by game state through the
# smooth s(game_seconds_remaining) term.
#
# Survivorship controls (drive_start_posteam_timeouts, timeouts_used_in_drive)
# are KEPT in the model to remove selection bias from the training data —
# they just don't enter the timeout value calculation.

ep_model <- gam(
  target_net_h1_points ~
    # --- Main smooth effects ---
    s(yardline_100,           bs = "cr", k = 8) +
    s(game_seconds_remaining, bs = "cr", k = 8) +
    s(score_differential,     bs = "cr", k = 8) +
    s(ydstogo,                bs = "cr", k = 6) +
    # --- Tensor product interactions ---
    # Make EP(gsr+dt) - EP(gsr) vary by field position and score so timeout
    # value depends on WHERE and WHEN you are, not just time remaining.
    ti(game_seconds_remaining, yardline_100,       k = c(6, 6)) +
    ti(game_seconds_remaining, score_differential, k = c(6, 6)) +
    # --- Parametric terms ---
    defteam_timeouts_remaining      +
    drive_start_rating_differential +  # rating_differential dropped — same column
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

message("\n--- GAM Summary ---")
print(summary(ep_model))


# --- Phase 2, Section 4: Diagnostics -----------------------------------------
ep_test$pred_ep <- as.numeric(predict(ep_model, newdata = ep_test))
rmse <- sqrt(mean((ep_test$target_net_h1_points - ep_test$pred_ep)^2, na.rm = TRUE))
message(glue("Test RMSE: {round(rmse, 4)}"))

# Sanity check: more time remaining should predict more expected points when
# in scoring position. Compare 15 sec vs 60 sec left at opponent's 40, tied.
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

ep_15sec <- as.numeric(predict(ep_model, newdata = base_state))
ep_60sec <- as.numeric(predict(ep_model, newdata = more_time_state))
message(glue("EP at 15 sec left: {round(ep_15sec, 3)} | EP at 60 sec left: {round(ep_60sec, 3)}"))
message(glue("More time = more EP: {ep_60sec > ep_15sec} (must be TRUE)"))
if (ep_60sec <= ep_15sec) warning("Time curve is inverted — investigate model or training data.")


# --- Phase 2, Section 5: Turnover-adjusted EP prediction function ------------
# adjusted_EP = raw_EP - P(turnover | play_type) * avg_turnover_cost(time_bucket)
# turnover_cost_table and p_turnover_by_play_type built in Phase 1, Section 11.5.

predict_ep_adjusted <- function(state_df, play_type_val = "pass") {
  raw_ep <- as.numeric(predict(ep_model, newdata = state_df))

  p_to <- p_turnover_by_play_type %>%
    filter(play_type == play_type_val) %>%
    pull(p_turnover)
  if (length(p_to) == 0) p_to <- 0

  cost <- sapply(state_df$game_seconds_remaining, function(gsr) {
    bucket <- case_when(
      gsr > 1860 ~ "> 60 sec",
      gsr > 1830 ~ "30-60 sec",
      TRUE       ~ "10-30 sec"
    )
    val <- turnover_cost_table %>% filter(time_bucket == bucket) %>% pull(avg_turnover_cost)
    if (length(val) == 0) 0 else val
  })

  raw_ep - p_to * cost
}


# --- Phase 2, Section 6: Timeout value via time-delta approach ---------------
# A timeout buys dt seconds of clock. The EP gain from those extra seconds is
# the timeout's value. dt is the EMPIRICALLY DERIVED clock savings from
# Section 11.75: avg clock consumed per play pair with NO timeout minus avg
# clock consumed per play pair WITH an offensive timeout between snaps.
# Stratified by posteam_timeouts_remaining so diminishing returns are data-
# driven rather than assumed.
#
# Timeout value = EP(current state + dt seconds) - EP(current state)

# empirical_dt_table is already in memory from Phase 1 Section 11.75.
# rest_premium_epa is also in memory from the rest-premium block above.
# In the standalone Phase2_EP_Timeout.R both are loaded from RDS at the top.

# Safety: if Phase 2 is run without Phase 1 having executed, fall back to 0.
if (!exists("rest_premium_epa")) rest_premium_epa <- 0

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
  # Team calls timeout while clock is running → saves dt seconds vs. hurry-up.
  state_clock_save <- state_df
  state_clock_save$game_seconds_remaining <- min(state_df$game_seconds_remaining + dt, 1920)
  ep_clock_save <- predict_ep_adjusted(state_clock_save, play_type_val) -
                   predict_ep_adjusted(state_df,         play_type_val)

  # Component 2: last-play opportunity
  # Team calls timeout after their final offensive snap → enables one more play
  # from the current position (FG attempt or an extra down). EP gain modeled as
  # the expected improvement from advancing avg_gain yards in avg_dur seconds.
  state_one_play <- state_df
  state_one_play$yardline_100              <- max(state_df$yardline_100 - avg_gain, 1)
  state_one_play$game_seconds_remaining    <- max(state_df$game_seconds_remaining - avg_dur, 1800)
  state_one_play$n_plays_in_drive_so_far   <- state_df$n_plays_in_drive_so_far + 1L
  state_one_play$advance_yards_in_drive_so_far <- state_df$advance_yards_in_drive_so_far + avg_gain
  ep_last_play <- predict_ep_adjusted(state_one_play, play_type_val) -
                  predict_ep_adjusted(state_df,       play_type_val)

  # Weighted blend by empirical proportion of each timeout type,
  # plus the rest premium (extra EPA on the play immediately after a TO,
  # from rest + better play selection — see Phase 1 estimation block).
  (1 - p_last) * ep_clock_save + p_last * ep_last_play + rest_premium_epa
}


# --- Phase 2, Section 7: Example timeout value table ------------------------
# Show the value of each timeout (1st, 2nd, 3rd) across key game states.
# Each row is a unique (yardline, time, score) combination.
# Columns show what each incremental timeout is worth in expected points.

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

# Compute value for each timeout count (1st = going 0->1, 2nd = 1->2, 3rd = 2->3)
for (n in 1:3) {
  col_name <- paste0("to_", n, "_ep")
  scenarios_n <- base_scenarios
  scenarios_n$posteam_timeouts_remaining <- n
  base_scenarios[[col_name]] <- sapply(
    seq_len(nrow(scenarios_n)),
    function(i) compute_timeout_value(scenarios_n[i, ], "pass")
  )
}

message("\n--- Incremental Timeout Value by Situation (Expected Points, pass plays) ---")
print(
  base_scenarios %>%
    select(yardline_100, game_seconds_remaining, score_differential,
           to_1_ep, to_2_ep, to_3_ep) %>%
    arrange(game_seconds_remaining, score_differential, yardline_100)
)

message("\nPhase 2 complete.")




# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================
#
#   PHASE 3: Convert EP Timeout Values to Win Probability Change
#
# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================
# ==============================================================================
#
# Goal: Scale EP timeout values from Phase 2 into win probability (WP) change,
# expressed in percentage points. This is what the article needs — the claim
# that a timeout is worth X% WP in a given situation.
#
# Method: estimate the local dWP/dEP slope from nflfastR's vegas_wp and ep
# columns in the same H1 two-minute window. This avoids training a separate
# WP model. The slope is stratified by score bucket because WP is more
# sensitive to EP swings when the game is close.
#
# Validation target: Burke (2010) and Pelechrinis estimate timeouts worth
# ~1.5–3% WP in the second half. Our H1 window values should be in a similar
# range, potentially higher for drives with more time remaining.
# ==============================================================================


# --- Phase 3, Section 1: Load EP→WP calibration data -------------------------
# ep_wp_calib was saved in Phase 1 Section 14.
# Contains nflfastR's ep and vegas_wp from qualifying H1 window plays.
ep_wp_calib <- readRDS(file.path("build", "phase1_ep_wp_calib.rds"))
message(glue("EP→WP calibration rows: {nrow(ep_wp_calib)}"))


# --- Phase 3, Section 2: Estimate dWP/dEP slope by score bucket --------------
# Stratify by score differential so that the EP→WP scaling reflects that
# WP is more volatile when the game is close.
score_breaks  <- c(-Inf, -10, -4, 4, 10, Inf)
score_labels  <- c("down10+", "down4-10", "tied±4", "up4-10", "up10+")

ep_to_wp_by_score <- ep_wp_calib %>%
  mutate(score_bucket = cut(score_differential, breaks = score_breaks, labels = score_labels)) %>%
  group_by(score_bucket) %>%
  summarise(
    ep_to_wp_slope = coef(lm(vegas_wp ~ ep))["ep"],
    n              = n(),
    .groups        = "drop"
  )

# Global (unstratified) slope as a cross-check
ep_to_wp_global <- coef(lm(vegas_wp ~ ep, data = ep_wp_calib))["ep"]

message(glue("\nGlobal EP→WP slope: {round(ep_to_wp_global, 4)} ({round(ep_to_wp_global * 100, 2)}% WP per 1 EP)"))
message("EP→WP slope by score bucket:")
print(ep_to_wp_by_score)


# --- Phase 3, Section 3: Apply WP conversion to timeout value table ----------
# base_scenarios already has to_1_ep / to_2_ep / to_3_ep from Phase 2 Section 7.
# Multiply EP values by the score-stratified slope to get WP percentage points.

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

# Summary ranges for article text
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