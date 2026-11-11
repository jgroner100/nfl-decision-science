# ============================================================================
# PHASE 4: P(qualifying H1 2-min drill drive | current state)
# ----------------------------------------------------------------------------
# Phase 2 gives the value of a timeout *conditional on* being in a qualifying
# H1 2-min drive. Phase 4 estimates the probability that a team gets such a
# drive from any pre-2-min-warning state, so the article can multiply the two:
#
#   unconditional TO value = P(team gets qualifying drive | state)
#                          × representative TO value | drive happens
#
# Two GAMs are trained — one each for posteam and defteam perspective — since
# the article needs to answer both "I'm on offense now, will I get the 2-min
# drill?" and "I'm on defense now, will my team get it after I get the ball
# back?"
# ============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, nflfastR, glue, mgcv)

rds_dir <- file.path(getwd(), "build")

message("Loading nflfastR PBP data for 2017-2024 (REG only)...")
source("load_play_by_play.R")
pbp_raw <- load_play_by_play(2017:2024)

# Same H1 filter as the rest of the pipeline
pbp <- pbp_raw %>%
  filter(season_type == "REG", !is.na(play_type), !is.na(posteam),
         !is.na(game_seconds_remaining), !is.na(yardline_100),
         qtr %in% c(1, 2)) %>%
  mutate(rating_differential = ifelse(
    !is.na(spread_line),
    spread_line,
    qnorm(pmin(pmax(vegas_wp, 1e-6), 1 - 1e-6)) * 14
  )) %>%
  arrange(game_id, play_id)


# --- Section 1: Identify qualifying drive starts (same rule as Phase 1) -----
# A drive qualifies as a "useful 2-min drive" if its first offensive snap is
# in the window (1800, 1920] AND meets the field-position feasibility filter.
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
  select(game_id, drive_posteam = posteam)

# Per game: which teams got a qualifying drive (set)
teams_with_qualifying_drive <- drive_starts %>%
  distinct(game_id, drive_posteam)

message(glue("Games with at least one qualifying drive: ",
             "{n_distinct(teams_with_qualifying_drive$game_id)}"))
message(glue("Total qualifying (game, team) pairs: ",
             "{nrow(teams_with_qualifying_drive)}"))


# --- Section 2: Build per-play training dataset -----------------------------
# Each row is an H1 play BEFORE the 2-min warning (gsr > 1920). We attach two
# outcomes: did this play's posteam eventually get a qualifying drive, and
# did this play's defteam eventually get one. Either or both can be true.
plays_pre_warning <- pbp %>%
  filter(game_seconds_remaining > 1920,
         !is.na(down),
         play_type %in% c("run", "pass"),
         !is.na(score_differential),
         !is.na(ydstogo),
         !is.na(posteam_timeouts_remaining),
         !is.na(defteam_timeouts_remaining))

# Mark outcomes via join: did posteam appear in qualifying-drive list for this game?
qual_lookup <- teams_with_qualifying_drive %>%
  mutate(has_qual = TRUE)

plays_pre_warning <- plays_pre_warning %>%
  left_join(qual_lookup %>% rename(posteam = drive_posteam,
                                   posteam_got_qual_drive = has_qual),
            by = c("game_id", "posteam")) %>%
  left_join(qual_lookup %>% rename(defteam = drive_posteam,
                                   defteam_got_qual_drive = has_qual),
            by = c("game_id", "defteam")) %>%
  mutate(posteam_got_qual_drive = coalesce(posteam_got_qual_drive, FALSE),
         defteam_got_qual_drive = coalesce(defteam_got_qual_drive, FALSE))

message(glue("Training rows (pre-warning plays): {nrow(plays_pre_warning)}"))
message(glue("Posteam-got-drive rate: ",
             "{round(100 * mean(plays_pre_warning$posteam_got_qual_drive), 1)}%"))
message(glue("Defteam-got-drive rate: ",
             "{round(100 * mean(plays_pre_warning$defteam_got_qual_drive), 1)}%"))


# --- Section 3: Train GAMs --------------------------------------------------
# Logistic GAM. Smooths on the continuous state variables, parametric on the
# discrete ones. K is conservative — these features have natural shapes
# (probability rises sharply as gsr approaches 1920).
fit_prob_model <- function(outcome_var, df) {
  formula_obj <- as.formula(paste0(
    outcome_var, " ~ ",
    "s(game_seconds_remaining, bs = 'cr', k = 8) + ",
    "s(yardline_100,           bs = 'cr', k = 6) + ",
    "s(score_differential,     bs = 'cr', k = 8) + ",
    "s(ydstogo,                bs = 'cr', k = 5) + ",
    "as.factor(down) + ",
    "posteam_timeouts_remaining + defteam_timeouts_remaining + ",
    "rating_differential + ",
    "ti(game_seconds_remaining, score_differential, k = c(6, 6))"
  ))
  gam(formula_obj, data = df, family = binomial(), method = "REML")
}

message("\nFitting posteam-perspective model...")
pos_prob_model <- fit_prob_model("posteam_got_qual_drive", plays_pre_warning)
message("Fitting defteam-perspective model...")
def_prob_model <- fit_prob_model("defteam_got_qual_drive", plays_pre_warning)

cat("\n--- POSTEAM model summary ---\n")
print(summary(pos_prob_model)$p.table)
print(summary(pos_prob_model)$s.table)
cat("\n--- DEFTEAM model summary ---\n")
print(summary(def_prob_model)$p.table)
print(summary(def_prob_model)$s.table)


# --- Section 4: Calibration check -------------------------------------------
# Bin predicted probabilities into deciles and compare predicted vs actual.
# Want each decile to be close to the diagonal.
calibration_check <- function(model, outcome, df, label) {
  df$pred <- as.numeric(predict(model, type = "response", newdata = df))
  cal <- df %>%
    mutate(bin = ntile(pred, 10)) %>%
    group_by(bin) %>%
    summarise(n = n(),
              mean_pred = mean(pred),
              mean_actual = mean(.data[[outcome]]),
              .groups = "drop")
  cat(sprintf("\nCalibration: %s\n", label))
  print(cal)
}
calibration_check(pos_prob_model, "posteam_got_qual_drive", plays_pre_warning, "POSTEAM")
calibration_check(def_prob_model, "defteam_got_qual_drive", plays_pre_warning, "DEFTEAM")


# --- Section 5: Wrapper for easy querying -----------------------------------
# Given a state and a perspective, return P(that team gets a qualifying drive).
# State only needs game_seconds_remaining, yardline_100, score_differential,
# and (optionally) the other features. Defaults are 1st-and-10 with 3 TOs each.
#
# IMPORTANT: yardline_100 is always from the CURRENT OFFENSE's perspective —
# the team with the ball at the queried play. For a defteam query, that means
# yardline_100 is the field position of the OPPOSING team (the team currently
# on offense), not the queried team.
#
# Far from the 2-min warning (gsr > MODEL_CUTOFF_GSR = 2340, i.e. earlier than
# 9:00 remaining in Q2), the model's predictions are essentially flat at the
# base rate, so we short-circuit with a constant 0.55. The model only adds
# meaningful state-conditional value inside the last ~9 minutes of the half,
# where drive sequencing becomes predictable enough to matter.
MODEL_CUTOFF_GSR <- 2340   # 1800 + 9*60 = 9:00 remaining in Q2
EARLY_GAME_BASELINE_P <- 0.55

prob_2min_drive <- function(game_seconds_remaining,
                            yardline_100,
                            score_differential,
                            perspective                = c("posteam", "defteam"),
                            down                       = 1L,
                            ydstogo                    = 10,
                            posteam_timeouts_remaining = 3L,
                            defteam_timeouts_remaining = 3L,
                            rating_differential        = 0) {
  perspective <- match.arg(perspective)

  # Short-circuit: before 9:00 left in Q2, use the flat base rate. The model
  # has near-zero state sensitivity this far out and adds visual complexity
  # without changing the answer.
  if (game_seconds_remaining > MODEL_CUTOFF_GSR) {
    return(EARLY_GAME_BASELINE_P)
  }

  newdata <- data.frame(
    game_seconds_remaining     = game_seconds_remaining,
    yardline_100               = yardline_100,
    score_differential         = score_differential,
    down                       = as.integer(down),
    ydstogo                    = ydstogo,
    posteam_timeouts_remaining = as.integer(posteam_timeouts_remaining),
    defteam_timeouts_remaining = as.integer(defteam_timeouts_remaining),
    rating_differential        = rating_differential
  )
  model <- if (perspective == "posteam") pos_prob_model else def_prob_model
  as.numeric(predict(model, newdata = newdata, type = "response"))
}


# --- Section 6: Representative conditional TO value (for article framing) ---
# A single number for the "if you do get a useful drive, what's the TO worth?"
# half of the composition. Pulled from the Phase 2 base_scenarios output
# (mid-table values, 2nd TO, midfield, tied, mid-window).
#
# Update these if you re-run Phase 2 with different inputs and want to refresh
# the article's headline number.
REPRESENTATIVE_TO_EP_PER_TO <- 0.30   # avg EP value of one TO across realistic states
REPRESENTATIVE_TO_WP_PER_TO <- 1.30   # avg WP % value of one TO across realistic states


# --- Section 7: Demonstration: P(drive) sweep through H1 -------------------
# Shows how P(drive) shifts from early-game (near base rate) to near-warning
# (much more state-dependent — current possessor's situation matters a lot).
gsr_sweep <- tibble(
  game_seconds_remaining = c(3500, 3000, 2700, 2400, 2100, 1950),
  description = c("Q1 ~15:00 left", "Q1 ~5:00 left", "Q2 starts",
                  "Q2 ~10:00 left", "Q2 ~5:00 left", "Q2 ~0:30 to warning")
) %>%
  mutate(
    posteam_tied_mid = mapply(prob_2min_drive, game_seconds_remaining,
                              yardline_100 = 50, score_differential = 0,
                              perspective = "posteam"),
    defteam_tied_mid = mapply(prob_2min_drive, game_seconds_remaining,
                              yardline_100 = 50, score_differential = 0,
                              perspective = "defteam")
  ) %>%
  mutate(across(c(posteam_tied_mid, defteam_tied_mid), ~ round(.x, 3)))

cat("\n--- P(qualifying drive) as H1 progresses (tied, midfield) ---\n")
print(gsr_sweep)

# Article's example: "team A on defense in Q2 with 10 minutes left"
example_state <- list(
  gsr = 2400,        # 10:00 left in Q2 (gsr = 30min - 10min remaining + 30min H2 = 40min = 2400s left)
  yardline_100 = 50,
  score_differential = 0
)

p_def <- prob_2min_drive(example_state$gsr, example_state$yardline_100,
                         example_state$score_differential, perspective = "defteam")
p_pos <- prob_2min_drive(example_state$gsr, example_state$yardline_100,
                         example_state$score_differential, perspective = "posteam")
cat(sprintf("\n--- Article example: Q2, ~10:00 left, tied, midfield ---\n"))
cat(sprintf("  P(posteam gets qualifying drive): %.1f%%\n", 100 * p_pos))
cat(sprintf("  P(defteam gets qualifying drive): %.1f%%\n", 100 * p_def))
cat(sprintf("  Burning 1 TO right now (defteam perspective):\n"))
cat(sprintf("    Unconditional EP cost: %.3f  (= %.1f%% × %.2f EP)\n",
            p_def * REPRESENTATIVE_TO_EP_PER_TO, 100 * p_def, REPRESENTATIVE_TO_EP_PER_TO))
cat(sprintf("    Unconditional WP cost: %.2f%%  (= %.1f%% × %.2f%% WP)\n",
            p_def * REPRESENTATIVE_TO_WP_PER_TO, 100 * p_def, REPRESENTATIVE_TO_WP_PER_TO))

# Broader sample table for general use
sample_states <- expand.grid(
  game_seconds_remaining = c(3000, 2400, 1950),
  yardline_100           = c(75, 50, 25),
  score_differential     = c(-14, 0, 14),
  perspective            = c("posteam", "defteam"),
  stringsAsFactors       = FALSE
)

sample_states$P_drive <- mapply(prob_2min_drive,
  game_seconds_remaining = sample_states$game_seconds_remaining,
  yardline_100           = sample_states$yardline_100,
  score_differential     = sample_states$score_differential,
  perspective            = sample_states$perspective)

sample_states <- sample_states %>%
  mutate(unconditional_wp_pct_per_to = round(P_drive * REPRESENTATIVE_TO_WP_PER_TO, 2),
         P_drive = round(P_drive, 3))

cat("\n--- Sample table: P(drive) across states (1 TO unconditional WP value) ---\n")
print(sample_states %>%
        arrange(perspective, desc(game_seconds_remaining),
                score_differential, yardline_100))


# --- Section 8: Save models -------------------------------------------------
saveRDS(pos_prob_model, file.path(rds_dir, "phase4_pos_prob_model.rds"))
saveRDS(def_prob_model, file.path(rds_dir, "phase4_def_prob_model.rds"))

phase4_meta <- list(
  representative_to_ep_per_to     = REPRESENTATIVE_TO_EP_PER_TO,
  representative_to_wp_pct_per_to = REPRESENTATIVE_TO_WP_PER_TO,
  n_training_plays                = nrow(plays_pre_warning),
  posteam_base_rate               = mean(plays_pre_warning$posteam_got_qual_drive),
  defteam_base_rate               = mean(plays_pre_warning$defteam_got_qual_drive),
  note = paste(
    "GAMs predict P(team eventually starts a qualifying H1 2-min drill drive)",
    "given pre-2-min-warning state. Two perspectives (posteam, defteam) because",
    "the article queries both. Multiply by representative TO value to get",
    "unconditional TO value at the queried state. See prob_2min_drive() wrapper."
  )
)
saveRDS(phase4_meta, file.path(rds_dir, "phase4_meta.rds"))

message(glue("\nSaved phase4_pos_prob_model.rds and phase4_def_prob_model.rds to {rds_dir}"))
message("Phase 4 complete.")
