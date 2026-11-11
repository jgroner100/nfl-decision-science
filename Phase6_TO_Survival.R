# ============================================================================
# PHASE 6: P(kth TO held NOW survives to the 2-min drill | state, current N)
# ----------------------------------------------------------------------------
# Teams burn timeouts before the 2-min warning (challenges, defensive
# substitutions, etc). The kth TO you currently hold only has marginal value
# if it actually survives to the qualifying drive. This phase models that
# survival probability.
#
# Combined with Phases 2, 4, and 5, the full marginal value of the kth TO
# held NOW at any pre-warning state is:
#
#   P(qualifying drive | state)           — Phase 4 (or flat 0.55 before 9:00 Q2)
# × P(kth TO survives to drill | state)   — Phase 6, this script
# × P(use kth TO | drive, starts with k)  — Phase 5
# × conditional value per TO use          — Phase 6c call-weighted, indexed by
#                                            tos_at_call = current_to - kth + 1
#                                            (falls back to flat 0.30/1.30 if
#                                            phase6c_call_weighted_value.rds
#                                            is missing)
#
# Empirical baseline: teams starting Q1 with 3 TOs average ~1.87 TOs at the
# drill — so >1 TO burned before the warning. Probability of all 3 surviving
# is only ~30%.
# ============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, nflfastR, glue, mgcv)

rds_dir <- file.path(getwd(), "build")

message("Loading nflfastR PBP data for 2017-2024 (REG only)...")
source("load_play_by_play.R")
pbp_raw <- load_play_by_play(2017:2024)

pbp <- pbp_raw %>%
  filter(season_type == "REG", !is.na(play_type), !is.na(posteam),
         !is.na(game_seconds_remaining), !is.na(yardline_100),
         qtr %in% c(1, 2)) %>%
  arrange(game_id, play_id)


# --- Section 1: Qualifying drives -------------------------------------------
qualifying_drives <- pbp %>%
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
  select(game_id, posteam,
         qual_drive_to    = posteam_timeouts_remaining,
         qual_drive_gsr   = game_seconds_remaining)


# --- Section 2: Link pre-warning plays to their team's eventual qual drive --
pre_warning_plays <- pbp %>%
  filter(game_seconds_remaining > 1920,
         !is.na(down),
         play_type %in% c("run", "pass"),
         !is.na(posteam_timeouts_remaining),
         !is.na(score_differential))

# Many-to-many join: a play could be linked to its team's qual drive (or not).
# We require the qual drive to occur AFTER this play (lower gsr).
linked <- pre_warning_plays %>%
  inner_join(qualifying_drives, by = c("game_id", "posteam"),
             relationship = "many-to-many") %>%
  filter(qual_drive_gsr < game_seconds_remaining) %>%
  group_by(game_id, posteam, play_id) %>%
  slice_max(qual_drive_gsr, n = 1, with_ties = FALSE) %>%
  ungroup()

message(glue("Training rows (pre-warning plays w/ eventual qual drive): {nrow(linked)}"))


# --- Section 3: Build per-k survival outcomes -------------------------------
# For each k in 1..3, a play with current_to ≥ k contributes one outcome:
# did this team have ≥k TOs at their qualifying drive start?
linked <- linked %>%
  mutate(
    has_geq1_at_drill = as.integer(qual_drive_to >= 1),
    has_geq2_at_drill = as.integer(qual_drive_to >= 2),
    has_geq3_at_drill = as.integer(qual_drive_to >= 3)
  )

# Fit logistic GAMs. Smooth on gsr (the big predictor) and score_diff;
# parametric on current TO count when it varies.
fit_survival <- function(outcome, df, include_to_factor = TRUE) {
  formula_obj <- if (include_to_factor) {
    as.formula(paste0(
      outcome,
      " ~ s(game_seconds_remaining, bs='cr', k=8) + ",
      "s(score_differential, bs='cr', k=6) + ",
      "as.factor(posteam_timeouts_remaining)"
    ))
  } else {
    as.formula(paste0(
      outcome,
      " ~ s(game_seconds_remaining, bs='cr', k=8) + ",
      "s(score_differential, bs='cr', k=6)"
    ))
  }
  gam(formula_obj, data = df, family = binomial(), method = "REML")
}

# For k=1: trained on plays where current_to ≥ 1 (3 levels of factor)
m_geq1 <- fit_survival("has_geq1_at_drill",
                       linked %>% filter(posteam_timeouts_remaining >= 1))
# For k=2: current_to ≥ 2 (2 levels of factor)
m_geq2 <- fit_survival("has_geq2_at_drill",
                       linked %>% filter(posteam_timeouts_remaining >= 2))
# For k=3: current_to == 3 only — factor has 1 level, drop it
m_geq3 <- fit_survival("has_geq3_at_drill",
                       linked %>% filter(posteam_timeouts_remaining == 3),
                       include_to_factor = FALSE)

cat("\n--- P(have ≥1 TO at drill) model summary ---\n")
print(summary(m_geq1)$p.table)
cat("\n--- P(have ≥2 TOs at drill) model summary ---\n")
print(summary(m_geq2)$p.table)
cat("\n--- P(have ≥3 TOs at drill) model summary ---\n")
print(summary(m_geq3)$p.table)


# --- Section 4: Wrapper for easy querying -----------------------------------
# Returns P(kth TO survives to drill | currently has at least k TOs and state).
# k must be 1, 2, or 3. current_to is the team's CURRENT TO count.
p_to_survives <- function(kth_to,
                          current_to,
                          game_seconds_remaining,
                          score_differential = 0) {
  if (kth_to > current_to) return(0)             # can't survive what you don't have
  if (kth_to < 1)          return(NA_real_)

  newdata <- data.frame(
    game_seconds_remaining     = game_seconds_remaining,
    score_differential         = score_differential,
    posteam_timeouts_remaining = as.integer(current_to)
  )

  model <- switch(as.character(kth_to),
                  "1" = m_geq1, "2" = m_geq2, "3" = m_geq3)
  as.numeric(predict(model, newdata = newdata, type = "response"))
}


# --- Section 5: Validation table — does it match the empirical patterns? ---
cat("\n--- Survival predictions (current_to=3, tied score) ---\n")
cat("Should match the empirical sweep we saw:\n")
sweep_check <- tibble(
  game_seconds_remaining = c(3500, 3000, 2700, 2400, 2100, 1950)
) %>% mutate(
  description = c("Q1 ~15:00", "Q1 ~5:00", "Q2 start",
                  "Q2 ~10:00", "Q2 ~5:00", "Q2 ~0:30"),
  p_geq1 = mapply(p_to_survives, kth_to = 1, current_to = 3,
                  game_seconds_remaining = game_seconds_remaining),
  p_geq2 = mapply(p_to_survives, kth_to = 2, current_to = 3,
                  game_seconds_remaining = game_seconds_remaining),
  p_geq3 = mapply(p_to_survives, kth_to = 3, current_to = 3,
                  game_seconds_remaining = game_seconds_remaining),
  expected_tos_at_drill = round(p_geq1 + p_geq2 + p_geq3, 2),
  across(c(p_geq1, p_geq2, p_geq3), ~ round(.x, 3))
)
print(sweep_check)


# --- Section 6: Full marginal-value composition for the article -------------
# This is the headline number the article needs: for any pre-warning state,
# what is the marginal WP cost of burning the kth TO held right now?
P_DRIVE_BEFORE_9MIN   <- 0.55  # Phase 4 flat baseline pre-9:00 Q2
MODEL_CUTOFF_GSR      <- 2340  # 1800 + 9*60 — same as Phase 4

# Fallback constants if Phase 6c output is unavailable
COND_VALUE_PER_TO_EP_FLAT <- 0.30
COND_VALUE_PER_TO_WP_FLAT <- 1.30

# Phase 6c call-weighted conditional values, keyed by tos_at_call (= TOs
# remaining at the moment the TO is called in the drill).
#
# Mapping for a team CURRENTLY holding `current_to` TOs and considering the
# `kth_to` they hold:  tos_at_call = current_to - kth_to + 1
#   e.g. current_to=3:  kth=1 -> tos_at_call=3 (the 1st-of-drive call)
#                       kth=2 -> tos_at_call=2 (the 2nd-of-drive call)
#                       kth=3 -> tos_at_call=1 (the 3rd-of-drive call)
#
# Phase6c_Diagnostic.R checked whether further conditioning on drive_start_to
# (cross-tab cells) was warranted: cell-level deviations from the 1D marginal
# were |z| < 2.4 and mostly tracked yardline mix at the call moment, not a
# drive_start_to effect per se. The 1D mapping is retained.
phase6c_file <- file.path(rds_dir, "phase6c_call_weighted_value.rds")
if (file.exists(phase6c_file)) {
  phase6c_value <- readRDS(phase6c_file)
  # phase6c_value$by_starting_to is misnamed: it's by posteam_timeouts_remaining
  # at the call moment (i.e., tos_at_call). Re-key it.
  by_tos_at_call <- phase6c_value$by_starting_to %>%
    arrange(posteam_timeouts_remaining)
  COND_VALUE_BY_TOS_AT_CALL_EP <- setNames(
    by_tos_at_call$mean_ep,
    as.character(by_tos_at_call$posteam_timeouts_remaining)
  )
  COND_VALUE_BY_TOS_AT_CALL_WP <- setNames(
    by_tos_at_call$mean_wp,
    as.character(by_tos_at_call$posteam_timeouts_remaining)
  )
  COND_VALUE_SOURCE <- "phase6c_call_weighted"
  message(glue("Loaded Phase 6c cond-value table: ",
               "EP=[{paste(sprintf('%.3f', COND_VALUE_BY_TOS_AT_CALL_EP), collapse=', ')}], ",
               "WP%=[{paste(sprintf('%.2f', COND_VALUE_BY_TOS_AT_CALL_WP), collapse=', ')}]"))
} else {
  COND_VALUE_BY_TOS_AT_CALL_EP <- c("1" = COND_VALUE_PER_TO_EP_FLAT,
                                    "2" = COND_VALUE_PER_TO_EP_FLAT,
                                    "3" = COND_VALUE_PER_TO_EP_FLAT)
  COND_VALUE_BY_TOS_AT_CALL_WP <- c("1" = COND_VALUE_PER_TO_WP_FLAT,
                                    "2" = COND_VALUE_PER_TO_WP_FLAT,
                                    "3" = COND_VALUE_PER_TO_WP_FLAT)
  COND_VALUE_SOURCE <- "flat_fallback"
  warning("phase6c_call_weighted_value.rds not found — falling back to flat ",
          COND_VALUE_PER_TO_EP_FLAT, " EP / ",
          COND_VALUE_PER_TO_WP_FLAT, "% WP for cond_value.")
}

# Phase 5 marginal usage rates (P(use kth | started drill with k))
P_USE_KTH <- c("1" = 0.500, "2" = 0.270, "3" = 0.181)

# Load Phase 4 models so the gsr <= cutoff branch can compute state-aware P(drive)
pos_prob_model <- tryCatch(
  readRDS(file.path(rds_dir, "phase4_pos_prob_model.rds")),
  error = function(e) NULL
)
def_prob_model <- tryCatch(
  readRDS(file.path(rds_dir, "phase4_def_prob_model.rds")),
  error = function(e) NULL
)

p_qualifying_drive <- function(game_seconds_remaining,
                               yardline_100              = 50,
                               score_differential        = 0,
                               perspective               = c("posteam", "defteam"),
                               down                      = 1L,
                               ydstogo                   = 10,
                               posteam_timeouts_remaining = 3L,
                               defteam_timeouts_remaining = 3L,
                               rating_differential       = 0) {
  perspective <- match.arg(perspective)
  if (game_seconds_remaining > MODEL_CUTOFF_GSR) return(P_DRIVE_BEFORE_9MIN)
  model <- if (perspective == "posteam") pos_prob_model else def_prob_model
  if (is.null(model)) return(P_DRIVE_BEFORE_9MIN)
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
  as.numeric(predict(model, newdata = newdata, type = "response"))
}

marginal_to_value <- function(kth_to, current_to,
                              game_seconds_remaining,
                              score_differential = 0,
                              perspective        = "posteam",
                              yardline_100       = 50) {
  p_drive <- p_qualifying_drive(
    game_seconds_remaining,
    yardline_100       = yardline_100,
    score_differential = score_differential,
    perspective        = perspective,
    posteam_timeouts_remaining = as.integer(current_to)
  )
  p_surv <- p_to_survives(kth_to, current_to,
                          game_seconds_remaining, score_differential)
  p_use  <- as.numeric(P_USE_KTH[as.character(kth_to)])

  # Look up state-conditional cond_value from Phase 6c table.
  # kth held -> tos_at_call = current_to - kth_to + 1 at the moment of (potential) use.
  tos_at_call <- as.integer(current_to) - as.integer(kth_to) + 1L
  cond_ep <- as.numeric(COND_VALUE_BY_TOS_AT_CALL_EP[as.character(tos_at_call)])
  cond_wp <- as.numeric(COND_VALUE_BY_TOS_AT_CALL_WP[as.character(tos_at_call)])

  list(
    p_drive          = p_drive,
    p_survives       = p_surv,
    p_use_if_starts  = p_use,
    tos_at_call      = tos_at_call,
    cond_value_ep    = cond_ep,
    cond_value_wp    = cond_wp,
    marginal_ep      = p_drive * p_surv * p_use * cond_ep,
    marginal_wp      = p_drive * p_surv * p_use * cond_wp
  )
}


# --- Section 7: Worked examples for the article ----------------------------
cat("\n=========================================================\n")
cat("WORKED EXAMPLES: marginal WP cost of burning the Nth TO\n")
cat("(Where 'Nth' = the next TO you'd burn given current count)\n")
cat("=========================================================\n")

scenarios <- list(
  list(label = "Q1 start, 3 TOs",                gsr = 3500, current_to = 3),
  list(label = "End of Q1, 3 TOs",               gsr = 2700, current_to = 3),
  list(label = "Q2 ~10:00, 3 TOs",               gsr = 2400, current_to = 3),
  list(label = "Q2 ~8:00, 3 TOs",                gsr = 2280, current_to = 3),
  list(label = "Q2 ~5:00, 3 TOs",                gsr = 2100, current_to = 3),
  list(label = "Q1 start, 2 TOs (already burned 1)", gsr = 3500, current_to = 2),
  list(label = "Q2 ~5:00, 2 TOs (already burned 1)", gsr = 2100, current_to = 2),
  list(label = "Q2 ~5:00, 1 TO (already burned 2)",  gsr = 2100, current_to = 1)
)

for (s in scenarios) {
  v <- marginal_to_value(s$current_to, s$current_to,
                         game_seconds_remaining = s$gsr)
  cat(sprintf("\n%s:\n", s$label))
  cat(sprintf("  Burning the %s TO held now (tos_at_call=%d):\n",
              c("1st", "2nd", "3rd")[s$current_to], v$tos_at_call))
  cat(sprintf("    P(qual) × P(surv) × P(used) × cond_val = %.2f × %.3f × %.3f × %.2f%%\n",
              v$p_drive, v$p_survives, v$p_use_if_starts, v$cond_value_wp))
  cat(sprintf("    Marginal EP cost: %.4f  |  Marginal WP cost: %.3f%%\n",
              v$marginal_ep, v$marginal_wp))
}


# --- Section 8: Full TO-by-TO breakdown at a single state ------------------
cat("\n=========================================================\n")
cat("Full TO-by-TO breakdown: Q2 ~8:00, currently 3 TOs\n")
cat("=========================================================\n")

state_gsr <- 2280  # Q2 ~8:00 left = 8*60 + 1800 = 2280
for (kth in 1:3) {
  v <- marginal_to_value(kth, current_to = 3,
                         game_seconds_remaining = state_gsr)
  cat(sprintf("  TO #%d held now (tos_at_call=%d): P(qual=%.2f) × P(surv=%.3f) × P(use=%.3f) × cond=%.2f%% → %.4f EP / %.3f%% WP\n",
              kth, v$tos_at_call, v$p_drive, v$p_survives, v$p_use_if_starts,
              v$cond_value_wp, v$marginal_ep, v$marginal_wp))
}
total_ep <- sum(sapply(1:3, function(k) marginal_to_value(k, 3, state_gsr)$marginal_ep))
total_wp <- sum(sapply(1:3, function(k) marginal_to_value(k, 3, state_gsr)$marginal_wp))
cat(sprintf("\n  Total expected TO value held = %.3f EP / %.2f%% WP\n", total_ep, total_wp))


# --- Section 9: Save models -------------------------------------------------
saveRDS(m_geq1, file.path(rds_dir, "phase6_surv_geq1.rds"))
saveRDS(m_geq2, file.path(rds_dir, "phase6_surv_geq2.rds"))
saveRDS(m_geq3, file.path(rds_dir, "phase6_surv_geq3.rds"))

phase6_meta <- list(
  cond_value_source              = COND_VALUE_SOURCE,
  cond_value_by_tos_at_call_ep   = COND_VALUE_BY_TOS_AT_CALL_EP,
  cond_value_by_tos_at_call_wp   = COND_VALUE_BY_TOS_AT_CALL_WP,
  p_drive_before_9min            = P_DRIVE_BEFORE_9MIN,
  p_use_kth                      = P_USE_KTH,
  n_training_plays               = nrow(linked),
  note = paste(
    "P(kth TO held now survives to qualifying drive start | state, current N).",
    "Three logistic GAMs (one per k). Combined with Phase 4 P(drive),",
    "Phase 5 P(use), and Phase 6c call-weighted conditional value (indexed by",
    "tos_at_call = current_to - kth + 1), gives the full marginal value of",
    "each TO held now. See marginal_to_value() and the worked-example block.",
    "Phase6c_Diagnostic.R verified that adding drive_start_to as a second",
    "conditioning dimension is not warranted: cell deviations have |z| < 2.4",
    "and largely track yardline-mix shifts rather than drive_start_to itself."
  )
)
saveRDS(phase6_meta, file.path(rds_dir, "phase6_meta.rds"))

message(glue("\nSaved phase6_surv_geqK.rds (K=1,2,3) and phase6_meta.rds to {rds_dir}"))
message("Phase 6 complete.")
