# ============================================================================
# PHASE 5: P(team actually uses the kth TO | they have it entering the drive)
# ----------------------------------------------------------------------------
# A timeout you don't use is worth nothing. Phase 2 computes the conditional
# value of a timeout *given that it's called*; Phase 4 gives P(qualifying
# drive | state); this phase fills the middle term:
#
#   P(team actually uses kth TO | starts qualifying drive with k+ TOs)
#
# Composition for the article:
#   unconditional_value_of_kth_TO
#     = P(get drive)                         (Phase 4)
#     × P(use kth TO | drive, has k+ to start) (Phase 5, this script)
#     × conditional value per use            (Phase 2)
#
# Empirically the 3rd TO of a "3-TO start" drive is used only ~18% of the
# time — large discount when computing its marginal value.
# ============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, nflfastR, glue)

rds_dir <- file.path(getwd(), "build")

message("Loading nflfastR PBP data for 2017-2024 (REG only)...")
source("load_play_by_play.R")
pbp_raw <- load_play_by_play(2017:2024)

pbp <- pbp_raw %>%
  filter(season_type == "REG", !is.na(play_type), !is.na(posteam),
         !is.na(game_seconds_remaining), !is.na(yardline_100),
         qtr %in% c(1, 2)) %>%
  arrange(game_id, play_id)


# --- Section 1: Qualifying drives (same definition as Phase 1) --------------
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
         drive_start_gsr        = game_seconds_remaining,
         drive_start_yardline   = yardline_100,
         drive_start_score_diff = score_differential,
         drive_start_to         = posteam_timeouts_remaining)

message(glue("Qualifying drives: {nrow(drive_starts)}"))


# --- Section 2: Per-drive TO usage ------------------------------------------
# `to_used` = drive_start_to − min(posteam_timeouts_remaining over the drive).
# Using min handles the rare case of TOs reset between halves (shouldn't
# happen within an H1 drive, but defensive).
drive_to_usage <- pbp %>%
  semi_join(drive_starts, by = c("game_id", "fixed_drive")) %>%
  filter(!is.na(posteam_timeouts_remaining)) %>%
  group_by(game_id, fixed_drive) %>%
  summarise(to_end = min(posteam_timeouts_remaining), .groups = "drop") %>%
  inner_join(drive_starts, by = c("game_id", "fixed_drive")) %>%
  mutate(to_used = pmax(drive_start_to - to_end, 0L))


# --- Section 3: Usage table by starting TO count ----------------------------
to_usage_table <- drive_to_usage %>%
  filter(drive_start_to %in% 0:3) %>%
  group_by(drive_start_to) %>%
  summarise(
    n_drives    = n(),
    mean_used   = mean(to_used),
    p_use_1plus = mean(to_used >= 1),
    p_use_2plus = mean(to_used >= 2),
    p_use_3plus = mean(to_used >= 3),
    .groups     = "drop"
  )

cat("\n--- P(uses at least k TOs | starts qualifying drive with N TOs) ---\n")
print(to_usage_table)


# --- Section 4: Marginal usage probabilities --------------------------------
# For the burning decision: if you currently have N TOs and consider burning
# one, the cost is the value of the marginal Nth TO. That marginal value =
# P(use the Nth TO | had N to start) × conditional value per use.
#
# P(use the Nth TO | had N to start) = P(use >= N | starts with N)
marginal_usage <- to_usage_table %>%
  mutate(
    p_use_marginal = case_when(
      drive_start_to == 1 ~ p_use_1plus,
      drive_start_to == 2 ~ p_use_2plus,
      drive_start_to == 3 ~ p_use_3plus,
      TRUE                ~ NA_real_
    )
  ) %>%
  select(starting_tos = drive_start_to, n_drives,
         p_use_marginal_kth = p_use_marginal,
         mean_used)

cat("\n--- Marginal usage: P(use Nth TO | started drive with N) ---\n")
print(marginal_usage %>% filter(starting_tos > 0))


# --- Section 5: State sensitivity check (sanity) ----------------------------
# Yardline matters more than score; gsr at drive start matters a moderate
# amount. We're keeping the main table marginal (state-agnostic) for the
# article — these are diagnostic only.
cat("\n--- Usage by drive-start GSR (starts with 3 TOs) ---\n")
drive_to_usage %>% filter(drive_start_to == 3) %>%
  mutate(gsr_bin = cut(drive_start_gsr, c(1800, 1830, 1860, 1890, 1920))) %>%
  group_by(gsr_bin) %>%
  summarise(n = n(), mean_used = round(mean(to_used), 2),
            p_use_3plus = round(mean(to_used >= 3), 3), .groups = "drop") %>%
  print()

cat("\n--- Usage by drive-start yardline (starts with 3 TOs) ---\n")
drive_to_usage %>% filter(drive_start_to == 3) %>%
  mutate(yd_bin = cut(drive_start_yardline, c(0, 30, 50, 70, 100))) %>%
  group_by(yd_bin) %>%
  summarise(n = n(), mean_used = round(mean(to_used), 2),
            p_use_3plus = round(mean(to_used >= 3), 3), .groups = "drop") %>%
  print()


# --- Section 6: Wrapper for composition -------------------------------------
# Given the kth TO you currently hold and your starting-drive count, return
# P(actually uses kth TO).
p_to_used <- function(kth_to, starting_tos = kth_to) {
  if (kth_to > starting_tos) return(0)  # can't use more TOs than you started with
  if (kth_to < 1 || starting_tos < 1)   return(0)
  row <- to_usage_table %>% filter(drive_start_to == starting_tos)
  if (nrow(row) == 0) return(NA_real_)
  switch(as.character(kth_to),
         "1" = row$p_use_1plus,
         "2" = row$p_use_2plus,
         "3" = row$p_use_3plus)
}


# --- Section 7: Worked example for the article ------------------------------
# A team currently has 3 TOs going into a Q2 sequence. They're considering
# burning one for an information-gathering lineup on 4th down. What's the
# opportunity cost of that 3rd TO?
P_DRIVE_55             <- 0.55   # Phase 4 baseline before 9:00 in Q2
COND_VALUE_PER_TO_EP   <- 0.30   # Phase 2 representative conditional EP value
COND_VALUE_PER_TO_WP   <- 1.30   # Phase 2 representative conditional WP %

p_use_3rd <- p_to_used(kth_to = 3, starting_tos = 3)
opportunity_cost_ep <- P_DRIVE_55 * p_use_3rd * COND_VALUE_PER_TO_EP
opportunity_cost_wp <- P_DRIVE_55 * p_use_3rd * COND_VALUE_PER_TO_WP

cat("\n--- WORKED EXAMPLE: Burning the 3rd TO before 9:00 in Q2 ---\n")
cat(sprintf("  P(qualifying drive | state):       %.2f\n", P_DRIVE_55))
cat(sprintf("  P(uses 3rd TO | drive, 3 to start): %.3f\n", p_use_3rd))
cat(sprintf("  Conditional value per TO:           %.2f EP / %.2f%% WP\n",
            COND_VALUE_PER_TO_EP, COND_VALUE_PER_TO_WP))
cat(sprintf("  ─────────────────────────────────────\n"))
cat(sprintf("  Opportunity cost:                   %.3f EP / %.3f%% WP\n",
            opportunity_cost_ep, opportunity_cost_wp))
cat("\n  Interpretation: burning a TO 'for free' (e.g. to gather info on a\n")
cat("  4th-down lineup) costs the offense roughly this much in expectation.\n")
cat("  The delay-of-game penalty (5 yards) must be compared against this cost.\n")

# Same calc for the 1st and 2nd TOs (for context)
cat("\n--- Marginal cost of each TO held (assuming 3 TOs entering the drill) ---\n")
for (k in 3:1) {
  p_use <- p_to_used(k, 3)
  cat(sprintf("  TO #%d:  P(used)=%.3f  →  marginal value = %.3f EP / %.3f%% WP\n",
              k, p_use,
              P_DRIVE_55 * p_use * COND_VALUE_PER_TO_EP,
              P_DRIVE_55 * p_use * COND_VALUE_PER_TO_WP))
}


# --- Section 8: Save outputs ------------------------------------------------
phase5_output <- list(
  usage_table = to_usage_table,
  marginal    = marginal_usage,
  note = paste(
    "P(team uses at least k TOs in a qualifying H1 2-min drive given they",
    "started the drive with N TOs). Use marginal column for the burning",
    "decision: P(use the Nth TO | started with N). Multiply by Phase 4",
    "P(qualifying drive) and Phase 2 conditional value per TO to get the",
    "unconditional opportunity cost of burning that TO earlier in the half."
  )
)

saveRDS(phase5_output, file.path(rds_dir, "phase5_to_usage.rds"))
message(glue("\nSaved phase5_to_usage.rds to {rds_dir}"))
message("Phase 5 complete.")
