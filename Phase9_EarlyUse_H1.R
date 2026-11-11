# ============================================================================
# PHASE 9 (PRODUCTION): H1 early-burn fix — credit early-burned timeouts at
#   their full drill value instead of zeroing them. Mechanism = DROP p_survives.
#
#     old: value(kth) = p_drive * p_surv(kth) * p_use(kth) * cond(tos_at_call)
#     new: value(kth) = p_drive *               p_use(kth) * cond(tos_at_call)
#
#   Each timeout's value = its current value / its own survival prob: a big
#   add-back for a low-survival timeout (the surplus you'd burn first), almost
#   none for a high-survival one (the reserve). Scarcity is preserved by p_use
#   and cond; survival was only widening the reserve-vs-surplus spread.
#
#   Loads SAVED Phase 4/6/6c models (no load_pbp, no refit). FROZEN Phase 6
#   outputs (phase6_surv_geq*.rds, phase6_meta.rds) are NOT touched.
#   Saves: phase9_h1_burncost.rds (lookup grid) + phase9_h1_meta.rds.
# ============================================================================
suppressPackageStartupMessages({ library(mgcv); library(tidyverse) })
rds_dir <- file.path(getwd(), "build")

# ---- load saved models ------------------------------------------------------
m_geq1 <- readRDS(file.path(rds_dir, "phase6_surv_geq1.rds"))
m_geq2 <- readRDS(file.path(rds_dir, "phase6_surv_geq2.rds"))
m_geq3 <- readRDS(file.path(rds_dir, "phase6_surv_geq3.rds"))
pos_prob_model <- readRDS(file.path(rds_dir, "phase4_pos_prob_model.rds"))

# ---- constants + cond values (replicates Phase6_TO_Survival.R) --------------
P_DRIVE_BEFORE_9MIN <- 0.55
MODEL_CUTOFF_GSR    <- 2340
P_USE_KTH <- c("1" = 0.500, "2" = 0.270, "3" = 0.181)

phase6c_value <- readRDS(file.path(rds_dir, "phase6c_call_weighted_value.rds"))
by_toc <- phase6c_value$by_starting_to %>% arrange(posteam_timeouts_remaining)
COND_WP <- setNames(by_toc$mean_wp, as.character(by_toc$posteam_timeouts_remaining))
COND_EP <- setNames(by_toc$mean_ep, as.character(by_toc$posteam_timeouts_remaining))

# ---- query helpers (replicate Phase 6) --------------------------------------
p_to_survives <- function(kth_to, current_to, gsr, score_diff = 0) {
  if (kth_to > current_to) return(0)
  nd <- data.frame(game_seconds_remaining = gsr, score_differential = score_diff,
                   posteam_timeouts_remaining = as.integer(current_to))
  model <- switch(as.character(kth_to), "1" = m_geq1, "2" = m_geq2, "3" = m_geq3)
  as.numeric(predict(model, newdata = nd, type = "response"))
}
p_qualifying_drive <- function(gsr, yl = 50, score_diff = 0, current_to = 3L) {
  if (gsr > MODEL_CUTOFF_GSR) return(P_DRIVE_BEFORE_9MIN)
  nd <- data.frame(game_seconds_remaining = gsr, yardline_100 = yl,
                   score_differential = score_diff, down = 1L, ydstogo = 10,
                   posteam_timeouts_remaining = as.integer(current_to),
                   defteam_timeouts_remaining = 3L, rating_differential = 0)
  as.numeric(predict(pos_prob_model, newdata = nd, type = "response"))
}

# ---- the fix: marginal value with survival DROPPED --------------------------
# Returns both old (survival-discounted) and new (fixed) for transparency.
marginal_value <- function(kth_to, current_to, gsr, score_diff = 0, yl = 50) {
  p_drive <- p_qualifying_drive(gsr, yl, score_diff, current_to)
  p_surv  <- p_to_survives(kth_to, current_to, gsr, score_diff)
  p_use   <- as.numeric(P_USE_KTH[as.character(kth_to)])
  toc     <- as.integer(current_to) - as.integer(kth_to) + 1L
  cwp     <- as.numeric(COND_WP[as.character(toc)])
  cep     <- as.numeric(COND_EP[as.character(toc)])
  tibble(kth = kth_to, tos_at_call = toc,
         p_drive = p_drive, p_surv = p_surv, p_use = p_use, cond_wp = cwp,
         old_wp  = p_drive * p_surv * p_use * cwp,   # frozen Phase 6 (survival)
         burn_wp = p_drive *          p_use * cwp,   # FIXED (drop survival)
         burn_ep = p_drive *          p_use * cep)
}

# ---- confirmation print: hold 3 at Q2 ~8:00 + marginal across the half ------
cat("=== Hold 3 at Q2 ~8:00 (gsr 2280): per-timeout, old vs fixed ===\n")
t1 <- map_dfr(1:3, ~marginal_value(.x, 3, 2280)) %>%
  mutate(label = c("burn last (reserve)", "middle", "burn first (surplus)")[kth],
         across(c(p_surv, cond_wp, old_wp, burn_wp), ~round(.x, 3)))
print(as.data.frame(t1[, c("label","p_surv","p_use","cond_wp","old_wp","burn_wp")]),
      row.names = FALSE)
cat(sprintf("Scarcity ratio reserve/surplus: old %.1fx -> fixed %.1fx\n",
            t1$old_wp[1]/t1$old_wp[3], t1$burn_wp[1]/t1$burn_wp[3]))

# ---- build + save the burn-cost lookup grid ---------------------------------
# Key = (gsr, score_differential, current_to); value = cost to burn the
# marginal TO (the one you'd burn first = kth_to = current_to). yardline fixed
# at 50 (matters only via p_drive after 9:00 Q2). Also store the full per-kth
# breakdown for "total value held".
GSR_GRID   <- c(3500, 3000, 2700, 2400, 2280, 2100, 1950)
SCORE_GRID <- c(-14, -10, -7, -3, 0, 3, 7, 10, 14)

lookup <- expand_grid(gsr = GSR_GRID, score_differential = SCORE_GRID,
                      current_to = 1:3) %>%
  rowwise() %>%
  mutate(r = list(marginal_value(current_to, current_to, gsr, score_differential))) %>%
  unnest(r) %>% ungroup() %>%
  transmute(gsr, score_differential, current_to,
            p_drive = round(p_drive, 3),
            burn_wp = round(burn_wp, 4), burn_ep = round(burn_ep, 4),
            old_burn_wp = round(p_drive * p_surv * p_use * cond_wp, 4))

cat("\n=== Saved lookup (marginal burn cost), tied score, by gsr x current_to ===\n")
print(as.data.frame(
  lookup %>% filter(score_differential == 0) %>%
    select(gsr, current_to, p_drive, old_burn_wp, burn_wp)
), row.names = FALSE)

# full per-kth breakdown (total value held), tied, across gsr
breakdown <- map_dfr(GSR_GRID, function(g)
    map_dfr(1:3, ~marginal_value(.x, 3, g, 0)) %>% mutate(gsr = g)) %>%
  transmute(gsr, kth, tos_at_call, burn_wp = round(burn_wp, 4))

saveRDS(lookup, file.path(rds_dir, "phase9_h1_burncost.rds"))
saveRDS(list(
  method = paste("H1 early-burn fix: DROP p_survives so each TO is valued at its",
                 "full drill value (= old value / p_surv). value = p_drive *",
                 "p_use(kth) * cond(tos_at_call). Credits early-burned TOs at full",
                 "drill value as a stand-in for unmodeled early uses; scarcity",
                 "preserved by p_use + cond. No lambda. Frozen Phase 6 untouched."),
  formula = "burn_wp = p_drive * p_use(kth) * cond(tos_at_call)",
  cond_wp = COND_WP, p_use_kth = P_USE_KTH,
  gsr_grid = GSR_GRID, score_grid = SCORE_GRID,
  breakdown_hold3_tied = breakdown,
  built = Sys.time()
), file.path(rds_dir, "phase9_h1_meta.rds"))

cat("\nSaved phase9_h1_burncost.rds (", nrow(lookup), "rows) and phase9_h1_meta.rds\n", sep = "")
cat("=== DONE (frozen Phase 6 outputs untouched) ===\n")
