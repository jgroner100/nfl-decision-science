# ============================================================================
# Phase9b_H1_Reanchor.R — DECISION-LAYER re-anchor of the H1 timeout burn cost.
# Replaces H1's EP->WP conversion (the global cross-sectional slope 0.04578) with
# a vegas_wp-DIRECT, SCORE-CONDITIONAL value-of-a-point V(score) measured on the
# H1 2-min-drill population (phase1_ep_wp_calib.rds, gsr 1800-1933). This puts H1
# on the same vegas_wp ruler as H2/DOG and makes it leverage-aware (it was
# score-flat). The frozen EP machinery (phase6c mean_ep) and Phase 4/6/6c/9 are
# untouched -- this only swaps the final EP->WP constant for V(score):
#   old: burn_wp = p_drive * p_use(kth) * mean_ep(toc) * 0.04578 * 100
#   new: burn_wp = p_drive * p_use(kth) * mean_ep(toc) * V(score) * 100
# Saves: phase9b_h1_burncost_reanchored.rds + phase9b_h1_meta.rds  (new files).
# ============================================================================
suppressPackageStartupMessages({ library(mgcv); library(tidyverse) })
rds_dir <- file.path(getwd(), "build")
SLOPE_OLD <- 0.04578

pos_prob_model <- readRDS(file.path(rds_dir,"phase4_pos_prob_model.rds"))
P_DRIVE_BEFORE_9MIN <- 0.55; MODEL_CUTOFF_GSR <- 2340
P_USE_KTH <- c("1"=0.500,"2"=0.270,"3"=0.181)
by_toc <- readRDS(file.path(rds_dir,"phase6c_call_weighted_value.rds"))$by_starting_to %>%
  arrange(posteam_timeouts_remaining)
COND_EP <- setNames(by_toc$mean_ep, as.character(by_toc$posteam_timeouts_remaining))

# ---- V(score): vegas_wp value-of-a-point on the drill population -------------
cal <- as.data.frame(readRDS(file.path(rds_dir,"phase1_ep_wp_calib.rds")))
cat("calib score range:", paste(range(cal$score_differential),collapse=" .. "),
    " | gsr:", paste(round(range(cal$game_seconds_remaining)),collapse=" .. "), "\n")
vop_gam <- gam(vegas_wp ~ s(score_differential, k=10), data=cal, method="REML")
Vfun <- function(s){ s <- pmin(pmax(s,-21),21)
  as.numeric(predict(vop_gam, data.frame(score_differential=s+0.5)) -
             predict(vop_gam, data.frame(score_differential=s-0.5))) }
cat(sprintf("V(0)=%.4f   (old global slope %.4f; DOG 1st-half VoP ~0.034 at gsr2700)\n", Vfun(0), SLOPE_OLD))
vtab <- data.frame(score=c(-14,-10,-7,-3,-1,0,1,3,7,10,14)); vtab$V <- round(Vfun(vtab$score),4)
cat("value-of-a-point V(score) on the drill population:\n"); print(vtab, row.names=FALSE)

# ---- helpers (replicate Phase 9) -------------------------------------------
p_qualifying_drive <- function(gsr, yl=50, score=0, current_to=3L){
  if (gsr > MODEL_CUTOFF_GSR) return(P_DRIVE_BEFORE_9MIN)
  nd <- data.frame(game_seconds_remaining=gsr, yardline_100=yl, score_differential=score,
                   down=1L, ydstogo=10, posteam_timeouts_remaining=as.integer(current_to),
                   defteam_timeouts_remaining=3L, rating_differential=0)
  as.numeric(predict(pos_prob_model, nd, type="response")) }
marginal_value <- function(kth, current_to, gsr, score=0, yl=50){
  p_drive <- p_qualifying_drive(gsr, yl, score, current_to)
  p_use <- as.numeric(P_USE_KTH[as.character(kth)])
  toc <- as.integer(current_to) - as.integer(kth) + 1L
  ep <- as.numeric(COND_EP[as.character(toc)]); V <- Vfun(score)
  tibble(kth=kth, tos_at_call=toc, p_drive=p_drive, V=V, burn_ep=p_drive*p_use*ep,
         burn_wp=p_drive*p_use*ep*V*100, burn_wp_old=p_drive*p_use*ep*SLOPE_OLD*100) }

# ---- lookup grid (mirror Phase 9 keys) + before/after -----------------------
GSR_GRID <- c(3500,3000,2700,2400,2280,2100,1950)
SCORE_GRID <- c(-14,-10,-7,-3,0,3,7,10,14)
lookup <- expand_grid(gsr=GSR_GRID, score_differential=SCORE_GRID, current_to=1:3) %>%
  rowwise() %>% mutate(r=list(marginal_value(current_to,current_to,gsr,score_differential))) %>%
  unnest(r) %>% ungroup() %>%
  transmute(gsr, score_differential, current_to, p_drive=round(p_drive,3), V=round(V,4),
            burn_wp=round(burn_wp,4), burn_ep=round(burn_ep,4), burn_wp_old=round(burn_wp_old,4))

cat("\n=== re-anchored vs old, TIED, by gsr x current_to (expect ~unchanged) ===\n")
print(as.data.frame(lookup %>% filter(score_differential==0) %>%
  select(gsr,current_to,p_drive,V,burn_wp_old,burn_wp)), row.names=FALSE)
cat("\n=== score sweep at gsr 2280 (new leverage taper; current_to=3 = marginal) ===\n")
print(as.data.frame(lookup %>% filter(gsr==2280, current_to==3) %>%
  select(score_differential,V,burn_wp_old,burn_wp)), row.names=FALSE)

saveRDS(lookup, file.path(rds_dir,"phase9b_h1_burncost_reanchored.rds"))
saveRDS(list(method="H1 re-anchored to vegas_wp value-of-a-point V(score) on the 2-min-drill population (calib); replaces global EP->WP slope 0.04578. Score-conditional (Option B). Frozen EP/Phase4/6/6c/9 untouched.",
             V_of_score=vtab, V0=Vfun(0), slope_old=SLOPE_OLD, cond_ep=COND_EP, p_use=P_USE_KTH,
             vop_gam_note="gam(vegas_wp ~ s(score_differential,k=10)) on phase1_ep_wp_calib.rds", built=Sys.time()),
        file.path(rds_dir,"phase9b_h1_meta.rds"))
cat("\nSaved phase9b_h1_burncost_reanchored.rds + phase9b_h1_meta.rds\n")
