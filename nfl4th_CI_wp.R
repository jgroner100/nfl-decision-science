# ============================================================================
# nfl4th_CI_wp.R — STEP 2 of the go_boost CI: add the WIN-PROBABILITY-SURFACE
# uncertainty on top of the conversion band (step 1), keeping nfl4th's go_boost
# as the fixed POINT estimate.
#
# nfl4th's WP is a frozen xgboost (no vcov to draw from), so the WP-surface
# uncertainty WIDTH is sourced by POSTERIOR-SIMULATING our refittable GAM WP
# surface (the same one DOG/H2 use) and using it purely as a SENSITIVITY around
# nfl4th's point:
#   go_boost_i,b = go_boost_nfl4th_i  +  dconv_i,b  +  dwp_i,b
#     dconv_i,b = go_boost_conv(p_i,b) - go_boost_conv(p_hat_i)     [nfl4th wp_succeed/wp_fail; p_b ~ Beta(p_hat*n_eff)]
#     dwp_i,b   = go_boost_oursurf(theta_b)_i - go_boost_oursurf(theta_hat)_i   [theta_b ~ posterior of our WP GAM]
# theta_b is GLOBAL per draw (one surface), so plays move together (correct).
# The WP-sensitivity uses the simple to-the-sticks go valuation (adequate for a
# width: the delta is insensitive to the go-engine choice). Toss-up = CI straddles 0.
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(nflreadr); library(mgcv); library(MASS) })
select <- dplyr::select
rds_dir <- file.path(getwd(), "build"); clamp01 <- function(x) pmin(pmax(x,0),1)
SMOOTH_MIN_GSR <- 240; B <- 500L; set.seed(20260604)

conv <- readRDS(file.path(rds_dir,"nfl4th_ci_conv.rds"))$ci %>%
  filter(is.finite(maxnon), is.finite(first_down_prob), is.finite(wp_succeed), is.finite(wp_fail),
         is.finite(go_boost), is.finite(n_eff), n_eff>0)
fdm  <- readRDS(file.path(rds_dir,"fourthdown_model.rds"))
P_fg   <- approxfun(fdm$fg_dd, fdm$fg_p, rule=2)
F_net  <- function(yl) as.numeric(predict(fdm$net_punt, data.frame(yardline_100=yl)))
cat(sprintf("plays carried from step 1: %d\n", nrow(conv)))

# ---- refit our WP surface (need coef + vcov; fourthdown_model.rds stored only arrays) ----
message("load_pbp(2017:2024) to refit the WP surface for posterior draws...")
source("load_play_by_play.R")
pbp <- load_play_by_play(2017:2024)
GB <- c(0,15,30,60,120,240,480,900,1800,2400,3000,3600); nb <- length(GB)-1; mid <- (head(GB,-1)+tail(GB,-1))/2
wpd <- pbp %>% filter(!is.na(vegas_wp),!is.na(score_differential),!is.na(game_seconds_remaining),!is.na(yardline_100),
                      qtr<=4, play_type %in% c("pass","run","qb_kneel","qb_spike"),
                      game_seconds_remaining>0, game_seconds_remaining<=3600, abs(score_differential)<=24) %>%
  mutate(gi = findInterval(game_seconds_remaining, GB, rightmost.closed=TRUE))
message("fitting 11 WP bams...")
wpm <- lapply(1:nb, function(i) bam(vegas_wp ~ te(score_differential, yardline_100, k=c(12,5)),
                                    data=filter(wpd, gi==i), method="fREML", discrete=TRUE))
MG <- -16:16
Xf  <- lapply(1:nb, function(i) predict(wpm[[i]], expand.grid(score_differential=MG, yardline_100=1:99), type="lpmatrix"))
X75 <- lapply(1:nb, function(i) predict(wpm[[i]], tibble(score_differential=MG, yardline_100=75), type="lpmatrix"))
recal_smooth <- mid >= SMOOTH_MIN_GSR; mcvals <- -14:14
kb <- function(i) filter(wpd, gi==i, yardline_100>=65, yardline_100<=85, abs(score_differential)<=14) %>% mutate(mc=pmin(pmax(score_differential,-14),14))
emp_stats <- lapply(1:nb, function(i){ kb(i) %>% group_by(mc) %>% summarise(wp=mean(vegas_wp), .groups="drop") %>% arrange(mc) })
recal_gam <- lapply(1:nb, function(i) if(recal_smooth[i]) gam(vegas_wp ~ s(mc, k=8), data=kb(i), method="REML") else NULL)
empm_build <- function(vals) approxfun(vals$x, isoreg(vals$x, vals$y)$yf, rule=2)
empm_pt <- lapply(1:nb, function(i){ if(recal_smooth[i]) empm_build(list(x=mcvals, y=as.numeric(predict(recal_gam[[i]], tibble(mc=mcvals))))) else { d<-emp_stats[[i]]; empm_build(list(x=d$mc, y=d$wp)) } })

# wp_arr from an arbitrary coefficient list (the recal anchor empm_pt is held fixed)
make_wp_arr <- function(betas) lapply(1:nb, function(i){
  mtx <- matrix(clamp01(as.numeric(Xf[[i]] %*% betas[[i]])), nrow=length(MG))
  cv  <- empm_pt[[i]](MG) - clamp01(as.numeric(X75[[i]] %*% betas[[i]])); clamp01(mtx + cv) })

# vectorized WP lookup over plays for a given wp_arr (bilinear-in-time bin interp)
WP_vec <- function(m, yl, t, wa){
  A <- pmin(pmax(round(m),-16),16)+17L; Bx <- pmin(pmax(round(yl),1),99); idx <- cbind(A,Bx)
  VALS <- vapply(1:nb, function(i) wa[[i]][idx], numeric(length(A)))
  tc <- pmin(pmax(t, mid[1]), mid[nb]); lo <- pmin(pmax(findInterval(tc, mid),1L), nb-1L); hi <- lo+1L
  w <- (tc - mid[lo])/(mid[hi]-mid[lo])
  clamp01((1-w)*VALS[cbind(seq_along(tc),lo)] + w*VALS[cbind(seq_along(tc),hi)]) }

# ---- play vectors + go_boost on OUR surface (to-the-sticks; conv fixed at nfl4th p_hat) ----
M <- conv$score_differential; Y <- conv$yardline_100; ytg <- conv$ydstogo; Tt <- conv$game_seconds_remaining
phat <- conv$first_down_prob; n_eff <- conv$n_eff; n <- nrow(conv)
pm <- P_fg(Y+18); Fp <- F_net(Y)
use_fg <- !is.na(conv$fg_wp) & (is.na(conv$punt_wp) | conv$fg_wp >= conv$punt_wp)   # which alt nfl4th used
KO <- rep(75, n)
go_boost_oursurf <- function(wa){
  ny <- Y - ytg
  ws <- ifelse(ny<=0, 1 - WP_vec(-(M+7), KO, Tt, wa), WP_vec(M, pmin(pmax(ny,1),99), Tt, wa))
  wf <- 1 - WP_vec(-M, pmin(pmax(100-Y,1),99), Tt, wa)
  go <- phat*ws + (1-phat)*wf
  fg <- pm*(1 - WP_vec(-(M+3), KO, Tt, wa)) + (1-pm)*(1 - WP_vec(-M, pmin(pmax(93-Y,1),99), Tt, wa))
  pu <- 1 - WP_vec(-M, pmin(pmax(Fp,1),99), Tt, wa)
  100*(go - ifelse(use_fg, fg, pu)) }

# conversion-leg go_boost using nfl4th's own wp_succeed/wp_fail (fg/punt fixed)
wsucc <- conv$wp_succeed; wfail <- conv$wp_fail; maxnon_nfl <- conv$maxnon
gb_conv <- function(p) 100*((p*wsucc + (1-p)*wfail) - maxnon_nfl)
gb_conv_hat <- gb_conv(phat)
gb_nfl <- conv$go_boost
gb_surf_hat <- go_boost_oursurf(make_wp_arr(lapply(wpm, coef)))

# ---- Monte Carlo: combine conversion draws + WP-surface posterior draws -------
message(sprintf("MC over B=%d draws (conv + WP-surface)...", B))
draws_comb <- matrix(0, n, B); draws_conv <- matrix(0, n, B)
dwp_sum <- numeric(n); dwp_sq <- numeric(n)
for (b in 1:B){
  theta <- lapply(1:nb, function(i) MASS::mvrnorm(1, coef(wpm[[i]]), vcov(wpm[[i]])))
  dwp   <- go_boost_oursurf(make_wp_arr(theta)) - gb_surf_hat
  p_b   <- rbeta(n, phat*n_eff + 0.5, (1-phat)*n_eff + 0.5)
  dconv <- gb_conv(p_b) - gb_conv_hat
  draws_conv[,b] <- gb_nfl + dconv
  draws_comb[,b] <- gb_nfl + dconv + dwp
  dwp_sum <- dwp_sum + dwp; dwp_sq <- dwp_sq + dwp^2
}
qrow <- function(Mx, q) apply(Mx, 1, quantile, probs=q, names=FALSE)
out <- conv %>% transmute(game_id, play_id, season, qtr, game_seconds_remaining, yardline_100, ydstogo,
                          score_differential, n_eff, go_boost, fg_wp, punt_wp, first_down_prob) %>%
  mutate(
    conv_lo = qrow(draws_conv,0.025), conv_hi = qrow(draws_conv,0.975),
    ci_lo   = qrow(draws_comb,0.025), ci_hi   = qrow(draws_comb,0.975),
    sd_wp   = sqrt(pmax(dwp_sq/B - (dwp_sum/B)^2, 0)),
    half_conv = (conv_hi-conv_lo)/2, half_comb = (ci_hi-ci_lo)/2,
    tossup_conv = conv_lo<0 & conv_hi>0, tossup_comb = ci_lo<0 & ci_hi>0)

# ---- report -----------------------------------------------------------------
cat(sprintf("\nWP-surface |delta| per play (sd over draws, pp): median %.3f  mean %.3f  p95 %.3f\n",
            median(out$sd_wp), mean(out$sd_wp), quantile(out$sd_wp,0.95,names=FALSE)))
cat(sprintf("CI half-width (pp):  conversion-only median %.2f  ->  combined median %.2f\n",
            median(out$half_conv), median(out$half_comb)))
cat(sprintf("\n=== TOSS-UP share (95%% CI straddles 0) ===\n"))
cat(sprintf("  conversion-only : %.1f%%\n", 100*mean(out$tossup_conv)))
cat(sprintf("  + WP surface    : %.1f%%   (marginal +%.1f pp)\n",
            100*mean(out$tossup_comb), 100*(mean(out$tossup_comb)-mean(out$tossup_conv))))

cat("\nby ydstogo (combined):\n")
print(as.data.frame(out %>% mutate(ytg=cut(ydstogo,c(0,1,2,3,5,8,99),labels=c("1","2","3","4-5","6-8","9+"))) %>%
  group_by(ytg) %>% summarise(n=n(), med_half=round(median(half_comb),2),
    med_sd_wp=round(median(sd_wp),3), tossup=round(100*mean(tossup_comb),1))), row.names=FALSE)

cat("\nexample plays (nfl4th point [combined 95% CI], pp):\n")
set.seed(1)
ex <- out %>% sample_n(12) %>%
  transmute(ytg=ydstogo, yl=yardline_100, score=score_differential, gsr=round(game_seconds_remaining),
            go_boost=round(go_boost,2), lo=round(ci_lo,2), hi=round(ci_hi,2),
            sd_wp=round(sd_wp,2), tossup=tossup_comb) %>% arrange(go_boost)
print(as.data.frame(ex), row.names=FALSE)

saveRDS(list(ci=out, B=B, seasons=readRDS(file.path(rds_dir,"nfl4th_ci_conv.rds"))$seasons,
  method="Combined go_boost CI on nfl4th. Point=nfl4th go_boost. Band = conversion sample-size draws (Beta(p_hat*n_eff)) propagated through nfl4th wp_succeed/wp_fail + WP-surface posterior draws (mvrnorm on our GAM WP surface coef/vcov, recal anchor fixed) as an additive sensitivity (to-the-sticks go valuation). Toss-up = 95% CI straddles 0.",
  built=Sys.time()), file.path(rds_dir,"nfl4th_ci_full.rds"))
cat("\nSaved nfl4th_ci_full.rds (combined conversion + WP-surface CI).\n")
