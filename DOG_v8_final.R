# ============================================================================
# DOG v8 (FINAL): v7 (smooth FG make-prob + hybrid recal) + FINER first-half
#   time bins so the early value-of-a-point is gsr-resolved (3.4%/pt at gsr 2700,
#   matching raw vegas_wp) instead of the coarse 1800-3600 bin average (3.7%).
#   ONLY the first-half bin is subdivided (1800/2400/3000/3600); the late-binary
#   bins (0-1800) are byte-for-byte unchanged, so the end-game is untouched.
#   This is the load-bearing DOG model. v4 superseded for points + CIs.
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(nflreadr); library(mgcv) })
rds_dir <- file.path(getwd(), "build"); clamp01 <- function(x) pmin(pmax(x,0),1)
set.seed(4); B <- 1000; SMOOTH_MIN_GSR <- 240
source("load_play_by_play.R")
pbp <- load_play_by_play(2017:2024)

# ---- FG make-prob: smooth logistic GAM -------------------------------------
fg <- pbp %>% filter(play_type=="field_goal", !is.na(kick_distance), !is.na(field_goal_result)) %>% mutate(made=as.integer(field_goal_result=="made"))
xp <- pbp %>% filter(play_type=="extra_point", !is.na(extra_point_result)) %>% mutate(made=as.integer(extra_point_result=="good"))
fg_gam <- gam(made ~ s(kick_distance, k=12), data=fg, family=binomial(), method="REML")
dd <- 18:65; Xdd <- predict(fg_gam, tibble(kick_distance=dd), type="lpmatrix"); X38 <- predict(fg_gam, tibble(kick_distance=38), type="lpmatrix")
mono <- function(eta) -isoreg(dd, -plogis(as.numeric(eta)))$yf
# 2026-06-04: nfl4th-style long-kick selection-bias decay (anchor kd=58, linear to 0 at kd=71; 0
# beyond). Fixes the flat-extrap tail (a 71yd FG had the same make-prob as 65yd). No effect kd<58,
# so the 9 headline scenarios are unchanged; only very-long FGs differ.
fgdecay <- function(Pf){ p58<-Pf(58); function(kd) ifelse(kd<58, Pf(kd), ifelse(kd>=71, 0, ((71-kd)/13)*p58)) }
P_fg_pt <- fgdecay(approxfun(dd, mono(Xdd %*% coef(fg_gam)), rule=2))
mx <- sum(xp$made); nx <- nrow(xp); P33_pt <- mx/nx; P38_pt <- plogis(as.numeric(X38 %*% coef(fg_gam))); gap2_pt <- P33_pt - P38_pt

# ---- net punt --------------------------------------------------------------
seqd <- pbp %>% group_by(game_id) %>% mutate(nxt_yl=lead(yardline_100), nxt_pos=lead(posteam)) %>% ungroup()
punt_out <- seqd %>% filter(play_type=="punt", down==4, !is.na(yardline_100), !is.na(nxt_yl), !is.na(nxt_pos), nxt_pos!=posteam)
net_punt <- gam(nxt_yl~s(yardline_100,k=9), data=punt_out)
np_yls <- c(40,45,80,85); X_np <- predict(net_punt, tibble(yardline_100=np_yls), type="lpmatrix")
F_pt <- setNames(as.numeric(X_np %*% coef(net_punt)), np_yls); Ffun <- function(Fv) function(yl) Fv[as.character(yl)]

# ---- WP surface: FINER first-half bins (1800/2400/3000/3600) ----------------
GB <- c(0,15,30,60,120,240,480,900,1800,2400,3000,3600); nb <- length(GB)-1; mid <- (head(GB,-1)+tail(GB,-1))/2
wpd <- pbp %>% filter(!is.na(vegas_wp),!is.na(score_differential),!is.na(game_seconds_remaining),!is.na(yardline_100),
                      qtr<=4, play_type %in% c("pass","run","qb_kneel","qb_spike"),
                      game_seconds_remaining>0, game_seconds_remaining<=3600, abs(score_differential)<=24) %>%
  mutate(gi = findInterval(game_seconds_remaining, GB, rightmost.closed=TRUE))
wpm <- lapply(1:nb, function(i) bam(vegas_wp ~ te(score_differential, yardline_100, k=c(12,5)), data=filter(wpd, gi==i), method="fREML", discrete=TRUE))
MG <- -16:16; YG <- 1:99; mcvals <- -14:14
gridf <- expand.grid(score_differential=MG, yardline_100=YG)
Xf  <- lapply(1:nb, function(i) predict(wpm[[i]], gridf, type="lpmatrix"))
X75 <- lapply(1:nb, function(i) predict(wpm[[i]], tibble(score_differential=MG, yardline_100=75), type="lpmatrix"))

recal_smooth <- mid >= SMOOTH_MIN_GSR
kb <- function(i) filter(wpd, gi==i, yardline_100>=65, yardline_100<=85, abs(score_differential)<=14) %>% mutate(mc=pmin(pmax(score_differential,-14),14))
emp_stats <- lapply(1:nb, function(i){ kb(i) %>% group_by(mc) %>% summarise(wp=mean(vegas_wp), se=sd(vegas_wp)/sqrt(n()), n=n(), .groups="drop") %>% arrange(mc) })
recal_gam <- lapply(1:nb, function(i) if(recal_smooth[i]) gam(vegas_wp ~ s(mc, k=8), data=kb(i), method="REML") else NULL)
Xmc    <- lapply(1:nb, function(i) if(recal_smooth[i]) predict(recal_gam[[i]], tibble(mc=mcvals), type="lpmatrix") else NULL)
Brecal <- lapply(1:nb, function(i) if(recal_smooth[i]) MASS::mvrnorm(B, coef(recal_gam[[i]]), vcov(recal_gam[[i]])) else NULL)
empm_build <- function(i, vals) approxfun(vals$x, isoreg(vals$x, vals$y)$yf, rule=2)
empm_pt <- lapply(1:nb, function(i){ if(recal_smooth[i]) empm_build(i, list(x=mcvals, y=as.numeric(predict(recal_gam[[i]], tibble(mc=mcvals)))))
                                     else { d<-emp_stats[[i]]; empm_build(i, list(x=d$mc, y=d$wp)) } })

# value-of-a-point the model now uses at the first-half bin (gsr 2700):
vp <- empm_pt[[which(round(mid)==2700)]]; cat(sprintf("Model value-of-a-point at gsr 2700 bin: %.3f (target ~0.034)\n", vp(1)-vp(0)))

make_wp_arr <- function(betas, empm) lapply(1:nb, function(i){
  mtx <- matrix(clamp01(as.numeric(Xf[[i]] %*% betas[[i]])), nrow=length(MG))
  cv  <- empm[[i]](MG) - clamp01(as.numeric(X75[[i]] %*% betas[[i]])); clamp01(mtx + cv) })
WPpos_g <- function(wp_arr) function(m,yl,t){ a<-min(max(round(m),-16),16)+17; b<-min(max(round(yl),1),99)
  clamp01(approx(mid, sapply(1:nb, function(i) wp_arr[[i]][a,b]), xout=min(max(t,mid[1]),mid[nb]), rule=2)$y) }
KO <- 75
fg_cost_g    <- function(Y,M,T,P_fg,WP){ kd<-Y+18; p0<-P_fg(kd); p5<-P_fg(kd+5); Wm<-1-WP(-(M+3),KO,T); W0<-1-WP(-M,93-Y,T); W5<-1-WP(-M,88-Y,T); (p0*Wm+(1-p0)*W0-(p5*Wm+(1-p5)*W5))*100 }
twopt_cost_g <- function(M,T,gap2,WP) gap2*((1-WP(-(M+1),KO,T))-(1-WP(-M,KO,T)))*100
punt_cost_g  <- function(M,yl,T,F,WP) (WP(-M,F(yl+5),T)-WP(-M,F(yl),T))*100
scen <- function(P_fg,gap2,F,WP) c(
  twopt_FH=twopt_cost_g(-1,2700,gap2,WP), twopt_late2m=twopt_cost_g(-1,120,gap2,WP), twopt_decide=twopt_cost_g(-1,10,gap2,WP),
  FG52_tie_FH=fg_cost_g(34,-3,2700,P_fg,WP), FG52_tie_dec=fg_cost_g(34,-3,10,P_fg,WP), FG52_win_dec=fg_cost_g(34,0,10,P_fg,WP),
  FG50_FH=fg_cost_g(32,0,2700,P_fg,WP), punt_own20_FH=punt_cost_g(0,80,2700,F,WP), punt_deadzone=punt_cost_g(0,40,2700,F,WP))
point <- scen(P_fg_pt, gap2_pt, Ffun(F_pt), WPpos_g(make_wp_arr(lapply(wpm, coef), empm_pt)))

Bfg  <- MASS::mvrnorm(B, coef(fg_gam), vcov(fg_gam))
Bwpm <- lapply(1:nb, function(i) MASS::mvrnorm(B, coef(wpm[[i]]), vcov(wpm[[i]])))
Bnp  <- MASS::mvrnorm(B, coef(net_punt), vcov(net_punt))
mc <- matrix(NA_real_, B, length(point), dimnames=list(NULL, names(point)))
for (b in 1:B) {
  P_fg_b <- fgdecay(approxfun(dd, mono(Xdd %*% Bfg[b,]), rule=2))
  gap2_b <- rbeta(1, mx+0.5, nx-mx+0.5) - plogis(as.numeric(X38 %*% Bfg[b,]))
  F_b    <- Ffun(setNames(as.numeric(X_np %*% Bnp[b,]), np_yls))
  empm_b <- lapply(1:nb, function(i){ if(recal_smooth[i]) empm_build(i, list(x=mcvals, y=as.numeric(Xmc[[i]] %*% Brecal[[i]][b,])))
            else { d<-emp_stats[[i]]; se<-ifelse(is.na(d$se)|d$n<2,0,d$se); empm_build(i, list(x=d$mc, y=rnorm(nrow(d), d$wp, se))) } })
  WP_b   <- WPpos_g(make_wp_arr(lapply(1:nb, function(i) Bwpm[[i]][b,]), empm_b))
  mc[b,] <- scen(P_fg_b, gap2_b, F_b, WP_b)
}
ci <- t(apply(mc, 2, quantile, c(.025,.975), na.rm=TRUE))
out <- tibble(scenario=names(point), point=round(point,3), lo95=round(ci[,1],3), hi95=round(ci[,2],3),
              hw_pct=round((ci[,2]-ci[,1])/2/pmax(abs(point),1e-6)*100,0))
cat("\n=== DOG cost WP%, point + 95% CI (v8 FINAL: finer first-half bins) ===\n")
print(as.data.frame(out), row.names=FALSE)
saveRDS(list(scenarios=out, mc=mc, B=B, gsr_bins=GB, smooth_min_gsr=SMOOTH_MIN_GSR,
             gap2=gap2_pt, P33=P33_pt, P38=P38_pt,
             method="DOG FINAL (v8): smooth FG make-prob GAM + hybrid recal (smooth early / empirical late binary) + finer first-half time bins. 2026-06-04: + nfl4th-style FG long-kick decay (anchor kd58 -> 0 at kd71; fixes >65yd flat-extrap tail, 9 headline scenarios unchanged). v4 superseded."),
        file.path(rds_dir, "dog_v8_final_ci.rds"))
cat("\nSaved dog_v8_final_ci.rds (load-bearing DOG model). Late-binary bins unchanged from v7.\n")
