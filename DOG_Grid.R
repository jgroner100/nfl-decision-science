# ============================================================================
# DOG_Grid.R — regenerate the DOG cost as a STATE-KEYED GRID for the decision
# layer, reusing DOG v8's EXACT machinery (smooth FG make-prob GAM, hybrid
# recal, finer first-half time bins, net-punt GAM, the validated cost fns).
# dog_v8_final_ci.rds only stored 9 scenarios + the MC matrix; the layer needs
# a cost at every (action, margin, yardline, time). POINT ESTIMATES on the grid
# (the 9-scenario 95% CIs remain in dog_v8_final_ci.rds for the article).
# M = calling-team score margin (negative = trailing) — same sign as H1/H2.
# Actions = {2pt, FG, punt}; go-for-it is not a DOG branch (team snaps/burns TO).
# Saves: dog_grid_lookup.rds  (new file; v8 outputs untouched).
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(nflreadr); library(mgcv) })
rds_dir <- file.path(getwd(), "build"); clamp01 <- function(x) pmin(pmax(x,0),1)
set.seed(4); SMOOTH_MIN_GSR <- 240
message("load_pbp(2017:2024)...")
source("load_play_by_play.R")
pbp <- load_play_by_play(2017:2024)

# ---- FG make-prob: smooth logistic GAM (v8) --------------------------------
fg <- pbp %>% filter(play_type=="field_goal", !is.na(kick_distance), !is.na(field_goal_result)) %>% mutate(made=as.integer(field_goal_result=="made"))
xp <- pbp %>% filter(play_type=="extra_point", !is.na(extra_point_result)) %>% mutate(made=as.integer(extra_point_result=="good"))
fg_gam <- gam(made ~ s(kick_distance, k=12), data=fg, family=binomial(), method="REML")
dd <- 18:65; Xdd <- predict(fg_gam, tibble(kick_distance=dd), type="lpmatrix"); X38 <- predict(fg_gam, tibble(kick_distance=38), type="lpmatrix")
mono <- function(eta) -isoreg(dd, -plogis(as.numeric(eta)))$yf
# 2026-06-04: nfl4th-style long-kick selection-bias decay (anchor kd=58, linear to 0 at kd=71; 0
# beyond). Fixes the flat-extrap tail (a 71yd FG had the same make-prob as 65yd). No effect kd<58.
fgdecay <- function(Pf){ p58<-Pf(58); function(kd) ifelse(kd<58, Pf(kd), ifelse(kd>=71, 0, ((71-kd)/13)*p58)) }
P_fg_pt <- fgdecay(approxfun(dd, mono(Xdd %*% coef(fg_gam)), rule=2))
mx <- sum(xp$made); nx <- nrow(xp); P33_pt <- mx/nx; P38_pt <- plogis(as.numeric(X38 %*% coef(fg_gam))); gap2_pt <- P33_pt - P38_pt

# ---- net punt (v8) — CONTINUOUS over yardline for the grid ------------------
seqd <- pbp %>% group_by(game_id) %>% mutate(nxt_yl=lead(yardline_100), nxt_pos=lead(posteam)) %>% ungroup()
punt_out <- seqd %>% filter(play_type=="punt", down==4, !is.na(yardline_100), !is.na(nxt_yl), !is.na(nxt_pos), nxt_pos!=posteam)
net_punt <- gam(nxt_yl~s(yardline_100,k=9), data=punt_out)
F_cont <- function(yl) as.numeric(predict(net_punt, tibble(yardline_100=yl)))   # opp resulting yardline

# ---- WP surface: finer first-half bins + hybrid recal (v8) -----------------
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
emp_stats <- lapply(1:nb, function(i){ kb(i) %>% group_by(mc) %>% summarise(wp=mean(vegas_wp), .groups="drop") %>% arrange(mc) })
recal_gam <- lapply(1:nb, function(i) if(recal_smooth[i]) gam(vegas_wp ~ s(mc, k=8), data=kb(i), method="REML") else NULL)
empm_build <- function(i, vals) approxfun(vals$x, isoreg(vals$x, vals$y)$yf, rule=2)
empm_pt <- lapply(1:nb, function(i){ if(recal_smooth[i]) empm_build(i, list(x=mcvals, y=as.numeric(predict(recal_gam[[i]], tibble(mc=mcvals)))))
                                     else { d<-emp_stats[[i]]; empm_build(i, list(x=d$mc, y=d$wp)) } })
make_wp_arr <- function(betas, empm) lapply(1:nb, function(i){
  mtx <- matrix(clamp01(as.numeric(Xf[[i]] %*% betas[[i]])), nrow=length(MG))
  cv  <- empm[[i]](MG) - clamp01(as.numeric(X75[[i]] %*% betas[[i]])); clamp01(mtx + cv) })
WPpos_g <- function(wp_arr) function(m,yl,t){ a<-min(max(round(m),-16),16)+17; b<-min(max(round(yl),1),99)
  clamp01(approx(mid, sapply(1:nb, function(i) wp_arr[[i]][a,b]), xout=min(max(t,mid[1]),mid[nb]), rule=2)$y) }
KO <- 75
fg_cost_g    <- function(Y,M,T,P_fg,WP){ kd<-Y+18; p0<-P_fg(kd); p5<-P_fg(kd+5); Wm<-1-WP(-(M+3),KO,T); W0<-1-WP(-M,93-Y,T); W5<-1-WP(-M,88-Y,T); (p0*Wm+(1-p0)*W0-(p5*Wm+(1-p5)*W5))*100 }
twopt_cost_g <- function(M,T,gap2,WP) gap2*((1-WP(-(M+1),KO,T))-(1-WP(-M,KO,T)))*100
punt_cost_g  <- function(M,yl,T,F,WP) (WP(-M,F(yl+5),T)-WP(-M,F(yl),T))*100

WP <- WPpos_g(make_wp_arr(lapply(wpm, coef), empm_pt))

# ---- sanity vs v8's 9 scenarios (must match) -------------------------------
cat("\n=== sanity: grid machinery vs DOG v8 point estimates ===\n")
cat(sprintf("  twopt_FH      %.3f  (v8 0.196)\n", twopt_cost_g(-1,2700,gap2_pt,WP)))
cat(sprintf("  twopt_decide  %.3f  (v8 1.781)\n", twopt_cost_g(-1,10,gap2_pt,WP)))
cat(sprintf("  FG52_tie_FH   %.3f  (v8 1.865)\n", fg_cost_g(34,-3,2700,P_fg_pt,WP)))
cat(sprintf("  FG52_win_dec  %.3f  (v8 7.082)\n", fg_cost_g(34,0,10,P_fg_pt,WP)))
cat(sprintf("  punt_own20_FH %.3f  (v8 0.921)\n", punt_cost_g(0,80,2700,F_cont,WP)))
cat(sprintf("  punt_deadzone %.3f  (v8 0.165)\n", punt_cost_g(0,40,2700,F_cont,WP)))
cat(sprintf("  gap2 %.4f (v8 0.0582)  P33 %.4f  P38 %.4f\n", gap2_pt, P33_pt, P38_pt))

# ---- the grid --------------------------------------------------------------
# gsr aligned to BOTH H1 (1950-3500) and H2 (120-1800) grids + late binary.
T_GRID  <- c(3500,3000,2700,2400,2280,2100,1950,1800,1650,1500,1200,900,600,480,360,240,120,60,30,15)
M_GRID  <- c(-14,-10,-7,-4,-3,-1,0,1,3,4,7,10,14)
Y_FG    <- c(5,10,15,20,25,30,35,40,43,45,47)   # kd=Y+18 -> 23..65 yd (decay covers the long tail)
YL_PUNT <- c(40,50,60,70,80,90)        # LOS yardline_100 (dead-zone .. backed-up)

twopt <- expand_grid(action="2pt",  yardline=NA_real_, M=M_GRID, T=T_GRID) %>%
  mutate(dog_cost_wp = mapply(function(m,t) twopt_cost_g(m,t,gap2_pt,WP), M, T))
fgg   <- expand_grid(action="FG",   yardline=Y_FG,     M=M_GRID, T=T_GRID) %>%
  mutate(dog_cost_wp = mapply(function(y,m,t) fg_cost_g(y,m,t,P_fg_pt,WP), yardline, M, T))
puntg <- expand_grid(action="punt", yardline=YL_PUNT,  M=M_GRID, T=T_GRID) %>%
  mutate(dog_cost_wp = mapply(function(y,m,t) punt_cost_g(m,y,t,F_cont,WP), yardline, M, T))
dog_grid <- bind_rows(twopt, fgg, puntg) %>% rename(score_margin=M, gsr=T) %>%
  mutate(dog_cost_wp = round(dog_cost_wp, 4))

cat(sprintf("\ngrid rows: %d  (2pt %d, FG %d, punt %d)\n", nrow(dog_grid), nrow(twopt), nrow(fgg), nrow(puntg)))
cat("spot check — FG by distance, tied, deciding (gsr 15):\n")
print(as.data.frame(dog_grid %>% filter(action=="FG", score_margin==0, gsr==15) %>% select(yardline, dog_cost_wp)), row.names=FALSE)

saveRDS(list(grid=dog_grid, T_grid=T_GRID, M_grid=M_GRID, Y_fg=Y_FG, yl_punt=YL_PUNT,
  gap2=gap2_pt, P33=P33_pt, P38=P38_pt, gsr_bins=GB, smooth_min_gsr=SMOOTH_MIN_GSR,
  method="DOG cost grid from v8 machinery (smooth FG GAM + hybrid recal + finer 1st-half bins + continuous net-punt) + 2026-06-04 nfl4th-style FG long-kick decay (anchor kd58 -> 0 at kd71; fixes the >65yd flat-extrap tail). Point estimates; 9-scenario CIs in dog_v8_final_ci.rds. M=calling-team margin (neg=trailing). actions {2pt,FG,punt}.",
  built=Sys.time()), file.path(rds_dir,"dog_grid_lookup.rds"))
cat("\nSaved dog_grid_lookup.rds\n")
