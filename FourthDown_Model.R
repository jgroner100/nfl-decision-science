# ============================================================================
# FourthDown_Model.R — Stage 2-3 of the baseline 4th-down model.
# WP-branch valuation of go / FG / punt, reusing the DOG model's machinery
# (same vegas_wp surface, FG make-prob GAM, net-punt) + the Stage-1 conversion
# model. WP-max decision, vegas_wp units, calling-team perspective.
#   go_wp   = P*WP(new 1st down or TD) + (1-P)*WP(opp ball at LOS, flipped)
#   fg_wp   = make*WP(opp after made FG/KO) + miss*WP(opp at miss spot)   [DOG conv]
#   punt_wp = WP(opp at net-punt field position)
#   go_boost = (go_wp - max(fg_wp, punt_wp)) * 100   [WP percentage points]
# Time held constant across branches (same simplification as DOG; ~5-7s play
# duration is second-order). Saves a canonical sanity table.
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(nflreadr); library(mgcv) })
rds_dir <- file.path(getwd(), "build"); clamp01 <- function(x) pmin(pmax(x,0),1)
SMOOTH_MIN_GSR <- 240
conv <- readRDS(file.path(rds_dir,"fourthdown_conversion_model.rds"))$m4
message("load_pbp(2017:2024)...")
source("load_play_by_play.R")
pbp <- load_play_by_play(2017:2024)

# ---- reuse DOG build: FG make-prob GAM, net-punt, WP surface ----------------
fg <- pbp %>% filter(play_type=="field_goal", !is.na(kick_distance), !is.na(field_goal_result)) %>% mutate(made=as.integer(field_goal_result=="made"))
fg_gam <- gam(made ~ s(kick_distance, k=12), data=fg, family=binomial(), method="REML")
dd <- 18:65; Xdd <- predict(fg_gam, tibble(kick_distance=dd), type="lpmatrix")
mono <- function(eta) -isoreg(dd, -plogis(as.numeric(eta)))$yf
P_fg <- approxfun(dd, mono(Xdd %*% coef(fg_gam)), rule=2)
seqd <- pbp %>% group_by(game_id) %>% mutate(nxt_yl=lead(yardline_100), nxt_pos=lead(posteam)) %>% ungroup()
punt_out <- seqd %>% filter(play_type=="punt", down==4, !is.na(yardline_100), !is.na(nxt_yl), !is.na(nxt_pos), nxt_pos!=posteam)
net_punt <- gam(nxt_yl~s(yardline_100,k=9), data=punt_out)
F_punt <- function(yl) as.numeric(predict(net_punt, tibble(yardline_100=yl)))
GB <- c(0,15,30,60,120,240,480,900,1800,2400,3000,3600); nb <- length(GB)-1; mid <- (head(GB,-1)+tail(GB,-1))/2
wpd <- pbp %>% filter(!is.na(vegas_wp),!is.na(score_differential),!is.na(game_seconds_remaining),!is.na(yardline_100),
                      qtr<=4, play_type %in% c("pass","run","qb_kneel","qb_spike"),
                      game_seconds_remaining>0, game_seconds_remaining<=3600, abs(score_differential)<=24) %>%
  mutate(gi = findInterval(game_seconds_remaining, GB, rightmost.closed=TRUE))
wpm <- lapply(1:nb, function(i) bam(vegas_wp ~ te(score_differential, yardline_100, k=c(12,5)), data=filter(wpd, gi==i), method="fREML", discrete=TRUE))
MG <- -16:16
Xf  <- lapply(1:nb, function(i) predict(wpm[[i]], expand.grid(score_differential=MG, yardline_100=1:99), type="lpmatrix"))
X75 <- lapply(1:nb, function(i) predict(wpm[[i]], tibble(score_differential=MG, yardline_100=75), type="lpmatrix"))
recal_smooth <- mid >= SMOOTH_MIN_GSR; mcvals <- -14:14
kb <- function(i) filter(wpd, gi==i, yardline_100>=65, yardline_100<=85, abs(score_differential)<=14) %>% mutate(mc=pmin(pmax(score_differential,-14),14))
emp_stats <- lapply(1:nb, function(i){ kb(i) %>% group_by(mc) %>% summarise(wp=mean(vegas_wp), .groups="drop") %>% arrange(mc) })
recal_gam <- lapply(1:nb, function(i) if(recal_smooth[i]) gam(vegas_wp ~ s(mc, k=8), data=kb(i), method="REML") else NULL)
empm_build <- function(vals) approxfun(vals$x, isoreg(vals$x, vals$y)$yf, rule=2)
empm_pt <- lapply(1:nb, function(i){ if(recal_smooth[i]) empm_build(list(x=mcvals, y=as.numeric(predict(recal_gam[[i]], tibble(mc=mcvals))))) else { d<-emp_stats[[i]]; empm_build(list(x=d$mc, y=d$wp)) } })
wp_arr <- lapply(1:nb, function(i){ mtx <- matrix(clamp01(as.numeric(Xf[[i]] %*% coef(wpm[[i]]))), nrow=length(MG))
  cv  <- empm_pt[[i]](MG) - clamp01(as.numeric(X75[[i]] %*% coef(wpm[[i]]))); clamp01(mtx + cv) })
WP <- function(m,yl,t){ a<-min(max(round(m),-16),16)+17; b<-min(max(round(yl),1),99)
  clamp01(approx(mid, sapply(1:nb, function(i) wp_arr[[i]][a,b]), xout=min(max(t,mid[1]),mid[nb]), rule=2)$y) }

# ---- conversion + the three branch valuations (calling-team WP) -------------
p_conv <- function(ytg, yl) as.numeric(predict(conv, tibble(ydstogo=ytg, yardline_100=yl), type="response"))
KO <- 75
go_wp   <- function(Y,ytg,M,T){ p<-p_conv(ytg,Y); ny<-Y-ytg
  ws <- if (ny<=0) 1-WP(-(M+7),KO,T) else WP(M,ny,T); wf <- 1-WP(-M,100-Y,T); p*ws+(1-p)*wf }
fg_wp   <- function(Y,M,T){ pm<-P_fg(Y+18); pm*(1-WP(-(M+3),KO,T)) + (1-pm)*(1-WP(-M,93-Y,T)) }
punt_wp <- function(Y,M,T) 1-WP(-M,F_punt(Y),T)
FG_MAX_Y <- 48   # FG is a real option only within ~66-yd range (kick_distance = Y+18)
decide  <- function(Y,ytg,M,T){ g<-go_wp(Y,ytg,M,T); pu<-punt_wp(Y,M,T)
  f <- if (Y<=FG_MAX_Y) fg_wp(Y,M,T) else NA_real_; bk<-max(c(f,pu),na.rm=TRUE); cand<-c(GO=g,FG=f,PUNT=pu)
  tibble(go=round(g,3), fg=round(f,3), punt=round(pu,3), go_boost=round((g-bk)*100,2),
         rec=names(cand)[which.max(cand)], p_conv=round(p_conv(ytg,Y),3)) }

# ---- sanity table: canonical 4th downs (tied unless noted; gsr=1500 = ~Q3) ---
S <- function(lab,Y,ytg,M,T) decide(Y,ytg,M,T) %>% mutate(scenario=lab,.before=1)
tab <- bind_rows(
  S("4th-&-1, opp 40, tied",            40,1,0,1500),
  S("4th-&-1, own 30 (Y=70), tied",     70,1,0,1500),
  S("4th-&-5, own 50 (Y=50), tied",     50,5,0,1500),
  S("4th-&-3, opp 35 (52yd FG), tied",  35,3,0,1500),
  S("4th-&-10, opp 28 (45yd FG), tied", 28,10,0,1500),
  S("4th-&-goal at 2, down 4, Q4",       2,2,-4,600),
  S("4th-&-2, opp 8 (RZ), tied",         8,2,0,1500),
  S("4th-&-15, own 20 (Y=80), tied",    80,15,0,1500))
cat("\n=== 4th-down model sanity (WP per option + go_boost, WP%); calling team ===\n")
print(as.data.frame(tab %>% select(scenario,p_conv,go,fg,punt,go_boost,rec)), row.names=FALSE)

saveRDS(list(sanity=tab, wp_arr=wp_arr, mid=mid, conv=conv, fg_dd=dd, fg_p=P_fg(dd),
             net_punt=net_punt, fg_max_y=FG_MAX_Y,
             note="Baseline 4th-down WP-branch model. Components reconstruct WP/FG/punt/conversion WITHOUT load_pbp: wp_arr+mid (vegas_wp surface, DOG build), conv (4th-down conversion gam m4), fg_dd/fg_p (FG make-prob), net_punt. go_boost=(go-max(fg,punt))*100; FG capped at Y<=48; time held constant across branches."),
        file.path(rds_dir,"fourthdown_model.rds"))
cat("\nSaved fourthdown_model.rds (components + sanity)\n")
