# ============================================================================
# Breakeven_VOI_Season_Behavioral.R -- UNIFIED behavioral season total (audit
# fix A + D2). Replaces the old season leg that summed two different baselines
# (4th = value-vs-OPTIMAL option value; 2pt = value-vs-ACTUAL behavioral reclaim).
# Now BOTH legs are one coherent quantity = value vs WHAT TEAMS ACTUALLY DO, via
# the same core+reclaim formula:
#
#   per decision, with go option wp_go, kick option kick_wp, line-up wp_lin,
#   nfl4th go_boost gb, empirical effective-go rate g = g_emp(state):
#     if gb >= thr  : forced-go (clear-cut). baseline = go. val = max(0, lin - go)   [core only]
#     else          : val = g*max(0, lin - go) + (1-g)*max(0, lin - kick)
#                            \___ core (would-go) ___/   \___ reclaim (reluctant) __/
#
# s=0.20, q=0.50, 0.1-pp floor on the per-decision value. 2-pt keeps the
# time-decaying LEAD CAP (+14 1H / +10 Q3 / +7 Q4). 4th-down g from
# behavioral_4th_gorate.rds (GAM g_hat + raw-binned g_raw = sensitivity); 2-pt g
# from behavioral_2pt_gorate.rds. Threshold sensitivity at gb >= {1.0,1.5,2.0}.
# Reports old-vs-new, GAM-vs-raw, ex-(2pt up-5+). -> breakeven_voi_season_behavioral.rds
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse) })
rds_dir <- file.path(getwd(), "build")
S <- 0.20; Q <- 0.50; FLOOR <- 0.10; THR_MAIN <- 1.5
p_boost <- function(p,s) ifelse(p <= 0.5, p*(1+s), p + s*(1-p))
wk <- function(gid) suppressWarnings(as.integer(str_split_fixed(gid,"_",4)[,2]))
MBREAKS <- c(-Inf,-5,-3,-2,-1,0,1,2,4,Inf)
MLAB <- c("down 5+","down 3-4","down 2","down 1","tied","up 1","up 2","up 3-4","up 5+")
tbin_of <- function(g) case_when(g<=120~"final 2 min", g<=360~"final 6 min", TRUE~"earlier")
cap_of  <- function(g) case_when(g > 1800 ~ 14, g > 900 ~ 10, TRUE ~ 7)

# ============================ 4th-down leg ==================================
g4o <- readRDS(file.path(rds_dir,"behavioral_4th_gorate.rds"))
d4 <- readRDS(file.path(rds_dir,"breakeven_voi.rds"))$data %>% mutate(week=wk(game_id)) %>% filter(week<=18)
d4 <- d4 %>% left_join(g4o$gkey, by=c("game_id","play_id"))
n_ts <- n_distinct(paste(d4$season, d4$posteam))
p<-d4$first_down_prob; pb<-p_boost(p,S)
d4$wp_go <- p*d4$wp_succeed+(1-p)*d4$wp_fail
go_b     <- pb*d4$wp_succeed+(1-pb)*d4$wp_fail
d4$lin   <- Q*go_b + (1-Q)*d4$kick_wp - pmin(1,1.5*(1-Q))*d4$C_pp/100
best <- pmax(d4$wp_go, d4$fg_wp, d4$punt_wp, na.rm=TRUE)             # OLD vs-optimal baseline
d4$val_old <- { g<-100*(d4$lin-best); ifelse(g>=FLOOR,g,0) }
# CLEAN decomposition (no spurious "fix-bad-gos" credit; see header):
#   core    = max(0, lin - best_outright)         [credited to ALL; = OLD vs-optimal]
#   reclaim = (1-g)*max(0, min(lin,go) - kick)    [reluctant fraction; only where go>kick]
#   forced-go (gb>=thr): reclaim=0, core=max(0,lin-go)  (best_outright=go there)
d4$best_out <- pmax(d4$wp_go, d4$kick_wp, na.rm=TRUE)
value4 <- function(g, thr){
  core    <- pmax(0, d4$lin - d4$best_out)
  reclaim <- ifelse(d4$go_boost>=thr, 0, (1-g)*pmax(0, pmin(d4$lin, d4$wp_go) - d4$kick_wp))
  vpp <- 100*(core + reclaim); ifelse(vpp>=FLOOR, vpp, 0) }
gcol <- function(thr) c("1"="g_hat_10","1.5"="g_hat_15","2"="g_hat_20")[as.character(thr)]

# ============================ 2-pt leg ======================================
g2o <- readRDS(file.path(rds_dir,"behavioral_2pt_gorate.rds"))
beh <- readRDS(file.path(rds_dir,"breakeven_voi_2pt_behavioral.rds"))      # OLD override-free g_emp
d2 <- readRDS(file.path(rds_dir,"breakeven_voi_2pt.rds"))$data %>% mutate(week=wk(game_id)) %>% filter(week<=18)
p<-d2$conv_2pt; pb<-p_boost(p,S)
d2$wp_go2 <- p*d2$wp_2+(1-p)*d2$wp_0
go2b      <- pb*d2$wp_2+(1-pb)*d2$wp_0
d2$lin2   <- Q*go2b + (1-Q)*d2$wp_go1 - pmin(1,1.5*(1-Q))*d2$C_pp/100
d2$xp <- d2$wp_go1; d2$gb2 <- 100*(d2$wp_go2 - d2$xp)
d2$mbin <- as.character(cut(d2$score_differential, MBREAKS, labels=MLAB))
d2$tbin <- tbin_of(d2$game_seconds_remaining)
d2$cap <- cap_of(d2$game_seconds_remaining); d2$in_play <- d2$score_differential <= d2$cap
g2lookup <- function(thr){ tb <- g2o$tables[[sprintf("thr_%.1f",thr)]]
  gt <- tb$g_table %>% mutate(mbin=as.character(mbin)); gm <- tb$g_margin %>% mutate(mbin=as.character(mbin))
  mapply(function(m,t){ r<-gt %>% filter(mbin==m,tbin==t); if(nrow(r)==1 && r$n>=30) return(r$g_emp)
    v<-gm$g_emp[gm$mbin==m]; if(length(v)) v else 0 }, d2$mbin, d2$tbin) }
d2$best_out2 <- pmax(d2$wp_go2, d2$xp, na.rm=TRUE)
value2 <- function(thr){ g<-g2lookup(thr)
  core    <- pmax(0, d2$lin2 - d2$best_out2)
  reclaim <- ifelse(d2$gb2>=thr, 0, (1-g)*pmax(0, pmin(d2$lin2, d2$wp_go2) - d2$xp))
  vpp <- 100*(core + reclaim); ifelse(d2$in_play & vpp>=FLOOR, vpp, 0) }

# OLD 2-pt (side-bucket only, override-free) to reproduce the 8.22 baseline -----
gto<-beh$g_table %>% mutate(mbin=as.character(mbin)); gmo<-beh$g_margin %>% mutate(mbin=as.character(mbin))
g_old<-mapply(function(m,t){ r<-gto %>% filter(mbin==m,tbin==t); if(nrow(r)==1 && r$n>=30) return(r$g_emp)
  v<-gmo$g_emp[gmo$mbin==m]; if(length(v)) v else 0 }, d2$mbin, d2$tbin)
Mopt <- cbind(go2=d2$wp_go2, XP=d2$xp, lineup=d2$lin2); Mopt[is.na(Mopt)]<- -Inf
opt2 <- c("go2","XP","lineup")[max.col(Mopt, ties.method="first")]
reclaim_o <- 100*(d2$lin2-d2$xp); w_side_o <- ifelse(opt2=="go2", 1-g_old, 0)
d2$val2_old <- ifelse(d2$in_play & reclaim_o>=FLOOR, w_side_o*reclaim_o, 0)

# ============================ totals ========================================
v4_old <- sum(d4$val_old)/n_ts; v2_old <- sum(d2$val2_old)/n_ts
v4_gam <- sum(value4(d4[[gcol(THR_MAIN)]], THR_MAIN))/n_ts
v4_raw <- sum(value4(d4$g_raw,             THR_MAIN))/n_ts
v2_new <- sum(value2(THR_MAIN))/n_ts
cat(sprintf("team-seasons %d | 4th downs %d (%.0f/ts) | 2-pt %d (%.0f/ts)\n",
            n_ts, nrow(d4), nrow(d4)/n_ts, nrow(d2), nrow(d2)/n_ts))
cat("\n=== SEASON WP VALUE per team per REG season (pp), threshold 1.5 ===\n")
cat(sprintf("              %12s %12s %12s\n","OLD","NEW(GAM)","NEW(raw-bin)"))
cat(sprintf("  4th downs : %12.2f %12.2f %12.2f   (old=vs-optimal; new=behavioral core+reclaim)\n", v4_old, v4_gam, v4_raw))
cat(sprintf("  2-pt      : %12.2f %12.2f %12s   (old=side-bucket only; new=+core+override)\n", v2_old, v2_new, "-"))
cat(sprintf("  --------------------------------------------------------\n"))
cat(sprintf("  TOTAL     : %12.2f %12.2f %12.2f   (~%.3f / ~%.3f wins)\n",
            v4_old+v2_old, v4_gam+v2_new, v4_raw+v2_new, (v4_gam+v2_new)/100, (v4_raw+v2_new)/100))

# ex-(2pt up-5+) ------------------------------------------------------------
v2_new_ex <- sum(value2(THR_MAIN)[d2$mbin!="up 5+"])/n_ts
cat(sprintf("\n  ex-(2pt up-5+): 2-pt %.2f -> total NEW(GAM) %.2f (~%.3f wins) | NEW(raw) %.2f\n",
            v2_new_ex, v4_gam+v2_new_ex, (v4_gam+v2_new_ex)/100, v4_raw+v2_new_ex))

# 4th-down core vs reclaim decomposition (GAM, thr 1.5) ----------------------
g<-d4[[gcol(THR_MAIN)]]; thr<-THR_MAIN
forced <- d4$go_boost>=thr
core_pp    <- 100*pmax(0, d4$lin - d4$best_out)
reclaim_pp <- 100*ifelse(forced, 0, (1-g)*pmax(0, pmin(d4$lin, d4$wp_go) - d4$kick_wp))
val_pp     <- value4(g, thr)
cat(sprintf("\n4th-down NEW(GAM) decomposition (pre-floor): forced-go %.1f%% of decisions | core %.2f pp/ts (=OLD) + reclaim %.2f pp/ts = %.2f; floored total %.2f\n",
            100*mean(forced), sum(core_pp)/n_ts, sum(reclaim_pp)/n_ts,
            (sum(core_pp)+sum(reclaim_pp))/n_ts, sum(val_pp)/n_ts))
cat("\n4th-down NEW(GAM) value by ydstogo (pp/team-season):\n")
print(as.data.frame(tibble(ytg=cut(d4$ydstogo,c(0,1,2,3,5,8,99),labels=c("1","2","3","4-5","6-8","9+")),
  v=val_pp, vo=d4$val_old) %>% group_by(ytg) %>%
  summarise(NEW=round(sum(v)/n_ts,3), OLD=round(sum(vo)/n_ts,3), .groups="drop")), row.names=FALSE)

# threshold sensitivity -----------------------------------------------------
cat("\n=== THRESHOLD SENSITIVITY (gb>= 1.0 / 1.5 / 2.0 pp), pp/team-season ===\n")
sens <- map_dfr(c(1.0,1.5,2.0), function(thr){
  v4 <- sum(value4(d4[[gcol(thr)]], thr))/n_ts; v2 <- sum(value2(thr))/n_ts
  tibble(threshold=thr, fourth_GAM=round(v4,2), two_pt=round(v2,2), total=round(v4+v2,2),
         forced_go_pct=round(100*mean(d4$go_boost>=thr),1)) })
print(as.data.frame(sens), row.names=FALSE)

# 2-pt by margin (NEW unified vs OLD side-bucket) ---------------------------
cat("\n2-pt value by post-TD margin: NEW(unified, override+core) vs OLD(side-bucket), pp/team-season:\n")
v2v <- value2(THR_MAIN)
print(as.data.frame(tibble(mbin=factor(d2$mbin,levels=MLAB), NEW=v2v, OLD=d2$val2_old) %>%
  group_by(mbin) %>% summarise(NEW=round(sum(NEW)/n_ts,3), OLD=round(sum(OLD)/n_ts,3), .groups="drop")), row.names=FALSE)

saveRDS(list(s=S, q=Q, floor=FLOOR, thr_main=THR_MAIN, team_seasons=n_ts,
  per_team_season_pp=list(
    old=c(fourth=v4_old, two_pt=v2_old, total=v4_old+v2_old),
    new_gam=c(fourth=v4_gam, two_pt=v2_new, total=v4_gam+v2_new, total_ex_up5=v4_gam+v2_new_ex),
    new_raw=c(fourth=v4_raw, two_pt=v2_new, total=v4_raw+v2_new)),
  threshold_sensitivity=sens,
  method=paste("UNIFIED behavioral season total (fix A/D2). Both legs = value vs ACTUAL behavior via",
    "core+reclaim: if go_boost>=thr forced-go (val=max(0,lin-go)); else g*max(0,lin-go)+(1-g)*max(0,lin-kick).",
    "g = effective-go rate (override baked in). 4th g: GAM(g_hat) + raw-binned(g_raw) sensitivity. 2-pt g:",
    "(margin,time) bins with same override. s=0.20 q=0.50, 0.1pp floor, 2-pt time-decaying lead cap. REG",
    "week<=18 / team-seasons. Threshold sensitivity 1.0/1.5/2.0."),
  built=Sys.time()), file.path(rds_dir,"breakeven_voi_season_behavioral.rds"))
cat("\nSaved breakeven_voi_season_behavioral.rds\n")
