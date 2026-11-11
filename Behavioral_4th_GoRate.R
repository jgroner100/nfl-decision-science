# ============================================================================
# Behavioral_4th_GoRate.R  -- empirical 4th-down GO rate g_emp(state), the analog
# of the 2-pt g_emp, for the behavioral 4th-down baseline (audit fix A).
#
# RESPONSE = effective_go (decision 1 = option (i): override baked into ONE rate):
#     effective_go = 1 if (team actually went) OR (nfl4th go_boost >= +1.5 pp)
#   Rationale: a 4th down where going beats the best kick by >= 1.5 pp WP and the
#   team kicks anyway is a clear coaching error; counting it as a non-go would
#   artificially depress the empirical go rate and over-credit the line-up reclaim.
#
# TWO ESTIMATORS produced (Phase-2 season total computed BOTH ways = sensitivity):
#   (A) logistic GAM  effective_go ~ te(ydstogo,yardline_100) + s(score_diff)
#        + s(gsr) + s(season) + ti(ydstogo,score_diff) + ti(ydstogo,gsr)
#        on 2017-24, EVALUATED at each decision's own season. The ti() terms add
#        the distance x score / distance x time interactions the additive model
#        lacked (cross-check showed 15-36 pp gaps in late/leading states).
#   (B) raw-binned effective-go rate (2021-24) over ytg x yardline x score x time
#        with a coarsening fallback (n>=40) -- a dumb estimator to test (A)'s
#        smoothing/interaction assumptions.
#
# Output behavioral_4th_gorate.rds: the bam model, raw-bin tables, and a per-play
# key (game_id, play_id, g_hat, g_raw) for the 2022-23 breakeven set.
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(mgcv); library(nfl4th); library(nflreadr) })
rds_dir <- file.path(getwd(), "build")
GB_THRESH <- 1.5   # the "clear-cut go" override threshold, pp
RAW_MIN   <- 40    # min n for a raw-binned cell before coarsening
wilson <- function(x, n){ if(n==0) return(c(NA,NA)); p<-x/n; z<-1.96
  d<-1+z^2/n; c(lo=(p+z^2/(2*n)-z*sqrt(p*(1-p)/n+z^2/(4*n^2)))/d,
                hi=(p+z^2/(2*n)+z*sqrt(p*(1-p)/n+z^2/(4*n^2)))/d) }
# identical binning used for the raw estimator (history) and the breakeven set (apply)
binit <- function(df) df %>% mutate(
  b_ytg = cut(ydstogo, c(0,1,2,3,5,8,99), labels=c("1","2","3","4-5","6-8","9+")),
  b_yl  = cut(yardline_100, c(0,20,40,60,80,100),
              labels=c("opp1-20","opp21-40","mid41-60","own61-80","own81-99")),
  b_sd  = cut(score_differential, c(-Inf,-8,-3,-0.5,0.5,3,8,Inf),
              labels=c("d9+","d4-8","d1-3","tied","u1-3","u4-8","u9+")),
  b_t   = cut(game_seconds_remaining, c(-Inf,120,360,Inf), labels=c("2min","6min","earlier")))

# ---- 1. historical 4th-down decisions 2017-24 with nfl4th go_boost ----------
message("load_pbp(2017:2024)...")
source("load_play_by_play.R")
pbp <- load_play_by_play(2017:2024) %>% filter(season_type=="REG")
message("add_4th_probs over 2017-24 4th downs (xgboost; ~1-2 min)...")
f4 <- nfl4th::add_4th_probs(pbp)
dec <- f4 %>%
  filter(down==4, !is.na(go_boost), !is.na(play_type),
         play_type %in% c("run","pass","punt","field_goal"),
         !is.na(ydstogo), !is.na(yardline_100), !is.na(score_differential),
         !is.na(game_seconds_remaining), qtr<=4) %>%
  mutate(actual_go   = as.integer(play_type %in% c("run","pass")),
         clearcut_go = as.integer(go_boost >= GB_THRESH),
         effective_go= as.integer(actual_go==1 | clearcut_go==1),
         season      = as.integer(season))
cat(sprintf("\n4th-down decisions (REG, 2017-24, real go/punt/FG): %d\n", nrow(dec)))
cat(sprintf("  raw go rate %.1f%% | clear-cut (gb>=%.1f) %.1f%% | EFFECTIVE go %.1f%% | of clear-cut, %% kicked %.1f%%\n",
            100*mean(dec$actual_go), GB_THRESH, 100*mean(dec$clearcut_go),
            100*mean(dec$effective_go), 100*with(filter(dec,clearcut_go==1), mean(actual_go==0))))

# ---- 2. era trend ----------------------------------------------------------
era <- dec %>% group_by(season) %>%
  summarise(n=n(), raw_go=round(100*mean(actual_go),1), eff_go=round(100*mean(effective_go),1),
            clearcut=round(100*mean(clearcut_go),1), .groups="drop")
cat("\n=== ERA TREND: go rate by season ===\n"); print(as.data.frame(era), row.names=FALSE)

# ---- 3A. GAM with interactions, fit at EACH threshold (for the season sensitivity) ----
fit_gam <- function(resp){ dd <- dec; dd$resp <- resp
  bam(resp ~ te(ydstogo, yardline_100, k=c(5,6)) + s(score_differential, k=8) +
        s(game_seconds_remaining, k=8) + s(season, k=6) +
        ti(ydstogo, score_differential, k=c(5,5)) + ti(ydstogo, game_seconds_remaining, k=c(5,5)),
      data=dd, family=binomial, method="fREML", discrete=TRUE) }
message("\nfitting effective_go GAMs (bam, binomial, + ti interactions) at thresholds 1.0/1.5/2.0...")
gam10 <- fit_gam(as.integer(dec$actual_go==1 | dec$go_boost>=1.0))
g_gam <- fit_gam(dec$effective_go)                               # the main (1.5) model
gam20 <- fit_gam(as.integer(dec$actual_go==1 | dec$go_boost>=2.0))
dec$g_hat <- as.numeric(predict(g_gam, type="response"))
cat(sprintf("GAM(1.5) in-sample mean predicted = %.3f vs empirical %.3f (calibration)\n",
            mean(dec$g_hat), mean(dec$effective_go)))

# ---- 3B. raw-binned estimator (2021-24) with coarsening fallback -----------
hist_b <- binit(dec %>% filter(season>=2021))
c4 <- hist_b %>% group_by(b_ytg,b_yl,b_sd,b_t) %>% summarise(g4=mean(effective_go), n4=n(), .groups="drop")
c3 <- hist_b %>% group_by(b_ytg,b_yl,b_sd)      %>% summarise(g3=mean(effective_go), n3=n(), .groups="drop")
c2 <- hist_b %>% group_by(b_ytg,b_yl)           %>% summarise(g2=mean(effective_go), n2=n(), .groups="drop")
c1 <- hist_b %>% group_by(b_ytg)                %>% summarise(g1=mean(effective_go), n1=n(), .groups="drop")
raw_lookup <- function(df){ b <- binit(df) %>%
    left_join(c4,by=c("b_ytg","b_yl","b_sd","b_t")) %>% left_join(c3,by=c("b_ytg","b_yl","b_sd")) %>%
    left_join(c2,by=c("b_ytg","b_yl")) %>% left_join(c1,by="b_ytg")
  dplyr::case_when(b$n4>=RAW_MIN ~ b$g4, b$n3>=RAW_MIN ~ b$g3, b$n2>=RAW_MIN ~ b$g2, TRUE ~ b$g1) }

# ---- 4. effective-go cells, n + Wilson CI ----------------------------------
cat("\n=== EFFECTIVE-go rate by ydstogo x yardline_100 (raw cells; n + 95% Wilson CI) ===\n")
cell <- dec %>%
  mutate(ytg=cut(ydstogo,c(0,1,2,3,5,8,99),labels=c("1","2","3","4-5","6-8","9+")),
         yl =cut(yardline_100,c(0,10,20,40,60,80,100),
                 labels=c("opp1-10","opp11-20","opp21-40","mid41-60","own61-80","own81-99"))) %>%
  group_by(ytg,yl) %>% summarise(n=n(), eff=mean(effective_go), .groups="drop") %>%
  rowwise() %>% mutate(lo=wilson(round(eff*n),n)["lo"], hi=wilson(round(eff*n),n)["hi"]) %>% ungroup() %>%
  mutate(eff_go=sprintf("%.0f%% [%.0f,%.0f] (n=%d)",100*eff,100*lo,100*hi,n))
print(as.data.frame(cell %>% select(ytg,yl,eff_go) %>% pivot_wider(names_from=yl,values_from=eff_go)), row.names=FALSE)

# ---- 5. apply BOTH estimators to the 2022-23 breakeven set -----------------
d4 <- readRDS(file.path(rds_dir,"breakeven_voi.rds"))$data
d4$g_hat_10 <- as.numeric(predict(gam10, newdata=d4, type="response"))
d4$g_hat_15 <- as.numeric(predict(g_gam, newdata=d4, type="response"))
d4$g_hat_20 <- as.numeric(predict(gam20, newdata=d4, type="response"))
d4$g_hat    <- d4$g_hat_15                                       # main = 1.5
d4$g_raw    <- raw_lookup(d4)                                    # raw-binned at 1.5 (estimator sensitivity)
gkey <- d4 %>% select(game_id, play_id, g_hat_10, g_hat_15, g_hat_20, g_raw)
comp <- d4 %>% filter(competitive)
cat(sprintf("\n=== ON THE 2022-23 BREAKEVEN SET (competitive n=%d) ===\n", nrow(comp)))
cat(sprintf("  forced-go (go_boost>=%.1f): %.1f%% | mean g_hat(GAM) %.1f%% | mean g_raw(binned) %.1f%% | cor %.3f\n",
            GB_THRESH, 100*mean(comp$go_boost>=GB_THRESH), 100*mean(comp$g_hat),
            100*mean(comp$g_raw), cor(comp$g_hat, comp$g_raw, use="complete.obs")))
cat("\nmean g_emp by ydstogo on competitive set (GAM vs raw-binned):\n")
print(as.data.frame(comp %>% mutate(ytg=cut(ydstogo,c(0,1,2,3,5,8,99),labels=c("1","2","3","4-5","6-8","9+"))) %>%
  group_by(ytg) %>% summarise(n=n(), forced_go=round(100*mean(go_boost>=GB_THRESH),1),
    gam=round(100*mean(g_hat),1), raw=round(100*mean(g_raw),1), .groups="drop")), row.names=FALSE)

# ---- 6. cross-check: GAM(+ti) vs raw-binned vs raw 2022-24 windows ----------
cat("\n=== CROSS-CHECK: GAM(+interactions, eval 2023) vs raw-binned vs raw 2022-24 window ===\n")
win <- dec %>% filter(season>=2022)
probe <- tribble(~lab,~ytg,~yl,~sd,~gsr,
  "4th-&-2 opp35 tied Q3",2,35,0,1500, "4th-&-5 mid50 tied Q3",5,50,0,1500,
  "4th-&-1 own40 tied Q2",1,60,0,2400, "4th-&-8 opp30 down4 Q4",8,30,-4,600,
  "4th-&-4 opp40 up3 Q4",4,40,3,600)
pnd <- probe %>% transmute(ydstogo=ytg, yardline_100=yl, score_differential=sd,
                           game_seconds_remaining=gsr, season=2023L)
probe$gam <- as.numeric(predict(g_gam, newdata=pnd, type="response"))
probe$raw_bin <- raw_lookup(pnd)
probe$raw_win <- mapply(function(yt,yl,sd,gsr){ sub<-win %>% filter(abs(ydstogo-yt)<=1, abs(yardline_100-yl)<=8,
    abs(score_differential-sd)<=4, abs(game_seconds_remaining-gsr)<=300); if(nrow(sub)<20) NA_real_ else mean(sub$effective_go) },
    probe$ytg,probe$yl,probe$sd,probe$gsr)
probe$win_n <- mapply(function(yt,yl,sd,gsr) nrow(win %>% filter(abs(ydstogo-yt)<=1, abs(yardline_100-yl)<=8,
    abs(score_differential-sd)<=4, abs(game_seconds_remaining-gsr)<=300)), probe$ytg,probe$yl,probe$sd,probe$gsr)
print(as.data.frame(probe %>% transmute(lab, gam=round(100*gam,1), raw_bin=round(100*raw_bin,1),
                    raw_win=round(100*raw_win,1), win_n)), row.names=FALSE)

# ---- save ------------------------------------------------------------------
saveRDS(list(model=g_gam, gb_thresh=GB_THRESH, raw_min=RAW_MIN,
  raw_cells=list(c4=c4,c3=c3,c2=c2,c1=c1), gkey=gkey, era=era, cells=cell,
  overall=c(n=nrow(dec), raw_go=mean(dec$actual_go), clearcut=mean(dec$clearcut_go), eff_go=mean(dec$effective_go)),
  method=paste("Behavioral 4th-down go rate g_emp(state). effective_go = actual_go OR go_boost>=1.5 (override",
    "baked into one rate). GAM(A): te(ytg,yl)+s(score)+s(gsr)+s(season)+ti(ytg,score)+ti(ytg,gsr), 2017-24 REG,",
    "eval at each decision's season. Raw-binned(B): ytg x yl x score x time over 2021-24, fallback n>=40.",
    "gkey carries g_hat(GAM) and g_raw(binned) for the 2022-23 breakeven set."),
  built=Sys.time()), file.path(rds_dir,"behavioral_4th_gorate.rds"))
cat("\nSaved behavioral_4th_gorate.rds\n")
