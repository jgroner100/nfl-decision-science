# ============================================================================
# Breakeven_VOI_2pt_Behavioral.R — the BEHAVIORAL 2-pt structure.
# Coaches go for 2 far less than the math says. This re-buckets the model around
# what coaches ACTUALLY do, and treats LINE-UP-AND-DECIDE as the palatable
# substitute for the "should-go-but-won't" cases.
#
# 1. EMPIRICAL go-for-2 rate g_emp(margin, time) from 2017-24 pbp.
# 2. Three-way math optimal (go2 / XP / lineup) recomputed at the CONSERVATIVE
#    boost s = 0.20 (1.2x multiplier; user-requested for this model only).
# 3. Re-bucket the math-GO2 cases by g_emp: a fraction g_emp actually go (-> go2),
#    the rest (1 - g_emp) won't -> the LINE-UP "side bucket".
# 4. VALUE: side-bucket cases currently kick the XP; if they lined up instead they
#    reclaim (lineup_wp - XP_wp) of the (go2_wp - XP_wp) they leave on the table.
# Reuses the saved 2-pt decision set (breakeven_voi_2pt.rds; WPs are s-independent,
# only the lineup branch uses s -> recomputed here). Saves breakeven_voi_2pt_behavioral.rds.
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(nflreadr) })
rds_dir <- file.path(getwd(), "build")
Q_DEF <- 0.50; S_BEH <- 0.20                                   # conservative boost for THIS model
p_boost <- function(p, s) ifelse(p <= 0.5, p*(1+s), p + s*(1-p))
MBREAKS <- c(-Inf,-5,-3,-2,-1,0,1,2,4,Inf)
MLAB <- c("down 5+","down 3-4","down 2","down 1","tied","up 1","up 2","up 3-4","up 5+")
tbin_of <- function(g) case_when(g<=120~"final 2 min", g<=360~"final 6 min", TRUE~"earlier")

# ---- 1. EMPIRICAL go-for-2 rate by score x time (2017-2024) -----------------
message("load_pbp(2017:2024) for empirical go-for-2 rates...")
source("load_play_by_play.R")
pbp <- load_play_by_play(2017:2024)
dec <- pbp %>% filter((two_point_attempt==1 | extra_point_attempt==1), qtr<=4,
                      !is.na(score_differential), !is.na(game_seconds_remaining)) %>%
  mutate(go2 = as.integer(two_point_attempt==1),
         mbin = cut(score_differential, MBREAKS, labels=MLAB),
         tbin = tbin_of(game_seconds_remaining))
cat(sprintf("PAT/2pt decisions 2017-24: %d | OVERALL go-for-2 rate: %.1f%%\n", nrow(dec), 100*mean(dec$go2)))
gtab  <- dec %>% group_by(mbin, tbin) %>% summarise(n=n(), g_emp=mean(go2), .groups="drop")
gmarg <- dec %>% group_by(mbin) %>% summarise(g_emp=mean(go2), .groups="drop")           # fallback
cat("\nempirical go-for-2 rate by margin x time (%):\n")
print(as.data.frame(gtab %>% mutate(g_emp=round(100*g_emp,1)) %>%
  select(mbin,tbin,g_emp) %>% pivot_wider(names_from=tbin, values_from=g_emp)), row.names=FALSE)
g_lookup <- function(m, t){ r <- gtab %>% filter(mbin==m, tbin==t)
  if (nrow(r)==1 && r$n>=30) return(r$g_emp)
  gmarg$g_emp[gmarg$mbin==m] }                                                            # thin cell -> margin-only

# ---- 2. three-way math optimal at s=0.20, on the competitive 2-pt set -------
two <- readRDS(file.path(rds_dir,"breakeven_voi_2pt.rds"))$data %>% filter(competitive)
strat2 <- function(d,q,s){ p<-d$conv_2pt; pb<-p_boost(p,s)
  wp_go2 <- p*d$wp_2+(1-p)*d$wp_0; go2_b <- pb*d$wp_2+(1-pb)*d$wp_0
  cost <- pmin(1,1.5*(1-q))*d$C_pp/100; wp_lin <- q*go2_b+(1-q)*d$wp_go1-cost
  M<-cbind(go2=wp_go2, XP=d$wp_go1, lineup=wp_lin); M[is.na(M)]<- -Inf
  list(wp_go2=wp_go2, wp_xp=d$wp_go1, wp_lineup=wp_lin, winner=c("go2","XP","lineup")[max.col(M,ties.method="first")]) }
r <- strat2(two, Q_DEF, S_BEH)
two <- two %>% mutate(opt=r$winner, wp_go2=r$wp_go2, wp_xp=r$wp_xp, wp_lineup=r$wp_lineup,
                      mbin=cut(score_differential, MBREAKS, labels=MLAB), tbin=tbin_of(game_seconds_remaining))
two$g_emp <- mapply(function(m,t) g_lookup(as.character(m), t), two$mbin, two$tbin)
cat(sprintf("\nPURE-MATH optimal at s=0.20 (n=%d competitive): go2 %.1f%% / lineup %.1f%% / XP %.1f%%\n",
            nrow(two), 100*mean(two$opt=="go2"), 100*mean(two$opt=="lineup"), 100*mean(two$opt=="XP")))

# ---- 3. behavioral re-bucketing (math-GO2 split by g_emp) -------------------
two <- two %>% mutate(
  w_go2     = if_else(opt=="go2", g_emp, 0),            # math says go AND coach actually goes
  w_side    = if_else(opt=="go2", 1-g_emp, 0),          # math says go, coach WON'T -> line-up side bucket
  w_lincore = if_else(opt=="lineup", 1, 0),             # math says line up
  w_xp      = if_else(opt=="XP", 1, 0))
cat("\n=== BEHAVIORAL distribution (s=0.20; math-go2 cases split by empirical go rate) ===\n")
cat(sprintf("  go for 2 (actually do) : %5.1f%%\n", 100*mean(two$w_go2)))
cat(sprintf("  LINE UP - core (math)  : %5.1f%%\n", 100*mean(two$w_lincore)))
cat(sprintf("  LINE UP - side bucket  : %5.1f%%   <- should-go-but-won't, nudged to line up\n", 100*mean(two$w_side)))
cat(sprintf("  LINE UP - TOTAL        : %5.1f%%\n", 100*mean(two$w_lincore + two$w_side)))
cat(sprintf("  kick XP                : %5.1f%%\n", 100*mean(two$w_xp)))

# ---- 4. value: what the side bucket reclaims vs its status-quo XP -----------
two <- two %>% mutate(leak_pp = 100*(wp_go2 - wp_xp), recover_pp = 100*(wp_lineup - wp_xp))
leak      <- sum(two$w_side * two$leak_pp)    / nrow(two)     # pp left on table per competitive 2pt decision
recovered <- sum(two$w_side * pmax(0,two$recover_pp)) / nrow(two)
neg_share <- with(filter(two, w_side>0), 100*mean(recover_pp < 0))   # side cases where lining up < XP
cat("\n=== VALUE (per competitive 2-pt decision, pp) ===\n")
cat(sprintf("  left on the table by NOT going for 2 (status quo)      : %.3f pp\n", leak))
cat(sprintf("  RECLAIMED by lining up instead of kicking (palatable)  : %.3f pp\n", recovered))
cat(sprintf("  recovery fraction (lineup reclaims of the leak)        : %.0f%%\n", 100*recovered/leak))
cat(sprintf("  (side-bucket cases where lining up < XP, kept as kick) : %.1f%%\n", neg_share))

cat("\nside-bucket share + reclaimed value, by post-TD margin:\n")
print(as.data.frame(two %>% group_by(mbin) %>% summarise(n=n(),
    side_pct = round(100*mean(w_side),1),
    reclaimed_pp = round(sum(w_side*pmax(0,recover_pp))/n(),3),
    g_emp = round(100*mean(g_emp),1), .groups="drop")), row.names=FALSE)

saveRDS(list(decisions=two, g_table=gtab, g_margin=gmarg, s=S_BEH, q=Q_DEF,
  behavioral_dist=c(go2=mean(two$w_go2), lineup_core=mean(two$w_lincore),
                    lineup_side=mean(two$w_side), xp=mean(two$w_xp)),
  value=c(leak_pp=leak, recovered_pp=recovered, recovery_frac=recovered/leak),
  method="Behavioral 2-pt: empirical go-for-2 rate g_emp(margin,time) 2017-24; 3-way math optimal at CONSERVATIVE s=0.20; math-go2 cases split by g_emp (g_emp -> go2, 1-g_emp -> line-up side bucket); value = side bucket reclaiming (lineup-XP) of the (go2-XP) left on the table by kicking. Premise (unmeasurable): coaches accept line-up where they reject outright go-for-2.",
  built=Sys.time()), file.path(rds_dir, "breakeven_voi_2pt_behavioral.rds"))
cat("\nSaved breakeven_voi_2pt_behavioral.rds\n")
