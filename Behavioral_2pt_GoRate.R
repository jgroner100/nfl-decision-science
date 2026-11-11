# ============================================================================
# Behavioral_2pt_GoRate.R -- empirical go-for-2 rate g_emp2(margin,time) WITH the
# same +1.5 pp override as the 4th-down leg (decision 6, methodology symmetry).
#   effective_go2 = actual_go2 OR (wp_go2 - wp_go1 >= thr pp)
# Supersedes the override-free g_emp inside Breakeven_VOI_2pt_Behavioral.R. Built
# for thresholds {1.0, 1.5, 2.0} so the season recompute can run the same threshold
# sensitivity as the 4th-down leg. 2-pt's state space is small + well-sampled, so a
# (margin x time) bin table is adequate (no GAM needed, unlike 4th down).
# Output: behavioral_2pt_gorate.rds (g_table + g_margin per threshold, era trend).
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(nfl4th); library(nflreadr) })
rds_dir <- file.path(getwd(), "build")
THRS <- c(1.0, 1.5, 2.0)
MBREAKS <- c(-Inf,-5,-3,-2,-1,0,1,2,4,Inf)
MLAB <- c("down 5+","down 3-4","down 2","down 1","tied","up 1","up 2","up 3-4","up 5+")
tbin_of <- function(g) case_when(g<=120~"final 2 min", g<=360~"final 6 min", TRUE~"earlier")

message("load_pbp(2017:2024) + add_2pt_probs...")
source("load_play_by_play.R")
pbp <- load_play_by_play(2017:2024) %>% filter(season_type=="REG")
two <- nfl4th::add_2pt_probs(pbp) %>%
  filter((two_point_attempt==1 | extra_point_attempt==1), qtr<=4,
         !is.na(wp_go2), !is.na(wp_go1), !is.na(score_differential), !is.na(game_seconds_remaining)) %>%
  mutate(actual_go2 = as.integer(two_point_attempt==1),
         gb2 = 100*(wp_go2 - wp_go1),
         mbin = cut(score_differential, MBREAKS, labels=MLAB),
         tbin = tbin_of(game_seconds_remaining),
         season = as.integer(season))
cat(sprintf("PAT/2pt decisions 2017-24: %d | raw go-for-2 %.1f%% | overall mean gb2 %.2f pp\n",
            nrow(two), 100*mean(two$actual_go2), mean(two$gb2)))

cat("\n=== ERA TREND go-for-2 (raw vs effective@1.5) + clear-cut share ===\n")
print(as.data.frame(two %>% group_by(season) %>% summarise(n=n(),
  raw=round(100*mean(actual_go2),1), eff15=round(100*mean(actual_go2==1 | gb2>=1.5),1),
  clearcut15=round(100*mean(gb2>=1.5),1), .groups="drop")), row.names=FALSE)

tables <- list()
for (thr in THRS){
  two$eff <- as.integer(two$actual_go2==1 | two$gb2 >= thr)
  gtab  <- two %>% group_by(mbin,tbin) %>% summarise(n=n(), g_emp=mean(eff), .groups="drop")
  gmarg <- two %>% group_by(mbin) %>% summarise(g_emp=mean(eff), .groups="drop")
  tables[[sprintf("thr_%.1f", thr)]] <- list(g_table=gtab, g_margin=gmarg,
    overall=c(raw=mean(two$actual_go2), clearcut=mean(two$gb2>=thr), eff=mean(two$eff)))
  cat(sprintf("\nthreshold %.1f pp: clear-cut %.1f%% | EFFECTIVE go-for-2 %.1f%% (raw %.1f%%)\n",
              thr, 100*mean(two$gb2>=thr), 100*mean(two$eff), 100*mean(two$actual_go2)))
}
cat("\neffective go-for-2 @1.5 by margin x time (%):\n")
print(as.data.frame(tables[["thr_1.5"]]$g_table %>% mutate(g_emp=round(100*g_emp,1)) %>%
  select(mbin,tbin,g_emp) %>% pivot_wider(names_from=tbin, values_from=g_emp)), row.names=FALSE)

saveRDS(list(thresholds=THRS, tables=tables,
  era=two %>% group_by(season) %>% summarise(n=n(), raw=mean(actual_go2), eff15=mean(actual_go2==1|gb2>=1.5), .groups="drop"),
  method=paste("Empirical go-for-2 rate g_emp2(margin,time) with +thr pp override:",
    "effective_go2 = actual_go2 OR (wp_go2-wp_go1>=thr). nfl4th add_2pt_probs, 2017-24 REG. Thresholds",
    "1.0/1.5/2.0. Replaces the override-free g_emp in Breakeven_VOI_2pt_Behavioral.R for the unified",
    "core+reclaim season recompute."),
  built=Sys.time()), file.path(rds_dir,"behavioral_2pt_gorate.rds"))
cat("\nSaved behavioral_2pt_gorate.rds\n")
