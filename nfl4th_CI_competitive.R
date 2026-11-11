# ============================================================================
# nfl4th_CI_competitive.R — re-report the go_boost toss-up share on COMPETITIVE
# 4th downs only (offense pre-snap vegas_wp in [0.10,0.90]), stripping garbage
# time where go_boost ~ 0 only because the game is already decided. Mirrors the
# 3rd-down blowout filter, now on the eval set. Augments nfl4th_ci_full.rds with
# vegas_wp + a `competitive` flag.
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(nflreadr) })
rds_dir <- file.path(getwd(), "build")
full <- readRDS(file.path(rds_dir, "nfl4th_ci_full.rds")); ci <- full$ci
message(sprintf("load_pbp(%s) for pre-snap vegas_wp...", paste(range(full$seasons), collapse=":")))
source("load_play_by_play.R")
pbp <- load_play_by_play(full$seasons)
ci <- ci %>% left_join(pbp %>% transmute(game_id, play_id, vegas_wp), by = c("game_id","play_id")) %>%
  mutate(competitive = !is.na(vegas_wp) & vegas_wp >= 0.10 & vegas_wp <= 0.90)
cat(sprintf("matched vegas_wp: %d/%d (%d NA)\n", sum(!is.na(ci$vegas_wp)), nrow(ci), sum(is.na(ci$vegas_wp))))
cat(sprintf("competitive (vegas_wp in [0.10,0.90]): %d (%.1f%%);  garbage/NA: %d\n",
            sum(ci$competitive), 100*mean(ci$competitive), sum(!ci$competitive)))

summ <- function(d, lab) cat(sprintf("  %-16s n=%5d | toss-up: conv-only %4.1f%%  combined %4.1f%% | med half %.2f pp\n",
            lab, nrow(d), 100*mean(d$tossup_conv), 100*mean(d$tossup_comb), median(d$half_comb)))
cat("\n=== toss-up share: ALL vs COMPETITIVE vs GARBAGE ===\n")
summ(ci, "all 4th downs"); summ(filter(ci, competitive), "competitive"); summ(filter(ci, !competitive), "garbage/NA")

cat("\nby ydstogo (COMPETITIVE only), combined CI:\n")
print(as.data.frame(ci %>% filter(competitive) %>%
  mutate(ytg = cut(ydstogo, c(0,1,2,3,5,8,99), labels=c("1","2","3","4-5","6-8","9+"))) %>%
  group_by(ytg) %>% summarise(n=n(), med_half=round(median(half_comb),2),
    tossup=round(100*mean(tossup_comb),1), .groups="drop")), row.names=FALSE)

# how much of the all-plays toss-up set was garbage-time?
tu_all <- ci %>% filter(tossup_comb)
cat(sprintf("\nof the %d combined toss-ups, %.1f%% are in garbage/NA states (go_boost~0 because the game is decided)\n",
            nrow(tu_all), 100*mean(!tu_all$competitive)))

full$ci <- ci
full$method <- paste0(full$method, " | eval augmented: pre-snap vegas_wp + `competitive` flag (offense vegas_wp in [0.10,0.90]).")
saveRDS(full, file.path(rds_dir, "nfl4th_ci_full.rds"))
cat("\nUpdated nfl4th_ci_full.rds (added vegas_wp + competitive).\n")
