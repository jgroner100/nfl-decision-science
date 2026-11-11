# ============================================================================
# nfl4th_Baseline.R — verify the ADOPTED nfl4th baseline end-to-end.
# go/punt/FG WP via nfl4th::add_4th_probs(); 2-pt baseline via add_2pt_probs().
# Both auto-clean raw nflfastR pbp (no `type` col -> prepare_nflfastr_data).
# Uses the locally-cached xgboost models (see nfl4th_Setup.R). Saves a real
# 4th-down go_boost sample for the upcoming bootstrap-CI step.
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(nfl4th); library(nflreadr) })
rds_dir <- file.path(getwd(), "build")
SEASONS <- 2022:2023

message(sprintf("load_pbp(%d:%d)...", min(SEASONS), max(SEASONS)))
source("load_play_by_play.R")
pbp <- load_play_by_play(SEASONS)
cat(sprintf("loaded %d plays\n", nrow(pbp)))

# ---- 4th-down baseline: go / punt / FG ------------------------------------
message("add_4th_probs...")
fourth <- nfl4th::add_4th_probs(pbp) %>%
  filter(down == 4, !is.na(go_boost), !is.na(yardline_100), !is.na(ydstogo), ydstogo >= 1)
cat(sprintf("\n4th-down plays with go_boost (%d-%d): %d\n", min(SEASONS), max(SEASONS), nrow(fourth)))
cat("go_boost (WP percentage points) summary:\n"); print(round(summary(fourth$go_boost), 2))

cat(sprintf("\npoint-estimate GO share (go_boost>0): %.1f%%\n", 100*mean(fourth$go_boost > 0)))
cat("point-estimate toss-up bands (CI-based toss-up = next step):\n")
for (b in c(0.5, 1, 1.5, 2, 4)) cat(sprintf("  |go_boost| < %4.1f pp: %5.1f%%\n", b, 100*mean(abs(fourth$go_boost) < b)))

cat("\nexample 4th downs (WP fractions; go_boost in pp):\n")
set.seed(1)
ex <- fourth %>% sample_n(10) %>%
  transmute(season, qtr, ydstogo, yl=yardline_100, score=score_differential,
            gsr=round(game_seconds_remaining), go_wp=round(go_wp,3), fg_wp=round(fg_wp,3),
            punt_wp=round(punt_wp,3), go_boost=round(go_boost,2), fd_prob=round(first_down_prob,3)) %>%
  arrange(yl)
print(as.data.frame(ex), row.names = FALSE)

# ---- 2-pt baseline: go-for-2 vs kick XP -----------------------------------
message("add_2pt_probs (on real PAT / 2-pt attempts)...")
twopt <- nfl4th::add_2pt_probs(pbp) %>%
  filter(!is.na(wp_go2), !is.na(wp_go1)) %>%
  mutate(reco = if_else(wp_go2 > wp_go1, "GO for 2", "kick XP"))
cat(sprintf("\n2-pt-decision rows scored: %d   |  GO-for-2 share: %.1f%%\n",
            nrow(twopt), 100*mean(twopt$reco == "GO for 2")))
cat("example 2-pt decisions:\n")
set.seed(2)
ex2 <- twopt %>% sample_n(8) %>%
  transmute(qtr, score=score_differential, gsr=round(game_seconds_remaining),
            conv_2pt=round(conv_2pt,3), wp_go2=round(wp_go2,3), wp_go1=round(wp_go1,3), reco) %>%
  arrange(score)
print(as.data.frame(ex2), row.names = FALSE)

# ---- save real-4th-down sample for the bootstrap-CI step ------------------
saveRDS(list(seasons = SEASONS,
  fourth = fourth %>% transmute(game_id, play_id, season, qtr, game_seconds_remaining,
            yardline_100, ydstogo, score_differential, posteam, posteam_timeouts_remaining,
            defteam_timeouts_remaining, go_wp, fg_wp, punt_wp, first_down_prob,
            wp_fail, wp_succeed, fg_make_prob, go_boost),
  built = Sys.time()),
  file.path(rds_dir, "nfl4th_baseline_sample.rds"))
cat("\nSaved nfl4th_baseline_sample.rds\n")
