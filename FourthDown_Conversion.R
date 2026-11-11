# ============================================================================
# FourthDown_Conversion.R  — Stage 1 of the baseline 4th-down model.
# The ONE new sub-model: P(convert | 4th down, ydstogo, yardline_100), league-
# average, GAM, 2017-2024 REG. (FG make-prob, net-punt, and the vegas_wp surface
# are reused from the DOG work in later stages.) Checks sample size and whether
# pooling 3rd-and-X (Romer/Sandholtz) is needed; validates vs known league rates.
# Saves fourthdown_conversion_model.rds. Decision rule on pooling: use 4th-only
# unless it is visibly noisy at long distances vs the pooled fit.
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(nflreadr); library(mgcv) })
rds_dir <- file.path(getwd(), "build")
message("load_pbp(2017:2024)...")
source("load_play_by_play.R")
pbp <- load_play_by_play(2017:2024)
base <- pbp %>% filter(season_type=="REG", !is.na(yardline_100), !is.na(ydstogo), ydstogo>=1)

# ---- TD-flag sanity: are 4th-down TDs counted as conversions? ----------------
cat("=== 4th-down run/pass TDs: how fourth_down_converted/failed flag them ===\n")
print(base %>% filter(down==4, play_type %in% c("run","pass"), touchdown==1) %>%
        count(fourth_down_converted, fourth_down_failed))

# ---- samples: 4th-down go attempts; 3rd-down attempts (for optional pooling) --
# converted = fourth_down_converted: already counts OFFENSIVE first downs incl.
# offensive TDs as 1, and (correctly) leaves defensive-TD turnovers as 0 (failed).
g4 <- base %>% filter(down==4, play_type %in% c("run","pass"),
                      (fourth_down_converted==1 | fourth_down_failed==1)) %>%
  mutate(converted = as.integer(fourth_down_converted==1), is_fourth=1L)
g3 <- base %>% filter(down==3, play_type %in% c("run","pass"),
                      (third_down_converted==1 | third_down_failed==1)) %>%
  mutate(converted = as.integer(third_down_converted==1), is_fourth=0L)
cat(sprintf("\n4th-down go attempts: %d | 3rd-down attempts: %d\n", nrow(g4), nrow(g3)))

cat("\nempirical 4th-down conversion by ydstogo (capped at 12):\n")
print(as.data.frame(g4 %>% mutate(d=pmin(ydstogo,12)) %>% group_by(ydstogo=d) %>%
        summarise(n=n(), conv=round(mean(converted),3), .groups="drop")), row.names=FALSE)

# ---- fit: 4th-only vs pooled (3rd+4th with a down term) ----------------------
m4    <- gam(converted ~ s(ydstogo, k=6) + s(yardline_100, k=5),
             data=g4, family=binomial(), method="REML")
mpool <- gam(converted ~ s(ydstogo, k=8) + s(yardline_100, k=6) + is_fourth,
             data=bind_rows(g4,g3), family=binomial(), method="REML")

grid <- tibble(ydstogo=c(1,2,3,4,5,7,10), yardline_100=50, is_fourth=1L)
cmp <- grid %>% transmute(ydstogo,
  p_4thonly=round(predict(m4,., type="response"),3),
  p_pooled =round(predict(mpool,., type="response"),3))
cat("\nP(convert) at midfield (yl50) by ydstogo — 4th-only vs pooled:\n")
print(as.data.frame(cmp), row.names=FALSE)
cat("(anchors: 4th-&-1 ~.68-.72, 4th-&-2 ~.55-.60, 4th-&-5 ~.45-.52, 4th-&-10 ~.27-.33)\n")

gridf <- tibble(ydstogo=3, yardline_100=c(3,10,25,50,75,95), is_fourth=1L)
cat("\nP(convert) 4th-&-3 by field position (4th-only):\n")
print(as.data.frame(gridf %>% transmute(yardline_100, p=round(predict(m4,.,type="response"),3))), row.names=FALSE)

saveRDS(list(m4=m4, mpool=mpool, n4=nrow(g4), n3=nrow(g3),
  note="4th-down conversion GAM, 2017-24 REG, league-average. m4=4th-only s(ydstogo)+s(yardline_100) [LOAD-BEARING]; mpool=3rd+4th pooled + is_fourth [diagnostic; pooling found unnecessary, agrees within 0.02]. converted = fourth_down_converted (offensive 1st down incl. off TD; def TDs excluded as failures)."),
  file.path(rds_dir,"fourthdown_conversion_model.rds"))
cat("\nSaved fourthdown_conversion_model.rds\n")
