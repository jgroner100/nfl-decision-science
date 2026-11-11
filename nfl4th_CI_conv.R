# ============================================================================
# nfl4th_CI_conv.R — STEP 1 of the go_boost CI: the CONVERSION-PROBABILITY band.
# nfl4th's go_boost stays the POINT estimate. We attach uncertainty by sizing an
# interval on nfl4th's own conversion prob (first_down_prob) from how much
# historical data supports that (ydstogo, field position), and propagating it
# through nfl4th's own wp_succeed / wp_fail:
#     go_wp(p)   = p*wp_succeed + (1-p)*wp_fail        [exact nfl4th identity]
#     go_boost(p)= 100*(go_wp(p) - max(fg_wp, punt_wp))
# Width source = pooled 3rd+4th-down go-attempt sample size (Romer/Sandholtz
# pooling => larger n => TIGHTER band; deliberately NOT raw thin 4th-only bins,
# which would over-state toss-ups — the anti-conservative direction for our
# "value of the look" thesis). Wilson 95% interval. NO replica, all nfl4th outputs.
# Toss-up = 95% CI straddles 0. Saves per-play CI for the WP-surface layer (step 2).
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(nflreadr) })
rds_dir <- file.path(getwd(), "build")
samp <- readRDS(file.path(rds_dir, "nfl4th_baseline_sample.rds"))
fourth <- samp$fourth
cat(sprintf("nfl4th sample: %d 4th downs (%s)\n", nrow(fourth), paste(range(samp$seasons), collapse="-")))

# ---- historical sample size: pooled 3rd+4th go attempts, 2017-24 REG --------
message("load_pbp(2017:2024) for conversion-support counts...")
source("load_play_by_play.R")
pbp <- load_play_by_play(2017:2024)
# 4th-down go attempts (ALL); 3rd-down attempts only in COMPETITIVE states
# (user: pool 3rd downs but drop blowouts -> offense vegas_wp must be in [0.10,0.90],
# so a team with >90% or <10% win prob on 3rd down does not inform the support).
att4 <- pbp %>%
  filter(season_type=="REG", play_type %in% c("run","pass"), ydstogo>=1, !is.na(yardline_100),
         down==4, (fourth_down_converted==1 | fourth_down_failed==1)) %>%
  transmute(ytg=pmin(as.integer(ydstogo),15L), yl=as.integer(pmin(pmax(yardline_100,1),99)))
att3_all <- pbp %>%
  filter(season_type=="REG", play_type %in% c("run","pass"), ydstogo>=1, !is.na(yardline_100),
         down==3, (third_down_converted==1 | third_down_failed==1))
att3 <- att3_all %>%
  filter(!is.na(vegas_wp), vegas_wp>=0.10, vegas_wp<=0.90) %>%
  transmute(ytg=pmin(as.integer(ydstogo),15L), yl=as.integer(pmin(pmax(yardline_100,1),99)))
cat(sprintf("4th-down go attempts: %d\n", nrow(att4)))
cat(sprintf("3rd-down attempts: %d total -> %d kept (vegas_wp in [0.10,0.90]); %d blowout-dropped (%.1f%%)\n",
            nrow(att3_all), nrow(att3), nrow(att3_all)-nrow(att3), 100*(1-nrow(att3)/nrow(att3_all))))
att <- bind_rows(att4, att3)
cat(sprintf("pooled support sample: %d\n", nrow(att)))

# count matrix [ytg 1..15, yl 1..99] + cumulative over yl for a +/-7yd window
M <- matrix(0L, nrow=15, ncol=99)
tb <- att %>% count(ytg, yl)
M[cbind(tb$ytg, tb$yl)] <- tb$n
Cum <- t(apply(M, 1, cumsum))                                   # Cum[a, y] = sum_{j<=y} M[a,j]
n_eff_fun <- function(a, y){ a <- min(max(a,1L),15L); y <- min(max(y,1L),99L)
  hi <- min(99L, y+7L); lo <- max(0L, y-8L)
  win <- Cum[a, hi] - if (lo>=1) Cum[a, lo] else 0L
  if (win < 30L) sum(M[a, ]) else win }                          # fall back to ytg-only if window thin

# ---- Wilson 95% interval on p given effective n -----------------------------
wilson <- function(p, n, z=1.96){ if (n<=0) return(c(0,1))
  d <- 1 + z^2/n; c <- (p + z^2/(2*n))/d; h <- (z/d)*sqrt(p*(1-p)/n + z^2/(4*n^2))
  c(max(0,c-h), min(1,c+h)) }

# ---- per-play propagation ---------------------------------------------------
go_boost_at <- function(p, wsucc, wfail, maxnon) 100*((p*wsucc + (1-p)*wfail) - maxnon)
res <- fourth %>%
  rowwise() %>%
  mutate(
    ytg_c = min(as.integer(ydstogo),15L),
    n_eff = n_eff_fun(ytg_c, as.integer(yardline_100)),
    maxnon = { v <- c(fg_wp, punt_wp); if (all(is.na(v))) NA_real_ else max(v, na.rm=TRUE) },
    gb_pt = go_boost_at(first_down_prob, wp_succeed, wp_fail, maxnon),    # reconstructed point
    p_lo = wilson(first_down_prob, n_eff)[1], p_hi = wilson(first_down_prob, n_eff)[2],
    gb_a = go_boost_at(p_lo, wp_succeed, wp_fail, maxnon),
    gb_b = go_boost_at(p_hi, wp_succeed, wp_fail, maxnon),
    gb_lo = min(gb_a, gb_b), gb_hi = max(gb_a, gb_b)
  ) %>% ungroup() %>%
  mutate(half_w = (gb_hi - gb_lo)/2,
         tossup = gb_lo < 0 & gb_hi > 0,
         rec_pt = if_else(go_boost > 0, "GO", "kick/punt"))

# ---- verify the reconstruction matches nfl4th's saved go_boost ---------------
chk <- res %>% filter(!is.na(gb_pt), !is.na(go_boost))
cat(sprintf("\nreconstruction check: max|gb_pt - nfl4th go_boost| = %.4f pp (mean %.4f)\n",
            max(abs(chk$gb_pt - chk$go_boost)), mean(abs(chk$gb_pt - chk$go_boost))))

ok <- res %>% filter(!is.na(gb_lo), !is.na(gb_hi))
cat(sprintf("plays with a CI: %d\n", nrow(ok)))
cat(sprintf("\nn_eff (conversion support) summary:\n")); print(round(summary(ok$n_eff)))
cat(sprintf("\ngo_boost CI half-width (pp) summary:\n")); print(round(summary(ok$half_w),2))
cat(sprintf("\n=== TOSS-UP share (95%% conversion CI straddles 0): %.1f%% ===\n", 100*mean(ok$tossup)))
cat(sprintf("(for reference, point-estimate |go_boost|<1pp was 40.7%%)\n"))

cat("\ntoss-up share by ydstogo bucket:\n")
print(as.data.frame(ok %>% mutate(ytg=cut(ydstogo,c(0,1,2,3,5,8,99),labels=c("1","2","3","4-5","6-8","9+"))) %>%
  group_by(ytg) %>% summarise(n=n(), med_n_eff=round(median(n_eff)),
    med_half_w=round(median(half_w),2), tossup=round(100*mean(tossup),1))), row.names=FALSE)

cat("\nexample plays (go_boost point [lo, hi], pp):\n")
set.seed(1)
ex <- ok %>% sample_n(12) %>%
  transmute(ytg=ydstogo, yl=yardline_100, score=score_differential, gsr=round(game_seconds_remaining),
            p=round(first_down_prob,3), n_eff, go_boost=round(go_boost,2),
            lo=round(gb_lo,2), hi=round(gb_hi,2), tossup) %>% arrange(go_boost)
print(as.data.frame(ex), row.names=FALSE)

saveRDS(list(ci = res, seasons = samp$seasons, method = "Conversion-probability band on nfl4th go_boost. p=first_down_prob; CI=Wilson(p, n_eff); n_eff=pooled (4th-down ALL + 3rd-down with offense vegas_wp in [0.10,0.90], blowouts dropped) go-attempt count in (ytg match, yl +/-7), fallback ytg-only if <30. Propagated through nfl4th wp_succeed/wp_fail; fg/punt held fixed. Toss-up = CI straddles 0. WP-surface uncertainty added in step 2.",
  built = Sys.time()), file.path(rds_dir, "nfl4th_ci_conv.rds"))
cat("\nSaved nfl4th_ci_conv.rds (per-play conversion-band CI; feeds step 2).\n")
