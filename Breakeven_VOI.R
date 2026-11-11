# ============================================================================
# Breakeven_VOI.R — value-of-the-look / break-even slider tool.
# Per 4th down, compare four strategies' offense WP and pick the best:
#   GO (outright)    = p*w_S + (1-p)*w_F                       [nfl4th, UNBOOSTED p]
#   FG / PUNT        = fg_wp / punt_wp                          [nfl4th]
#   LINE-UP & DECIDE = q*go_wp(p_boost) + (1-q)*kick_wp - cost
#       kick_wp = max(fg_wp, punt_wp)
#       cost    = min(1, 1.5*(1-q)) * C       (C = min(DOG,TO), decision layer)
#       p_boost = p*(1+s)            if p <= 0.5
#                 1 - (1-s)*(1-p)    if p >  0.5     (true +30% capped so it can't exceed 1)
# Two sliders: q = good-look frequency (default .50), s = perfect-look boost (.30).
# Reports WIN SHARE and WIN MAGNITUDE (gain of lineup over the best alternative).
# UNITS: nfl4th WPs are FRACTIONS (0-1); decision-layer C is WP PERCENTAGE POINTS
#        -> divide C by 100 before subtracting. Saves breakeven_voi.rds.
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse) })
rds_dir <- file.path(getwd(), "build")
proj    <- getwd()
Q_DEF <- 0.50; S_DEF <- 0.30

# ---- decision layer: defines to_cost / dog_cost / compute_lineup_cost -------
invisible(capture.output(source(file.path(proj, "DecisionLayer_Assemble.R"))))
C_state_pp <- function(gsr, margin, ct, action, yl){              # C = min(DOG,TO) in WP pp
  dgc <- tryCatch(dog_cost(action, gsr, margin, yl), error = function(e) NA_real_)
  if (!is.finite(dgc)) dgc <- Inf
  if (is.finite(ct) && ct >= 1){
    toc <- tryCatch(to_cost(gsr, margin, as.integer(ct), 0)$cost, error = function(e) Inf)
    min(toc, dgc)
  } else dgc                                                      # 0 TOs -> DOG only
}

# ---- nfl4th per-play outputs + competitive / toss-up flags -----------------
samp  <- readRDS(file.path(rds_dir, "nfl4th_baseline_sample.rds"))$fourth
flags <- readRDS(file.path(rds_dir, "nfl4th_ci_full.rds"))$ci %>%
  select(game_id, play_id, vegas_wp, competitive, tossup_comb)
dat <- samp %>% left_join(flags, by = c("game_id","play_id")) %>%
  filter(is.finite(first_down_prob), is.finite(wp_succeed), is.finite(wp_fail),
         is.finite(posteam_timeouts_remaining)) %>%
  mutate(kick_wp  = pmax(fg_wp, punt_wp, na.rm = TRUE),
         kick_wp  = ifelse(is.finite(kick_wp), kick_wp, NA_real_),
         fallback = if_else(!is.na(fg_wp) & (is.na(punt_wp) | fg_wp >= punt_wp), "FG", "punt")) %>%
  filter(is.finite(kick_wp))
dat <- dat %>% rowwise() %>%                                      # precompute C once (slow part)
  mutate(C_pp = C_state_pp(game_seconds_remaining, score_differential,
                           posteam_timeouts_remaining, fallback, yardline_100)) %>%
  ungroup() %>% filter(is.finite(C_pp))
cat(sprintf("states: %d total | %d competitive (vegas_wp in [0.10,0.90])\n",
            nrow(dat), sum(dat$competitive, na.rm = TRUE)))

# ---- the model -------------------------------------------------------------
p_boost <- function(p, s) ifelse(p <= 0.5, p*(1+s), p + s*(1-p))
strat <- function(d, q, s){
  p  <- d$first_down_prob; pb <- p_boost(p, s)
  wp_go  <- p*d$wp_succeed + (1-p)*d$wp_fail
  go_b   <- pb*d$wp_succeed + (1-pb)*d$wp_fail
  cost   <- pmin(1, 1.5*(1-q)) * d$C_pp/100
  wp_lin <- q*go_b + (1-q)*d$kick_wp - cost
  best_alt <- pmax(wp_go, d$fg_wp, d$punt_wp, na.rm = TRUE)       # best OUTRIGHT option
  M <- cbind(go = wp_go, FG = d$fg_wp, punt = d$punt_wp, lineup = wp_lin); M[is.na(M)] <- -Inf
  list(wp_go = wp_go, wp_fg = d$fg_wp, wp_punt = d$punt_wp, wp_lineup = wp_lin,
       best_alt = best_alt, gain_pp = 100*(wp_lin - best_alt),    # >0 iff lineup is best
       winner = c("go","FG","punt","lineup")[max.col(M, ties.method = "first")])
}
win_share <- function(d,q,s) mean(strat(d,q,s)$winner == "lineup")
meaningful_share <- function(d,q,s,thr=0.5) mean(strat(d,q,s)$gain_pp >= thr)

# ---- (a) default-slider strategy map (competitive) -------------------------
r0 <- strat(dat, Q_DEF, S_DEF); dat$winner0 <- r0$winner; dat$gain_pp0 <- r0$gain_pp
comp <- dat %>% filter(competitive)
cat(sprintf("\n=== DEFAULT q=%.2f s=%.2f : strategy mix, %d competitive 4th downs ===\n",
            Q_DEF, S_DEF, nrow(comp)))
print(as.data.frame(comp %>% count(winner0) %>% mutate(pct = round(100*n/sum(n),1)) %>% arrange(desc(n))), row.names = FALSE)
cat(sprintf("\n>>> LINE-UP-AND-DECIDE is the best strategy on %.1f%% of competitive 4th downs <<<\n",
            100*mean(comp$winner0 == "lineup")))

# ---- (a2) WIN MAGNITUDE : how much does lining up add over the best outright?
win <- comp %>% filter(winner0 == "lineup")
cat("\n=== WIN MAGNITUDE (defaults; gain = lineup WP - best outright option, in pp) ===\n")
cat(sprintf("when lineup is best (n=%d):  mean %.2f | median %.2f | p75 %.2f | p90 %.2f | max %.2f  (pp)\n",
            nrow(win), mean(win$gain_pp0), median(win$gain_pp0),
            quantile(win$gain_pp0,.75,names=FALSE), quantile(win$gain_pp0,.90,names=FALSE), max(win$gain_pp0)))
cat("\nshare of competitive 4th downs where lining up wins by at least:\n")
cat(sprintf("  >  0.00 pp (any win) : %5.1f%%\n", 100*mean(comp$gain_pp0 > 0)))
for (thr in c(0.10, 0.25, 0.50, 1.00))
  cat(sprintf("  >= %.2f pp           : %5.1f%%\n", thr, 100*mean(comp$gain_pp0 >= thr)))
cat(sprintf("\nEXPECTED WP added by the tactic (mean of max(0,gain) over ALL competitive) = %.3f pp\n",
            mean(pmax(0, comp$gain_pp0))))

cat("\nlineup wins + mean gain, by ydstogo:\n")
print(as.data.frame(comp %>% mutate(ytg = cut(ydstogo, c(0,1,2,3,5,8,99), labels = c("1","2","3","4-5","6-8","9+"))) %>%
  group_by(ytg) %>% summarise(n = n(), lineup_pct = round(100*mean(winner0=="lineup"),1),
    mean_gain_pp = round(mean(gain_pp0[winner0=="lineup"]),2),
    median_gain_pp = round(median(gain_pp0[winner0=="lineup"]),2), .groups="drop")), row.names = FALSE)
cat("\nlineup wins + mean gain, by CI toss-up status:\n")
print(as.data.frame(comp %>% group_by(ci_tossup = tossup_comb) %>%
  summarise(n = n(), lineup_pct = round(100*mean(winner0=="lineup"),1),
    mean_gain_pp = round(mean(gain_pp0[winner0=="lineup"]),2), .groups="drop")), row.names = FALSE)

# ---- (b) (q,s) sweep : win-share AND meaningful-win (>=0.5pp) share ---------
QS <- c(0.3,0.4,0.5,0.6,0.7); SS <- c(0.1,0.2,0.3,0.4)
sweep <- expand_grid(q = QS, s = SS) %>%
  mutate(win_pct  = round(100*mapply(function(q,s) win_share(comp,q,s), q, s), 1),
         meaningful_pct = round(100*mapply(function(q,s) meaningful_share(comp,q,s,0.5), q, s), 1))
cat("\n=== (q,s) SWEEP : % competitive where LINEUP wins (ANY positive margin) ===\n")
print(as.data.frame(sweep %>% select(q,s,win_pct) %>% pivot_wider(names_from = s, values_from = win_pct, names_prefix = "s=")), row.names = FALSE)
cat("\n=== (q,s) SWEEP : % competitive where LINEUP wins by >= 0.5 pp (meaningful) ===\n")
print(as.data.frame(sweep %>% select(q,s,meaningful_pct) %>% pivot_wider(names_from = s, values_from = meaningful_pct, names_prefix = "s=")), row.names = FALSE)

# ---- (c) worked examples : representative real plays -----------------------
pick <- function(ytglo,ytghi,yllo,ylhi,scmax,label)
  comp %>% filter(ydstogo>=ytglo, ydstogo<=ytghi, yardline_100>=yllo, yardline_100<=ylhi, abs(score_differential)<=scmax) %>%
    slice(1) %>% mutate(ex = label)
EX <- bind_rows(
  pick(3,3,45,55,3,"4th-&-3 midfield, tied"),
  pick(1,1,55,65,7,"4th-&-1 own ~40"),
  pick(7,7,28,40,7,"4th-&-7 opp ~35 (FG-ish)"),
  pick(1,3,1,6,7,"4th-&-goal inside 6"))
be_q <- function(d,s){ qs<-seq(0.01,0.99,0.01); w<-sapply(qs,function(q) strat(d,q,s)$winner=="lineup"); if(any(w)) min(qs[w]) else NA_real_ }
be_s <- function(d,q){ ss<-seq(0.01,1.00,0.01); w<-sapply(ss,function(s) strat(d,q,s)$winner=="lineup"); if(any(w)) min(ss[w]) else NA_real_ }
exr <- bind_rows(lapply(seq_len(nrow(EX)), function(i){
  d <- EX[i,]; r <- strat(d, Q_DEF, S_DEF)
  tibble(ex=d$ex, ytg=d$ydstogo, yl=d$yardline_100, score=d$score_differential,
         p=round(d$first_down_prob,3), C_pp=round(d$C_pp,3),
         go=round(r$wp_go,3), FG=round(r$wp_fg,3), punt=round(r$wp_punt,3),
         lineup=round(r$wp_lineup,3), winner=r$winner, gain_pp=round(r$gain_pp,2),
         beq_s30=round(be_q(d,S_DEF),2), bes_q50=round(be_s(d,Q_DEF),2)) }))
cat("\n=== WORKED EXAMPLES (real plays; WP fractions; defaults q=.50 s=.30) ===\n")
print(as.data.frame(exr), row.names = FALSE)
cat("(gain_pp = lineup margin over best outright, pp; beq_s30 = min q so lineup wins at s=.30; bes_q50 = min s at q=.50)\n")

mag <- list(n_wins = nrow(win),
  gain_when_wins = c(mean=mean(win$gain_pp0), median=median(win$gain_pp0),
                     p75=quantile(win$gain_pp0,.75,names=FALSE), p90=quantile(win$gain_pp0,.90,names=FALSE)),
  share_by_threshold_pp = sapply(c(0,0.10,0.25,0.50,1.00), function(t) 100*mean(comp$gain_pp0 >= t)),
  expected_pp_added = mean(pmax(0, comp$gain_pp0)))
saveRDS(list(data = dat, sweep = sweep, examples = exr, magnitude = mag, default = list(q=Q_DEF, s=S_DEF),
  method = "Value-of-the-look break-even. Per 4th down pick max WP among GO/FG/PUNT/LINEUP. WP_lineup=q*go(p_boost)+(1-q)*kick-cost; cost=min(1,1.5(1-q))*C/100; C=min(DOG,TO) decision layer; p_boost piecewise(s). gain_pp = lineup margin over best outright. nfl4th WPs (fractions) + decision-layer C (pp). 2022-23; competitive = offense vegas_wp in [0.10,0.90]. Sliders q (.50), s (.30).",
  built = Sys.time()), file.path(rds_dir, "breakeven_voi.rds"))
cat("\nSaved breakeven_voi.rds\n")
