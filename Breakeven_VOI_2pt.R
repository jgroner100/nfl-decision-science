# ============================================================================
# Breakeven_VOI_2pt.R — value-of-the-look / break-even for the 2-PT decision.
# Same model as the 4th-down tool, restricted to GO-FOR-2 vs KICK-XP (no FG/punt):
#   GO-FOR-2 (outright) = conv_2pt*wp_2 + (1-conv_2pt)*wp_0   [nfl4th add_2pt_probs, UNBOOSTED]
#   KICK XP  (outright) = wp_go1                               [nfl4th]
#   LINE-UP & DECIDE    = q*go2_wp(conv_2pt_boost) + (1-q)*wp_go1 - cost
#       cost = min(1,1.5*(1-q)) * C ;  C = min(DOG_2pt, TO)  (decision layer, action "2pt")
#       conv_2pt_boost piecewise(s).
# nfl4th map: p=conv_2pt, w_S=wp_2, w_F=wp_0, go_wp=wp_go2, kick_wp=wp_go1.
# Sliders q (.50), s (.30). UNITS: nfl4th WP fractions; C is pp -> /100. -> breakeven_voi_2pt.rds.
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(nfl4th); library(nflreadr) })
rds_dir <- file.path(getwd(), "build"); proj <- getwd()
Q_DEF <- 0.50; S_DEF <- 0.30
invisible(capture.output(source(file.path(proj, "DecisionLayer_Assemble.R"))))
C_2pt_pp <- function(gsr, margin, ct){                            # C = min(DOG_2pt, TO) in WP pp
  dgc <- tryCatch(dog_cost("2pt", gsr, margin), error = function(e) NA_real_)
  if (!is.finite(dgc)) dgc <- Inf
  if (is.finite(ct) && ct >= 1){
    toc <- tryCatch(to_cost(gsr, margin, as.integer(ct), 0)$cost, error = function(e) Inf); min(toc, dgc)
  } else dgc
}
p_boost <- function(p, s) ifelse(p <= 0.5, p*(1+s), p + s*(1-p))

message("load_pbp(2022:2023) + add_2pt_probs...")
source("load_play_by_play.R")
pbp <- load_play_by_play(2022:2023)
two <- nfl4th::add_2pt_probs(pbp) %>%
  filter(!is.na(wp_go2), !is.na(wp_go1), !is.na(conv_2pt), !is.na(wp_2), !is.na(wp_0),
         !is.na(score_differential), !is.na(game_seconds_remaining), !is.na(posteam_timeouts_remaining),
         qtr <= 4, game_seconds_remaining > 0) %>%
  transmute(game_id, play_id, qtr, game_seconds_remaining, score_differential,
            posteam_timeouts_remaining, vegas_wp, conv_2pt, wp_2, wp_0, wp_go2, wp_go1, conv_1pt)
cat(sprintf("reconstruction check: max|wp_go2 - (p*w_S+(1-p)*w_F)| = %.4f\n",
            max(abs((two$conv_2pt*two$wp_2 + (1-two$conv_2pt)*two$wp_0) - two$wp_go2))))
two <- two %>% rowwise() %>%
  mutate(C_pp = C_2pt_pp(game_seconds_remaining, score_differential, posteam_timeouts_remaining)) %>%
  ungroup() %>% filter(is.finite(C_pp)) %>%
  mutate(competitive = vegas_wp >= 0.10 & vegas_wp <= 0.90)
cat(sprintf("2-pt decisions: %d total | %d competitive (vegas_wp in [0.10,0.90])\n", nrow(two), sum(two$competitive)))

# ---- the model -------------------------------------------------------------
strat2 <- function(d, q, s){
  p <- d$conv_2pt; pb <- p_boost(p, s)
  wp_go2 <- p*d$wp_2 + (1-p)*d$wp_0
  go2_b  <- pb*d$wp_2 + (1-pb)*d$wp_0
  cost   <- pmin(1, 1.5*(1-q)) * d$C_pp/100
  wp_lin <- q*go2_b + (1-q)*d$wp_go1 - cost
  best_alt <- pmax(wp_go2, d$wp_go1, na.rm = TRUE)
  M <- cbind(go2 = wp_go2, XP = d$wp_go1, lineup = wp_lin); M[is.na(M)] <- -Inf
  list(wp_go2 = wp_go2, wp_xp = d$wp_go1, wp_lineup = wp_lin, best_alt = best_alt,
       gain_pp = 100*(wp_lin - best_alt), winner = c("go2","XP","lineup")[max.col(M, ties.method="first")])
}
win_share <- function(d,q,s) mean(strat2(d,q,s)$winner == "lineup")
meaningful_share <- function(d,q,s,thr=0.5) mean(strat2(d,q,s)$gain_pp >= thr)

# ---- (a) default-slider map (competitive) ----------------------------------
r0 <- strat2(two, Q_DEF, S_DEF); two$winner0 <- r0$winner; two$gain_pp0 <- r0$gain_pp
comp <- two %>% filter(competitive)
cat(sprintf("\n=== DEFAULT q=%.2f s=%.2f : strategy mix, %d competitive 2-pt decisions ===\n", Q_DEF, S_DEF, nrow(comp)))
print(as.data.frame(comp %>% count(winner0) %>% mutate(pct = round(100*n/sum(n),1)) %>% arrange(desc(n))), row.names = FALSE)
cat(sprintf("\n>>> LINE-UP-AND-DECIDE (for 2) is best on %.1f%% of competitive 2-pt decisions <<<\n", 100*mean(comp$winner0=="lineup")))

# ---- (a2) win magnitude ----------------------------------------------------
win <- comp %>% filter(winner0 == "lineup")
cat("\n=== WIN MAGNITUDE (defaults; gain = lineup - best outright, pp) ===\n")
cat(sprintf("when lineup is best (n=%d): mean %.2f | median %.2f | p75 %.2f | p90 %.2f | max %.2f (pp)\n",
            nrow(win), mean(win$gain_pp0), median(win$gain_pp0),
            quantile(win$gain_pp0,.75,names=FALSE), quantile(win$gain_pp0,.90,names=FALSE), max(win$gain_pp0)))
cat("\nshare of competitive 2-pt decisions where lining up wins by at least:\n")
cat(sprintf("  >  0.00 pp (any) : %5.1f%%\n", 100*mean(comp$gain_pp0 > 0)))
for (thr in c(0.10,0.25,0.50,1.00)) cat(sprintf("  >= %.2f pp        : %5.1f%%\n", thr, 100*mean(comp$gain_pp0 >= thr)))
cat(sprintf("\nEXPECTED WP added per competitive 2-pt decision = %.3f pp\n", mean(pmax(0, comp$gain_pp0))))

cat("\nlineup wins + mean gain, by post-TD margin:\n")
print(as.data.frame(comp %>% mutate(sit = cut(score_differential, c(-Inf,-5,-3,-2,-1,0,1,2,4,Inf),
    labels = c("down 5+","down 3-4","down 2","down 1","tied","up 1","up 2","up 3-4","up 5+"))) %>%
  group_by(sit) %>% summarise(n=n(), lineup_pct=round(100*mean(winner0=="lineup"),1),
    mean_gain=round(mean(gain_pp0[winner0=="lineup"]),2), .groups="drop")), row.names = FALSE)
cat("\nlineup wins + mean gain + mean C, by time:\n")
print(as.data.frame(comp %>% mutate(time = case_when(game_seconds_remaining<=120~"final 2 min",
      game_seconds_remaining<=360~"final 6 min", TRUE~"earlier")) %>%
  group_by(time) %>% summarise(n=n(), lineup_pct=round(100*mean(winner0=="lineup"),1),
    mean_gain=round(mean(gain_pp0[winner0=="lineup"]),2), mean_C_pp=round(mean(C_pp),2), .groups="drop")), row.names = FALSE)

# ---- (b) (q,s) sweep -------------------------------------------------------
QS <- c(0.3,0.4,0.5,0.6,0.7); SS <- c(0.1,0.2,0.3,0.4)
sweep <- expand_grid(q=QS, s=SS) %>% mutate(
  win_pct = round(100*mapply(function(q,s) win_share(comp,q,s), q, s), 1),
  meaningful_pct = round(100*mapply(function(q,s) meaningful_share(comp,q,s,0.5), q, s), 1))
cat("\n=== (q,s) SWEEP : % competitive 2-pt where LINEUP wins (any positive) ===\n")
print(as.data.frame(sweep %>% select(q,s,win_pct) %>% pivot_wider(names_from=s, values_from=win_pct, names_prefix="s=")), row.names=FALSE)
cat("\n=== (q,s) SWEEP : % where LINEUP wins by >= 0.5 pp (meaningful) ===\n")
print(as.data.frame(sweep %>% select(q,s,meaningful_pct) %>% pivot_wider(names_from=s, values_from=meaningful_pct, names_prefix="s=")), row.names=FALSE)

# ---- (c) worked examples ---------------------------------------------------
pick <- function(sclo,schi,gsrlo,gsrhi,label)
  comp %>% filter(score_differential>=sclo, score_differential<=schi, game_seconds_remaining>=gsrlo, game_seconds_remaining<=gsrhi) %>% slice(1) %>% mutate(ex=label)
EX <- bind_rows(
  pick(-2,-2,0,3600,"down 2 (classic go-for-2)"),
  pick(-1,-1,0,3600,"down 1"),
  pick(1,1,0,3600,"up 1"),
  pick(-2,-1,0,300,"trailing 1-2, final 5 min"))
be_q <- function(d,s){ qs<-seq(0.01,0.99,0.01); w<-sapply(qs,function(q) strat2(d,q,s)$winner=="lineup"); if(any(w)) min(qs[w]) else NA_real_ }
be_s <- function(d,q){ ss<-seq(0.01,1.00,0.01); w<-sapply(ss,function(s) strat2(d,q,s)$winner=="lineup"); if(any(w)) min(ss[w]) else NA_real_ }
exr <- bind_rows(lapply(seq_len(nrow(EX)), function(i){
  d <- EX[i,]; r <- strat2(d, Q_DEF, S_DEF)
  tibble(ex=d$ex, margin=d$score_differential, gsr=round(d$game_seconds_remaining), p=round(d$conv_2pt,3), C_pp=round(d$C_pp,3),
         go2=round(r$wp_go2,3), XP=round(r$wp_xp,3), lineup=round(r$wp_lineup,3), winner=r$winner, gain_pp=round(r$gain_pp,2),
         beq_s30=round(be_q(d,S_DEF),2), bes_q50=round(be_s(d,Q_DEF),2)) }))
cat("\n=== WORKED EXAMPLES (real 2-pt decisions; defaults q=.50 s=.30) ===\n")
print(as.data.frame(exr), row.names = FALSE)
cat("(gain_pp = lineup margin over best outright; beq_s30 = min q at s=.30; bes_q50 = min s at q=.50)\n")

saveRDS(list(data=two, sweep=sweep, examples=exr, default=list(q=Q_DEF,s=S_DEF),
  method="2-pt value-of-the-look break-even. GO-FOR-2 vs KICK-XP vs LINEUP; pick max WP. nfl4th add_2pt_probs (p=conv_2pt, w_S=wp_2, w_F=wp_0, go=wp_go2, kick=wp_go1) + C=min(DOG_2pt,TO) decision layer. Same model/sliders as 4th-down. 2022-23; competitive = offense vegas_wp in [0.10,0.90].",
  built=Sys.time()), file.path(rds_dir, "breakeven_voi_2pt.rds"))
cat("\nSaved breakeven_voi_2pt.rds\n")
