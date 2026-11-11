# ============================================================================
# Make_Figures.R -- all paper/presentation figures + tables for the line-up project.
# Reads saved RDS only (no load_pbp; matchup fig uses cached nfl4th boosters).
# Figures -> build/figures/  (PNG, 200 dpi, white bg)
# Tables  -> build/tables/   (CSV)
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse) })
rds  <- file.path(getwd(), "build")

figd <- file.path(rds, "figures"); tabd <- file.path(rds, "tables")
dir.create(figd, showWarnings=FALSE); dir.create(tabd, showWarnings=FALSE)
OK <- c(go="#009E73", FG="#E69F00", punt="#56B4E9", lineup="#D55E00",
        OLD="#999999", NEW="#D55E00", raw="#0072B2", eff="#D55E00")
theme_paper <- theme_minimal(base_size=14) +
  theme(panel.grid.minor=element_blank(), plot.title=element_text(face="bold"),
        plot.title.position="plot", legend.position="bottom",
        plot.subtitle=element_text(color="grey30"), plot.caption=element_text(color="grey45", size=9))
theme_set(theme_paper)
savep <- function(p, name, w=8, h=5){ ggsave(file.path(figd, paste0(name,".png")), p, width=w, height=h, dpi=200, bg="white"); cat("  fig:", name, "\n") }
savet <- function(df, name){ write.csv(df, file.path(tabd, paste0(name,".csv")), row.names=FALSE); cat("  tbl:", name, "\n") }
mk <- function(label, expr) {
  cat("[", label, "]\n")
  force(expr)
}
p_boost <- function(p,s) ifelse(p<=0.5, p*(1+s), p+s*(1-p)); wk<-function(g) suppressWarnings(as.integer(str_split_fixed(g,"_",4)[,2]))
MLAB <- c("down 5+","down 3-4","down 2","down 1","tied","up 1","up 2","up 3-4","up 5+")
MBREAKS<-c(-Inf,-5,-3,-2,-1,0,1,2,4,Inf); ytg_lab<-c("1","2","3","4-5","6-8","9+"); ytg_brk<-c(0,1,2,3,5,8,99)

# ============================ FIG 1: decision map ===========================
mk("F01 decision map", {
  qm <- 0.60; sm <- 0.20   # reference scenario: recompute the winner from the saved WP components
  m0 <- readRDS(file.path(rds,"breakeven_voi_synthgrid.rds"))$map
  pb <- p_boost(m0$p, sm); go <- m0$p*m0$wS+(1-m0$p)*m0$wF; gob <- pb*m0$wS+(1-pb)*m0$wF
  kick <- pmax(m0$fg, m0$punt, na.rm=TRUE); lin <- qm*gob+(1-qm)*kick - pmin(1,1.5*(1-qm))*m0$Cpp/100
  M <- cbind(go, m0$fg, m0$punt, lin); M[is.na(M)] <- -Inf
  m <- m0 %>% mutate(winner=factor(c("go","FG","punt","lineup")[max.col(M, ties.method="first")],
                                   levels=c("go","FG","punt","lineup")))
  p <- ggplot(m, aes(yl, factor(ytg), fill=winner)) + geom_tile(color="white", linewidth=.4) +
    scale_fill_manual(values=OK[c("go","FG","punt","lineup")], name="best strategy",
                      labels=c("GO","FG","PUNT","LINE UP & DECIDE")) +
    scale_x_continuous(breaks=seq(20,80,10)) +
    labs(title="When to line up on 4th down", subtitle="Best strategy by distance and field position (synthetic, tied, Q3; q=0.6, s=0.2)",
         x="yards to opponent goal (yardline_100)", y="yards to go",
         caption="Line-up wins the medium-distance midfield band; GO short, PUNT backed-up/long, FG long+close.")
  savep(p, "F01_decision_map", 9, 5.2); savet(m, "T08_decision_map")
})

# ============================ FIG 2: toss-up by ydstogo =====================
mk("F02 tossup by ydstogo", {
  ci <- readRDS(file.path(rds,"nfl4th_ci_full.rds"))$ci %>% filter(competitive) %>%
    mutate(ytg=cut(ydstogo, ytg_brk, labels=ytg_lab))
  d <- ci %>% group_by(ytg) %>% summarise(n=n(), tossup=mean(tossup_comb), .groups="drop")
  p <- ggplot(d, aes(ytg, 100*tossup)) + geom_col(fill=OK["lineup"]) +
    geom_text(aes(label=sprintf("%.0f%%\n(n=%d)",100*tossup,n)), vjust=-0.2, size=3.6, lineheight=.85) +
    expand_limits(y=max(100*d$tossup)*1.15) +
    labs(title="4th-down toss-ups concentrate at 4th-&-3-to-8",
         subtitle="Share of competitive 4th downs where nfl4th's go-vs-kick 95% CI straddles zero",
         x="yards to go", y="CI toss-up share (%)",
         caption="Competitive = offense pre-snap vegas_wp in [0.10, 0.90], 2022-23.")
  savep(p, "F02_tossup_by_ytg"); savet(d, "T_tossup_by_ytg")
})

# ============================ FIG 3: g_emp heatmap =========================
mk("F03 g_emp heatmap", {
  cells <- readRDS(file.path(rds,"behavioral_4th_gorate.rds"))$cells %>%
    mutate(ytg=factor(ytg, levels=ytg_lab),
           yl=factor(yl, levels=c("opp1-10","opp11-20","opp21-40","mid41-60","own61-80","own81-99")))
  p <- ggplot(cells, aes(yl, ytg, fill=100*eff)) + geom_tile(color="white", linewidth=.4) +
    geom_text(aes(label=sprintf("%.0f", 100*eff)), size=3.6) +
    scale_fill_viridis_c(name="effective go %", option="C", direction=1) +
    labs(title="Empirical 'effective go' rate by 4th-down state",
         subtitle="effective_go = actually went OR nfl4th go_boost >= 1.5 wp (the override); 2017-24",
         x="field position", y="yards to go",
         caption="The behavioral baseline g_emp. Cells well-sampled (n 97-3834); see T05 for n + Wilson CIs.")
  savep(p, "F03_gemp_heatmap", 9, 5)
  ct <- readRDS(file.path(rds,"behavioral_4th_gorate.rds"))$cells %>%
    transmute(ytg, yl, n, eff_go_pct=round(100*eff,1), lo=round(100*lo,1), hi=round(100*hi,1))
  savet(ct, "T05_gemp_cells")
})

# ============================ FIG 4: era trend ============================
mk("F04 era trend", {
  e4 <- readRDS(file.path(rds,"behavioral_4th_gorate.rds"))$era %>% transmute(season, raw=raw_go, effective=eff_go, leg="4th down")
  e2 <- readRDS(file.path(rds,"behavioral_2pt_gorate.rds"))$era %>% transmute(season, raw=100*raw, effective=100*eff15, leg="2-pt")
  e <- bind_rows(e4,e2) %>% pivot_longer(c(raw,effective), names_to="series", values_to="rate")
  p <- ggplot(e, aes(season, rate, color=series)) + geom_line(linewidth=1) + geom_point(size=2) +
    facet_wrap(~leg, scales="free_y") +
    scale_color_manual(values=c(raw=unname(OK["raw"]), effective=unname(OK["eff"])), name=NULL) +
    scale_x_continuous(breaks=2017:2024) +
    labs(title="Go rates rose then plateaued (the reason g_emp is season-aware)",
         subtitle="Raw vs override-effective go rate by season",
         x=NULL, y="go rate (%)",
         caption="The g_emp GAM carries a season smooth and is evaluated at each decision's own year.")
  savep(p, "F04_era_trend", 9, 4.6); savet(bind_rows(e4,e2), "T07_era_trend")
})

# ============================ FIG 5: DOG cost scenarios ====================
mk("F05 DOG scenarios", {
  lab <- c(twopt_FH="2-pt, first half", twopt_late2m="2-pt, 2:00 left", twopt_decide="2-pt, deciding (<=0:15)",
           FG52_tie_FH="FG 52yd tie, first half", FG52_tie_dec="FG 52yd tie, deciding", FG52_win_dec="FG 52yd win, deciding",
           FG50_FH="FG 50yd, first half", punt_own20_FH="punt own-20, first half", punt_deadzone="punt dead-zone (opp40)")
  sc <- readRDS(file.path(rds,"dog_v8_final_ci.rds"))$scenarios %>%
    mutate(name=factor(lab[scenario], levels=lab[c("punt_deadzone","twopt_FH","punt_own20_FH","twopt_late2m","FG50_FH","twopt_decide","FG52_tie_FH","FG52_tie_dec","FG52_win_dec")]))
  p <- ggplot(sc, aes(point, name)) +
    geom_col(fill=OK["FG"], alpha=.9) + geom_errorbarh(aes(xmin=lo95, xmax=hi95), height=.25, color="grey25") +
    geom_text(aes(label=sprintf("%.2f", point)), hjust=-0.25, size=3.4) + expand_limits(x=10) +
    labs(title="The cost of a delay-of-game, by what you fall back to",
         subtitle="WP% lost from a 5-yard DOG, with 95% CI (DOG model v8)",
         x="delay-of-game cost (win probability, wp)", y=NULL,
         caption="Scoring plays (FG/2-pt) are dear, especially deciding; the punt dead-zone is near-free.")
  savep(p, "F05_dog_scenarios", 9, 5); savet(readRDS(file.path(rds,"dog_v8_final_ci.rds"))$scenarios, "T01_dog_scenarios")
})

# ============================ FIG 6: TO cost by score/time =================
mk("F06 TO cost", {
  cb <- readRDS(file.path(rds,"decisionlayer_lineup_cost.rds"))$combined
  d <- cb %>% filter(current_to==3, margin %in% c(-7,0,3)) %>%
    group_by(gsr, margin) %>% summarise(to_cost=mean(to_cost), .groups="drop") %>%
    mutate(situation=recode(as.character(margin), `-7`="trailing 7", `0`="tied", `3`="leading 3"))
  dref <- cb %>% filter(action=="punt", yardline==40, margin==0, current_to==3) %>%
    transmute(gsr, dog=dog_cost)            # tied midfield-punt DOG, rises late (replaces the old flat 0.30)
  p <- ggplot(d, aes(gsr, to_cost, color=situation)) + geom_line(linewidth=1) +
    geom_line(data=dref, aes(gsr, dog), inherit.aes=FALSE, linetype="dashed", color="grey40", linewidth=0.8) +
    annotate("text", x=2250, y=0.55, label="~DOG cost (midfield punt)", color="grey40", size=3.2) +
    geom_vline(xintercept=1800, linetype="dotted", color="grey60") +
    annotate("text", x=1840, y=max(d$to_cost)*0.95, label="halftime", angle=90, color="grey55", size=3, hjust=1) +
    scale_x_reverse(breaks=c(1800,1200,900,600,360,120)) +
    scale_color_manual(values=c("trailing 7"="#D55E00","tied"="#0072B2","leading 3"="#009E73"), name=NULL) +
    labs(title="Cost of burning a timeout: keep them when trailing, spend when leading",
         subtitle="Marginal TO cost (holding 3) vs game clock, by score (decision layer)",
         x="game seconds remaining (-> end of game)", y="timeout cost (wp)",
         caption="Dashed = tied midfield-punt DOG (cheap early, rising late); trailing TO cost stays well above it.")
  savep(p, "F06_to_cost", 9, 5); savet(d, "T_to_cost")
})

# ============================ value engine (clean core+reclaim) ============
S<-0.20; Q<-0.50; FLOOR<-0.10; THR<-1.5; n_ts<-64
g4o <- readRDS(file.path(rds,"behavioral_4th_gorate.rds"))
d4 <- readRDS(file.path(rds,"breakeven_voi.rds"))$data %>% mutate(week=wk(game_id)) %>% filter(week<=18) %>%
  left_join(g4o$gkey, by=c("game_id","play_id"))
p4<-d4$first_down_prob; d4$wp_go<-p4*d4$wp_succeed+(1-p4)*d4$wp_fail; d4$best<-pmax(d4$wp_go,d4$kick_wp,na.rm=TRUE)
go_b<-p_boost(p4,S)*d4$wp_succeed+(1-p_boost(p4,S))*d4$wp_fail
d4$lin<-Q*go_b+(1-Q)*d4$kick_wp - pmin(1,1.5*(1-Q))*d4$C_pp/100
d4$core<-pmax(0,d4$lin-d4$best)
d4$reclaim<-ifelse(d4$go_boost>=THR,0,(1-d4$g_hat_15)*pmax(0,pmin(d4$lin,d4$wp_go)-d4$kick_wp))
d4$val_new<-{v<-100*(d4$core+d4$reclaim); ifelse(v>=FLOOR,v,0)}
d4$val_old<-{g<-100*pmax(0,d4$lin-d4$best); ifelse(g>=FLOOR,g,0)}

# ============================ FIG 7: headline value =======================
mk("F07 headline value", {
  pp <- readRDS(file.path(rds,"breakeven_voi_season_behavioral.rds"))$per_team_season_pp
  ci <- readRDS(file.path(rds,"breakeven_voi_season_ci.rds"))$default$total
  bars <- tibble(model=c("OLD\n(two baselines)","OLD\n(two baselines)","NEW\n(behavioral)","NEW\n(behavioral)"),
                 leg=c("4th down","2-pt","4th down","2-pt"),
                 val=c(pp$old["fourth"],pp$old["two_pt"],pp$new_gam["fourth"],pp$new_gam["two_pt"]))
  tot <- tibble(model=c("OLD\n(two baselines)","NEW\n(behavioral)"),
                total=c(pp$old["total"],pp$new_gam["total"]),
                lo=c(NA,quantile(ci,.025)), hi=c(NA,quantile(ci,.975)))
  p <- ggplot(bars, aes(model, val, fill=leg)) + geom_col(width=.62) +
    geom_errorbar(data=tot, aes(x=model, ymin=lo, ymax=hi), inherit.aes=FALSE, width=.12, color="grey20") +
    geom_text(data=tot, aes(x=model, y=total, label=sprintf("%.1f", total)), inherit.aes=FALSE, vjust=-0.9, fontface="bold", size=4.4) +
    scale_fill_manual(values=c("4th down"=unname(OK["go"]), "2-pt"=unname(OK["FG"])), name=NULL) +
    scale_x_discrete(limits=c("OLD\n(two baselines)","NEW\n(behavioral)")) + expand_limits(y=19.5) +
    labs(title="Season value of the line-up option ~ doubled after fixing the baseline",
         subtitle="Per team per regular season (q=0.5, s=0.2); error bar = 95% estimation CI",
         x=NULL, y="WP added (pp / team-season)",
         caption="OLD summed two different baselines; NEW = one coherent 'vs what teams actually do'. 13.6 -> 17.8 pp (~0.18 wins).")
  savep(p, "F07_headline_value", 8.5, 5.4)
  savet(tibble(metric=c("4th","2pt","total","total_ex_up5"),
               OLD=c(pp$old["fourth"],pp$old["two_pt"],pp$old["total"],NA),
               NEW_gam=c(pp$new_gam["fourth"],pp$new_gam["two_pt"],pp$new_gam["total"],pp$new_gam["total_ex_up5"]),
               NEW_raw=c(pp$new_raw["fourth"],pp$new_raw["two_pt"],pp$new_raw["total"],NA),
               NEW_total_CI_lo=c(NA,NA,quantile(ci,.025),NA), NEW_total_CI_hi=c(NA,NA,quantile(ci,.975),NA)), "T02_headline_value")
})

# ============================ FIG 8: 4th value by ytg =====================
mk("F08 value by ytg", {
  d <- tibble(ytg=cut(d4$ydstogo, ytg_brk, labels=ytg_lab), NEW=d4$val_new, OLD=d4$val_old) %>%
    group_by(ytg) %>% summarise(NEW=sum(NEW)/n_ts, OLD=sum(OLD)/n_ts, .groups="drop") %>%
    pivot_longer(c(NEW,OLD), names_to="model", values_to="pp")
  p <- ggplot(d, aes(ytg, pp, fill=model)) + geom_col(position=position_dodge(.7), width=.65) +
    scale_fill_manual(values=c(OLD=unname(OK["OLD"]), NEW=unname(OK["NEW"])), name=NULL) +
    labs(title="The 4th-down line-up value lands at 4th-&-3-to-8",
         subtitle="WP added per team-season by distance: OLD (vs-optimal) vs NEW (behavioral core+reclaim)",
         x="yards to go", y="WP added (pp / team-season)",
         caption="The behavioral reclaim (teams punt too much) is biggest in the medium-distance band.")
  savep(p, "F08_value_by_ytg"); savet(pivot_wider(d, names_from=model, values_from=pp), "T_value_by_ytg")
})

# ============================ FIG 9: 2pt waterfall ========================
mk("F09 2pt waterfall", {
  st <- tibble(step=factor(c("OLD\nside-bucket","+ core /\nformula","+ higher g\n(override)","- zero\nclear-cut","NEW\nunified"),
                           levels=c("OLD\nside-bucket","+ core /\nformula","+ higher g\n(override)","- zero\nclear-cut","NEW\nunified")),
               val=c(8.22, 9.63, 8.65, 5.64, 5.64), kind=c("end","up","down","down","end"))
  st <- st %>% mutate(prev=lag(val, default=0), ymin=pmin(prev,val), ymax=pmax(prev,val),
                      ymin=if_else(kind=="end",0,ymin), ymax=if_else(kind=="end",val,ymax),
                      delta=if_else(kind=="end", val, val-prev))
  p <- ggplot(st, aes(step)) + geom_rect(aes(xmin=as.numeric(step)-.35, xmax=as.numeric(step)+.35, ymin=ymin, ymax=ymax, fill=kind)) +
    geom_text(aes(y=ymax, label=sprintf("%+.2f", delta)), vjust=-0.4, size=3.6) +
    scale_fill_manual(values=c(end="#444444", up=unname(OK["go"]), down=unname(OK["lineup"])), guide="none") + expand_limits(y=10.4) +
    labs(title="Why the 2-pt season value fell (8.22 -> 5.64)",
         subtitle="The +1.5 wp override stops crediting line-up in clear-cut go-for-2 spots (mostly up-5+)",
         x=NULL, y="two-point win probability added (wp / team-season)",
         caption="Net: +1.41 (core+formula) - 0.98 (higher go rate) - 3.01 (zero clear-cut go); ~75% of the drop is up-5+.")
  savep(p, "F09_2pt_waterfall", 8.5, 5)
  savet(tibble(step=c("OLD","+core/formula","+higher_g","-zero_clearcut","NEW"), value=c(8.22,9.63,8.65,5.64,5.64)), "T09_2pt_waterfall")
})

# ============================ FIG 10: (q,s) heatmap ======================
mk("F10 qs heatmap", {
  g <- readRDS(file.path(rds,"breakeven_voi_season_qs_sweep.rds"))$grid
  long <- g %>% select(q,s,total,total_ex) %>%
    pivot_longer(c(total,total_ex), names_to="metric", values_to="pp") %>%
    mutate(metric=recode(metric, total="full total", total_ex="ex-(2-pt up-5+)"))
  p <- ggplot(long, aes(factor(s), factor(q), fill=pp)) + geom_tile(color="white", linewidth=.5) +
    geom_text(aes(label=sprintf("%.1f\n(%.2f w)", pp, pp/100)), size=3.3, lineheight=.8) +
    facet_wrap(~metric) + scale_fill_viridis_c(name="pp / team-season", option="D") +
    labs(title="Season value across the two unmeasurable sliders",
         subtitle="q = P(good look -> go); s = perfect-look conversion boost",
         x="s (conversion boost)", y="q (good-look frequency)",
         caption="q is the dominant lever; s ~linear. Default q=0.5, s=0.2. High corners are aggressive upper bounds.")
  savep(p, "F10_qs_heatmap", 11, 5)
  savet(g %>% transmute(q,s,fourth=round(fourth,2),two_pt=round(two_pt,2),total=round(total,2),
                        total_ex_up5=round(total_ex,2),wins=round(wins,3),wins_ex=round(wins_ex,3)), "T03_qs_sweep")
})

# ============================ FIG 11: floor sensitivity ==================
mk("F11 floor", {
  fs <- readRDS(file.path(rds,"breakeven_voi_season_qs_floor_sweep.rds"))$grid %>%
    filter(q==0.5, s %in% c(0.18,0.20,0.22,0.25))
  p <- ggplot(fs, aes(floor, total, color=factor(s), group=s)) + geom_line(linewidth=1) + geom_point(size=2) +
    scale_color_viridis_d(name="s", option="D", end=.85) + scale_x_continuous(breaks=c(.10,.15,.20,.25)) +
    labs(title="The floor is the weakest knob",
         subtitle="Total season value vs the per-decision floor (q=0.5)",
         x="floor (minimum per-decision wp that counts)", y="WP added (wp / team-season)",
         caption="Raising the floor 0.10 -> 0.25 trims the total only ~13%.")
  savep(p, "F11_floor")
  savet(readRDS(file.path(rds,"breakeven_voi_season_qs_floor_sweep.rds"))$grid %>%
          transmute(q,s,floor,fourth=round(fourth,2),two_pt=round(two_pt,2),total=round(total,2),total_ex_up5=round(total_ex,2)), "T04_floor_sweep")
})

# ============================ FIG 12: estimation CI vs sweep =============
mk("F12 CI vs sweep", {
  sc <- readRDS(file.path(rds,"paper_numbers.rds"))$scenarios %>%
    mutate(cell=factor(name, levels=name[order(total)]))
  p <- ggplot(sc, aes(total, cell, color=factor(q))) +
    geom_errorbarh(aes(xmin=total_lo, xmax=total_hi), height=0, linewidth=1.2) + geom_point(size=2.8) +
    scale_color_viridis_d(name="q", option="C", end=.85) +
    labs(title="Sampling error is tiny; the q/s assumptions are the real uncertainty",
         subtitle="Each scenario: season total + 95% estimation CI (g_emp posterior + nfl4th conversion); floor 0.25",
         x="total win probability added (wp / team-season)", y=NULL,
         caption="CIs are ~+/-1-2% (barely visible); the spread ACROSS scenarios (13.7 -> 30.4) dwarfs them.")
  savep(p, "F12_ci_vs_sweep", 9, 5)
  savet(sc %>% transmute(name,q,s,total=round(total,2),lo=round(total_lo,2),hi=round(total_hi,2)), "T08b_estimation_ci")
})

# ============================ FIG 13: matchup sensitivity ================
mk("F13 matchup", {
  suppressPackageStartupMessages({ library(nfl4th); library(xgboost) })
  fd <- nfl4th:::fd_model(); tp <- xgb.load.raw(get("two_pt_model", getNamespace("nfl4th"))); gains <- -10:65
  mkX <- expand_grid(posteam_spread=seq(-10,10,2)) %>% mutate(total_line=44, posteam_total=(total_line+posteam_spread)/2)
  go_conv <- function(ytg,yl){ X<-as.matrix(transmute(mkX, down=4, ydstogo=ytg, yardline_100=yl, era3=0, era4=1,
      outdoors=1, retractable=0, dome=0, posteam_spread, total_line, posteam_total))
    pr<-predict(fd,X); M<-if(is.matrix(pr)) pr else matrix(pr,nrow(mkX),byrow=TRUE); apply(M,1,function(p) sum(p[gains>=ytg])) }
  two_conv <- function(){ X<-as.matrix(transmute(mkX, era2=0,era3=0,era4=1,outdoors=1,retractable=0,dome=0,posteam_spread,total_line,posteam_total)); as.numeric(predict(tp,X)) }
  d <- mkX %>% mutate(`4th-&-1`=100*go_conv(1,40), `4th-&-3`=100*go_conv(3,40), `4th-&-7`=100*go_conv(7,40), `2-pt`=100*two_conv()) %>%
    pivot_longer(c(`4th-&-1`,`4th-&-3`,`4th-&-7`,`2-pt`), names_to="play", values_to="conv")
  p <- ggplot(d, aes(posteam_total, conv, color=play)) + geom_line(linewidth=1) + geom_point(size=1.5) +
    scale_color_manual(values=c("4th-&-1"="#009E73","4th-&-3"="#0072B2","4th-&-7"="#56B4E9","2-pt"="#D55E00"), name=NULL) +
    labs(title="nfl4th already adjusts conversion for the matchup",
         subtitle="Conversion prob vs offense's vegas-implied team total (geometry fixed at opp 40 / the 2)",
         x="offense implied team total (worse offense / better defense  ->  better / worse)", y="conversion probability (%)",
         caption="4th-down conversion swings ~5-7 percentage points across the league; 2-pt ~20 (geometry fixed). The team-strength adjustment is built in.")
  savep(p, "F13_matchup_sensitivity", 9, 5); savet(pivot_wider(d, names_from=play, values_from=conv), "T_matchup_sensitivity")
})

# ============================ extra tables ================================
mk("T06 threshold + T10 examples", {
  savet(readRDS(file.path(rds,"breakeven_voi_season_behavioral.rds"))$threshold_sensitivity, "T06_threshold_sensitivity")
  sg <- readRDS(file.path(rds,"breakeven_voi_synthgrid.rds"))
  savet(sg$ex_4th %>% transmute(example=lab, ytg, yl, score=sd, go=round(go,3), FG=round(fg,3), punt=round(punt,3),
                                lineup=round(lineup,3), C_pp=round(Cpp,3), winner, gain_pp=round(gain,2)), "T10_examples_4th")
  savet(sg$ex_2pt %>% transmute(example=lab, score=sd, go2=round(go2,3), XP=round(XP,3), lineup=round(lineup,3),
                                C_pp=round(Cpp,3), winner, gain_pp=round(gain,2)), "T10_examples_2pt")
})

cat("\nDONE. Figures in", figd, "| tables in", tabd, "\n")
cat("figures:", paste(sort(list.files(figd,"\\.png$")), collapse=", "), "\n")
cat("tables:",  paste(sort(list.files(tabd,"\\.csv$")), collapse=", "), "\n")
