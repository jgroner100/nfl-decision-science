# ============================================================================
# nfl4th_SynthGrid.R — CLEAN canonical worked examples for the break-even model.
# Builds synthetic 4th-down + 2-pt states (neutral-game template; state passed as
# function args; nfl4th recomputes EP internally so the stale template EP is moot),
# runs nfl4th add_4th_probs/add_2pt_probs at the EXACT states, then the break-even
# model (s=0.20, q=0.50, C=decision layer). Replaces the cherry-picked real plays.
# Outputs: a 4th-down decision MAP (ydstogo x yardline, tied Q3) + canonical
# worked examples (4th + 2pt). Saves breakeven_voi_synthgrid.rds.
# ============================================================================
suppressPackageStartupMessages({ library(nfl4th); library(nflreadr); library(tidyverse) })
rds_dir <- file.path(getwd(), "build"); proj <- getwd()
S <- 0.20; Q <- 0.50
p_boost <- function(p,s) ifelse(p <= 0.5, p*(1+s), p + s*(1-p))
invisible(capture.output(source(file.path(proj,"DecisionLayer_Assemble.R"))))
C_4th <- function(gsr,margin,ct,action,yl){ d<-tryCatch(dog_cost(action,gsr,margin,yl),error=function(e)NA_real_); if(!is.finite(d))d<-Inf
  if(is.finite(ct)&&ct>=1){t<-tryCatch(to_cost(gsr,margin,as.integer(ct),0)$cost,error=function(e)Inf);min(t,d)}else d }
C_2pt <- function(gsr,margin,ct){ d<-tryCatch(dog_cost("2pt",gsr,margin),error=function(e)NA_real_); if(!is.finite(d))d<-Inf
  if(is.finite(ct)&&ct>=1){t<-tryCatch(to_cost(gsr,margin,as.integer(ct),0)$cost,error=function(e)Inf);min(t,d)}else d }

# ---- neutral templates -----------------------------------------------------
message("load_pbp(2023) + schedules for templates...")
source("load_play_by_play.R")
pbp <- load_play_by_play(2023); sched <- nflreadr::load_schedules(2023) %>% filter(game_type=="REG")
ng <- sched %>% filter(!is.na(spread_line)) %>% arrange(abs(spread_line)) %>% slice(1)
cat(sprintf("neutral 4th template game: %s (spread=%.1f, total=%.0f)\n", ng$game_id, ng$spread_line, ng$total_line))
tmpl4 <- pbp %>% filter(game_id==ng$game_id, posteam==home_team, !is.na(ep)) %>% slice(1)
tmplX <- pbp %>% filter(posteam==home_team, extra_point_attempt==1, !is.na(spread_line), abs(spread_line)<=1) %>% slice(1)

gsr_of <- function(qtr,qsr){ hsr <- if(qtr %in% c(2,4)) qsr else qsr+900; if(qtr<=2) hsr+1800 else hsr }
hsr_of <- function(qtr,qsr) if(qtr %in% c(2,4)) qsr else qsr+900
mk4 <- function(ytg,yl,sd,qtr,qsr,ct=3) tmpl4 %>% mutate(down=4L, ydstogo=as.integer(ytg), yardline_100=as.integer(yl),
  score_differential=as.integer(sd), home_score=21L, away_score=as.integer(21-sd),
  qtr=as.integer(qtr), quarter_seconds_remaining=as.integer(qsr),
  half_seconds_remaining=as.integer(hsr_of(qtr,qsr)), game_seconds_remaining=as.integer(gsr_of(qtr,qsr)),
  posteam_timeouts_remaining=as.integer(ct), defteam_timeouts_remaining=3L)
mk2 <- function(sd,qtr,qsr,ct=3) tmplX %>% mutate(score_differential=as.integer(sd),
  home_score=21L, away_score=as.integer(21-sd), qtr=as.integer(qtr), quarter_seconds_remaining=as.integer(qsr),
  half_seconds_remaining=as.integer(hsr_of(qtr,qsr)), game_seconds_remaining=as.integer(gsr_of(qtr,qsr)),
  posteam_timeouts_remaining=as.integer(ct), defteam_timeouts_remaining=3L)

# ---- break-even model ------------------------------------------------------
strat4 <- function(p,wS,wF,fg,punt,Cpp,q=Q,s=S){ pb<-p_boost(p,s)
  go<-p*wS+(1-p)*wF; gob<-pb*wS+(1-pb)*wF; kick<-pmax(fg,punt,na.rm=TRUE)
  lin<-q*gob+(1-q)*kick-pmin(1,1.5*(1-q))*Cpp/100
  M<-cbind(go,fg,punt,lin); M[is.na(M)]<- -Inf
  best<-pmax(go,fg,punt,na.rm=TRUE); tibble(go,fg,punt,lineup=lin,
    winner=c("go","FG","punt","lineup")[max.col(M,ties.method="first")], gain=100*(lin-best)) }
strat2 <- function(p,w2,w0,xp,Cpp,q=Q,s=S){ pb<-p_boost(p,s)
  go2<-p*w2+(1-p)*w0; gob<-pb*w2+(1-pb)*w0; lin<-q*gob+(1-q)*xp-pmin(1,1.5*(1-q))*Cpp/100
  M<-cbind(go2,xp,lin); M[is.na(M)]<- -Inf; best<-pmax(go2,xp,na.rm=TRUE)
  tibble(go2,xp,lineup=lin,winner=c("go2","XP","lineup")[max.col(M,ties.method="first")],gain=100*(lin-best)) }

# ---- (1) 4th-down DECISION MAP: ydstogo x yardline, tied, Q3 7:00 -----------
YTG<-c(1,2,3,4,5,7,10); YL<-c(20,30,40,50,60,70,80)
mapgrid <- expand_grid(ytg=YTG, yl=YL)
g4 <- bind_rows(Map(function(a,b) mk4(a,b,0,3,420), mapgrid$ytg, mapgrid$yl))
pr <- nfl4th::add_4th_probs(g4) %>% transmute(ydstogo, yardline_100, first_down_prob, wp_succeed, wp_fail, fg_wp, punt_wp)
mp <- mapgrid %>% bind_cols(pr %>% select(p=first_down_prob, wS=wp_succeed, wF=wp_fail, fg=fg_wp, punt=punt_wp)) %>%
  rowwise() %>% mutate(fb=if_else(!is.na(fg)&(is.na(punt)|fg>=punt),"FG","punt"),
                       Cpp=C_4th(gsr_of(3,420), 0, 3, fb, yl)) %>% ungroup()
res <- strat4(mp$p, mp$wS, mp$wF, mp$fg, mp$punt, mp$Cpp); mp <- bind_cols(mp, res %>% select(winner, gain))
cat("\n=== 4th-DOWN DECISION MAP (tied, Q3 7:00, 3 TOs) — best strategy by ydstogo x yardline_100 ===\n")
print(as.data.frame(mp %>% select(ytg, yl, winner) %>% pivot_wider(names_from=yl, values_from=winner)), row.names=FALSE)
cat("\nline-up GAIN over best outright (pp) where lineup wins:\n")
print(as.data.frame(mp %>% mutate(g=if_else(winner=="lineup", round(gain,2), NA_real_)) %>%
  select(ytg, yl, g) %>% pivot_wider(names_from=yl, values_from=g)), row.names=FALSE)

# ---- (2) canonical 4th-down worked examples --------------------------------
EX4 <- tribble(~lab,~ytg,~yl,~sd,~qtr,~qsr,
  "4th-&-3, midfield, tied, Q3",        3,50,0,3,420,
  "4th-&-1, own 40, tied, Q3",          1,60,0,3,420,
  "4th-&-5, own 45, tied, Q4 4:00",     5,55,0,4,240,
  "4th-&-7, opp 35, down 3, Q4 2:00",   7,35,-3,4,120,
  "4th-&-2, opp 8, tied, Q3",           2,8,0,3,420,
  "4th-&-10, opp 30, tied, Q3",         10,30,0,3,420)
g4e <- bind_rows(Map(function(a,b,c,d,e) mk4(a,b,c,d,e), EX4$ytg,EX4$yl,EX4$sd,EX4$qtr,EX4$qsr))
pe <- nfl4th::add_4th_probs(g4e) %>% transmute(first_down_prob, wp_succeed, wp_fail, fg_wp, punt_wp)
ex4 <- EX4 %>% bind_cols(pe) %>% rowwise() %>%
  mutate(fb=if_else(!is.na(fg_wp)&(is.na(punt_wp)|fg_wp>=punt_wp),"FG","punt"),
         Cpp=C_4th(gsr_of(qtr,qsr), sd, 3, fb, yl)) %>% ungroup()
r4 <- strat4(ex4$first_down_prob, ex4$wp_succeed, ex4$wp_fail, ex4$fg_wp, ex4$punt_wp, ex4$Cpp)
ex4 <- bind_cols(ex4, r4 %>% select(go,fg=fg,punt,lineup,winner,gain))
cat("\n=== CANONICAL 4th-DOWN EXAMPLES (synthetic; s=0.20 q=0.50; WP fractions) ===\n")
print(as.data.frame(ex4 %>% transmute(lab, p=round(first_down_prob,3), Cpp=round(Cpp,2),
  go=round(go,3), FG=round(fg,3), punt=round(punt,3), lineup=round(lineup,3), winner,
  gain=if_else(winner=="lineup", round(gain,2), NA_real_))), row.names=FALSE)

# ---- (3) canonical 2-pt worked examples ------------------------------------
EX2 <- tribble(~lab,~sd,~qtr,~qsr,
  "down 2, Q4 6:00",  -2,4,360,  "down 1, Q4 6:00", -1,4,360,
  "tied,  Q4 2:00",    0,4,120,  "up 1,   Q4 2:00",  1,4,120,
  "up 5,   Q3 7:00",   5,3,420)
g2e <- bind_rows(Map(function(a,b,c) mk2(a,b,c), EX2$sd,EX2$qtr,EX2$qsr))
p2 <- nfl4th::add_2pt_probs(g2e) %>% transmute(conv_2pt, wp_2, wp_0, wp_go1, wp_go2)
ex2 <- EX2 %>% bind_cols(p2) %>% rowwise() %>% mutate(Cpp=C_2pt(gsr_of(qtr,qsr), sd, 3)) %>% ungroup()
r2 <- strat2(ex2$conv_2pt, ex2$wp_2, ex2$wp_0, ex2$wp_go1, ex2$Cpp)
ex2 <- bind_cols(ex2, r2 %>% select(go2,XP=xp,lineup,winner,gain))
cat("\n=== CANONICAL 2-PT EXAMPLES (synthetic; s=0.20 q=0.50; WP fractions) ===\n")
print(as.data.frame(ex2 %>% transmute(lab, conv2=round(conv_2pt,3), Cpp=round(Cpp,2),
  go2=round(go2,3), XP=round(XP,3), lineup=round(lineup,3), winner,
  gain=if_else(winner=="lineup", round(gain,2), NA_real_))), row.names=FALSE)

saveRDS(list(map=mp, ex_4th=ex4, ex_2pt=ex2, template_game=ng$game_id, s=S, q=Q,
  method="Synthetic canonical worked examples. nfl4th add_4th_probs/add_2pt_probs at exact constructed states (neutral-game template; nfl4th recomputes EP internally so template EP is moot). Break-even model s=0.20 q=0.50, C=decision layer. Replaces cherry-picked real plays.",
  built=Sys.time()), file.path(rds_dir,"breakeven_voi_synthgrid.rds"))
cat("\nSaved breakeven_voi_synthgrid.rds\n")
