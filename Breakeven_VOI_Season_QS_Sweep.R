# ============================================================================
# Breakeven_VOI_Season_QS_Sweep.R -- seasonal WP add (pp/team-season) over a
# (q, s) grid, using the FROZEN unified behavioral model (threshold 1.5, GAM
# g_emp, 0.1-pp floor, 2-pt lead cap, clean core+reclaim). Only `lin` (the
# line-up WP) depends on (q,s): lin = q*go_boosted(s) + (1-q)*kick - cost(q).
# All WPs, C, g_emp, go_boost are fixed. -> breakeven_voi_season_qs_sweep.rds
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse) })
rds_dir <- file.path(getwd(), "build")
FLOOR<-0.10; THR<-1.5
p_boost<-function(p,s) ifelse(p<=0.5, p*(1+s), p+s*(1-p))
wk<-function(g) suppressWarnings(as.integer(str_split_fixed(g,"_",4)[,2]))
MBREAKS<-c(-Inf,-5,-3,-2,-1,0,1,2,4,Inf); MLAB<-c("down 5+","down 3-4","down 2","down 1","tied","up 1","up 2","up 3-4","up 5+")
tbin_of<-function(g) case_when(g<=120~"final 2 min",g<=360~"final 6 min",TRUE~"earlier")
cap_of<-function(g) case_when(g>1800~14,g>900~10,TRUE~7)
mk_lk<-function(gt,gm){ gt<-gt%>%mutate(mbin=as.character(mbin)); gm<-gm%>%mutate(mbin=as.character(mbin))
  function(m,t){ r<-gt%>%filter(mbin==m,tbin==t); if(nrow(r)==1 && r$n>=30) return(r$g_emp); v<-gm$g_emp[gm$mbin==m]; if(length(v)) v else 0 } }

# ---- 4th-down fixed parts --------------------------------------------------
g4o<-readRDS(file.path(rds_dir,"behavioral_4th_gorate.rds"))
d4<-readRDS(file.path(rds_dir,"breakeven_voi.rds"))$data %>% mutate(week=wk(game_id)) %>% filter(week<=18) %>%
  left_join(g4o$gkey, by=c("game_id","play_id"))
n_ts<-n_distinct(paste(d4$season,d4$posteam))
p4<-d4$first_down_prob
d4$wp_go<-p4*d4$wp_succeed+(1-p4)*d4$wp_fail
d4$best_out<-pmax(d4$wp_go,d4$kick_wp,na.rm=TRUE)
g4<-d4$g_hat_15
value4<-function(q,s){ pb<-p_boost(p4,s); go_b<-pb*d4$wp_succeed+(1-pb)*d4$wp_fail
  lin<-q*go_b+(1-q)*d4$kick_wp - pmin(1,1.5*(1-q))*d4$C_pp/100
  core<-pmax(0,lin-d4$best_out)
  reclaim<-ifelse(d4$go_boost>=THR,0,(1-g4)*pmax(0,pmin(lin,d4$wp_go)-d4$kick_wp))
  vpp<-100*(core+reclaim); ifelse(vpp>=FLOOR,vpp,0) }

# ---- 2-pt fixed parts ------------------------------------------------------
g2o<-readRDS(file.path(rds_dir,"behavioral_2pt_gorate.rds"))$tables$thr_1.5
d2<-readRDS(file.path(rds_dir,"breakeven_voi_2pt.rds"))$data %>% mutate(week=wk(game_id)) %>% filter(week<=18)
p2<-d2$conv_2pt
d2$wp_go2<-p2*d2$wp_2+(1-p2)*d2$wp_0; d2$xp<-d2$wp_go1
d2$gb2<-100*(d2$wp_go2-d2$xp); d2$best_out2<-pmax(d2$wp_go2,d2$xp,na.rm=TRUE)
d2$mbin<-as.character(cut(d2$score_differential,MBREAKS,labels=MLAB))
d2$in_play<-d2$score_differential<=cap_of(d2$game_seconds_remaining)
g2<-mapply(mk_lk(g2o$g_table,g2o$g_margin), d2$mbin, tbin_of(d2$game_seconds_remaining))
value2<-function(q,s){ pb<-p_boost(p2,s); go2b<-pb*d2$wp_2+(1-pb)*d2$wp_0
  lin<-q*go2b+(1-q)*d2$xp - pmin(1,1.5*(1-q))*d2$C_pp/100
  core<-pmax(0,lin-d2$best_out2)
  reclaim<-ifelse(d2$gb2>=THR,0,(1-g2)*pmax(0,pmin(lin,d2$wp_go2)-d2$xp))
  vpp<-100*(core+reclaim); ifelse(d2$in_play & vpp>=FLOOR,vpp,0) }

# ---- the grid --------------------------------------------------------------
grid<-expand_grid(q=c(0.5,0.6,0.65,0.7), s=c(0.18,0.20,0.22,0.25))
res<-grid %>% mutate(
  fourth   = map2_dbl(q,s,~sum(value4(.x,.y))/n_ts),
  two_pt   = map2_dbl(q,s,~sum(value2(.x,.y))/n_ts),
  two_pt_ex= map2_dbl(q,s,~{v<-value2(.x,.y); sum(v[d2$mbin!="up 5+"])/n_ts}),
  total    = fourth+two_pt,
  total_ex = fourth+two_pt_ex,
  wins     = total/100,
  wins_ex  = total_ex/100)
cat(sprintf("team-seasons %d | thr %.1f | 4th n=%d | 2-pt n=%d | (GAM g_emp, 0.1pp floor, lead cap)\n\n",
            n_ts, THR, nrow(d4), nrow(d2)))
cat("=== Seasonal WP add per team per REG season (pp), by (q, s) ===\n")
print(as.data.frame(res %>% transmute(q, s, fourth=round(fourth,2), two_pt=round(two_pt,2),
  total=round(total,2), total_ex_up5=round(total_ex,2), wins=round(wins,3), wins_ex=round(wins_ex,3))), row.names=FALSE)

cat("\n=== TOTAL (pp) as a q x s matrix ===\n")
print(as.data.frame(res %>% select(q,s,total) %>% mutate(total=round(total,2)) %>%
  pivot_wider(names_from=s, values_from=total, names_prefix="s=")), row.names=FALSE)
cat("\n=== TOTAL ex-(2pt up-5+) (pp) as a q x s matrix ===\n")
print(as.data.frame(res %>% select(q,s,total_ex) %>% mutate(total_ex=round(total_ex,2)) %>%
  pivot_wider(names_from=s, values_from=total_ex, names_prefix="s=")), row.names=FALSE)

saveRDS(list(grid=res, thr=THR, floor=FLOOR, team_seasons=n_ts,
  method="Seasonal WP add (pp/team-season) over (q,s) grid. Frozen unified behavioral model (thr 1.5, GAM g_emp, clean core+reclaim, 0.1pp floor, 2-pt lead cap). Only lin depends on (q,s).",
  built=Sys.time()), file.path(rds_dir,"breakeven_voi_season_qs_sweep.rds"))
cat("\nSaved breakeven_voi_season_qs_sweep.rds\n")
