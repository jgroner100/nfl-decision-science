# ============================================================================
# Paper_Build.R -- numbers + figures for the article, at the user's framing:
#   FLOOR = 0.25 pp; four scenarios:
#     Ultra-conservative q=0.50 s=0.18 | Slightly conservative q=0.60 s=0.20
#     Base case q=0.65 s=0.20          | Aggressive q=0.65 s=0.22
# Computes per-scenario 4th/2pt/total/ex-up5 (point) + total/ex 95% estimation CI
# (g_emp GAM posterior + nfl4th conversion Beta), base-case decomposition + by-ytg
# + 2pt by-margin. Regenerates P_headline / P_value_by_ytg / P_qs_heatmap.
# -> paper_numbers.rds ; figures in figures/
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(mgcv); library(MASS) })
select <- dplyr::select
rds <- file.path(getwd(), "build")
figd <- file.path(rds, "figures"); dir.create(figd, showWarnings=FALSE)
B<-1000; set.seed(42); THR<-1.5; FLOOR<-0.25; n_ts<-64
p_boost<-function(p,s) ifelse(p<=0.5,p*(1+s),p+s*(1-p)); wk<-function(g) suppressWarnings(as.integer(str_split_fixed(g,"_",4)[,2]))
MBREAKS<-c(-Inf,-5,-3,-2,-1,0,1,2,4,Inf); MLAB<-c("down 5+","down 3-4","down 2","down 1","tied","up 1","up 2","up 3-4","up 5+")
tbin_of<-function(g) case_when(g<=120~"final 2 min",g<=360~"final 6 min",TRUE~"earlier"); cap_of<-function(g) case_when(g>1800~14,g>900~10,TRUE~7)
ytg_brk<-c(0,1,2,3,5,8,99); ytg_lab<-c("1","2","3","4-5","6-8","9+")
OK <- c(go="#009E73", FG="#E69F00", lineup="#D55E00", NEW="#D55E00")
theme_set(theme_minimal(base_size=14) + theme(panel.grid.minor=element_blank(), plot.title=element_text(face="bold"),
  plot.title.position="plot", legend.position="bottom", plot.subtitle=element_text(color="grey30"), plot.caption=element_text(color="grey45", size=9)))
savep<-function(p,n,w=8,h=5){ ggsave(file.path(figd,paste0(n,".png")),p,width=w,height=h,dpi=200,bg="white"); cat("fig:",n,"\n") }
SCEN <- tibble(name=c("Ultra-conservative","Slightly conservative","Base case","Aggressive"),
               q=c(0.50,0.60,0.65,0.65), s=c(0.18,0.20,0.20,0.22)) %>% mutate(lab=sprintf("%s\n(q=%.2f, s=%.2f)",name,q,s))

# ---- 4th inputs + draws ----
g4o<-readRDS(file.path(rds,"behavioral_4th_gorate.rds")); g_gam<-g4o$model
conv<-readRDS(file.path(rds,"nfl4th_ci_conv.rds"))$ci %>% select(game_id,play_id,n_eff)
d4<-readRDS(file.path(rds,"breakeven_voi.rds"))$data %>% mutate(week=wk(game_id)) %>% filter(week<=18) %>%
  left_join(g4o$gkey,by=c("game_id","play_id")) %>% left_join(conv,by=c("game_id","play_id"))
ws<-d4$wp_succeed; wf<-d4$wp_fail; kick<-d4$kick_wp; Cpp<-d4$C_pp; ppt<-d4$first_down_prob
neff<-d4$n_eff; neff[is.na(neff)]<-100; gpt<-d4$g_hat_15; N<-nrow(d4)
Lp<-predict(g_gam,newdata=d4,type="lpmatrix"); V<-tryCatch(vcov(g_gam,unconditional=TRUE),error=function(e) vcov(g_gam))
G<-plogis(Lp %*% t(MASS::mvrnorm(B,coef(g_gam),V))); aa<-ppt*neff+0.5; bb<-(1-ppt)*neff+0.5; Pm<-matrix(rbeta(N*B,aa,bb),N,B)
val4_mat<-function(P,Gm,q,s,floor){ costv<-pmin(1,1.5*(1-q))*Cpp/100; Pb<-p_boost(P,s)
  WPgo<-P*ws+(1-P)*wf; GOb<-Pb*ws+(1-Pb)*wf; LIN<-q*GOb+(1-q)*kick-costv; GB<-100*(WPgo-kick); BEST<-pmax(WPgo,kick)
  CORE<-pmax(0,LIN-BEST); REC<-(GB<THR)*(1-Gm)*pmax(0,pmin(LIN,WPgo)-kick); VPP<-100*(CORE+REC); VPP[VPP<floor]<-0; colSums(VPP)/n_ts }
val4_point<-function(q,s,floor){ costv<-pmin(1,1.5*(1-q))*Cpp/100; wpgo<-ppt*ws+(1-ppt)*wf
  go_b<-p_boost(ppt,s)*ws+(1-p_boost(ppt,s))*wf; lin<-q*go_b+(1-q)*kick-costv; gb<-100*(wpgo-kick); best<-pmax(wpgo,kick)
  core<-pmax(0,lin-best); rec<-ifelse(gb<THR,(1-gpt)*pmax(0,pmin(lin,wpgo)-kick),0); vpp<-100*(core+rec); keep<-vpp>=floor; vpp[!keep]<-0
  list(total=sum(vpp)/n_ts, core=sum(100*core[keep])/n_ts, reclaim=sum(100*rec[keep])/n_ts, vpp=vpp, forced=mean(gb>=THR)) }

# ---- 2pt inputs + draws ----
g2o<-readRDS(file.path(rds,"behavioral_2pt_gorate.rds"))$tables$thr_1.5
gt<-g2o$g_table %>% mutate(mbin=as.character(mbin)); gm<-g2o$g_margin %>% mutate(mbin=as.character(mbin)); margN<-gt %>% group_by(mbin) %>% summarise(nmarg=sum(n),.groups="drop")
d2<-readRDS(file.path(rds,"breakeven_voi_2pt.rds"))$data %>% mutate(week=wk(game_id)) %>% filter(week<=18) %>%
  mutate(mbin=as.character(cut(score_differential,MBREAKS,labels=MLAB)), tbin=tbin_of(game_seconds_remaining), in_play=score_differential<=cap_of(game_seconds_remaining)) %>%
  left_join(gt,by=c("mbin","tbin")) %>% left_join(gm %>% rename(g_marg=g_emp),by="mbin") %>% left_join(margN,by="mbin") %>%
  mutate(use_cell=!is.na(n)&n>=30, binid=if_else(use_cell,paste(mbin,tbin),paste0("marg:",mbin)), g2pt=if_else(use_cell,g_emp,g_marg), n2=if_else(use_cell,as.numeric(n),as.numeric(nmarg)))
N2<-nrow(d2); p2<-d2$conv_2pt; wp2<-d2$wp_2; wp0<-d2$wp_0; xp<-d2$wp_go1; inplay<-d2$in_play
bins<-d2 %>% distinct(binid,g2pt,n2); A2<-bins$g2pt*bins$n2+0.5; B2<-(1-bins$g2pt)*bins$n2+0.5
G2b<-matrix(rbeta(nrow(bins)*B,A2,B2),nrow(bins),B); rownames(G2b)<-bins$binid; G2<-G2b[d2$binid,]; mat2<-function(v) matrix(v,N2,B)
val2_mat<-function(q,s,floor){ cost2<-pmin(1,1.5*(1-q))*d2$C_pp/100; go2b<-p_boost(p2,s)*wp2+(1-p_boost(p2,s))*wp0; wpgo2<-p2*wp2+(1-p2)*wp0
  lin2<-q*go2b+(1-q)*xp-cost2; gb2<-100*(wpgo2-xp); best2<-pmax(wpgo2,xp); core2<-pmax(0,lin2-best2)
  REC2<-(mat2(gb2)<THR)*(1-G2)*pmax(0,pmin(mat2(lin2),mat2(wpgo2))-mat2(xp)); VPP2<-100*(mat2(core2)+REC2); VPP2[!inplay,]<-0; VPP2[VPP2<floor]<-0
  list(all=colSums(VPP2)/n_ts, ex=colSums(VPP2[d2$mbin!="up 5+",])/n_ts) }
val2_point<-function(q,s,floor){ cost2<-pmin(1,1.5*(1-q))*d2$C_pp/100; go2b<-p_boost(p2,s)*wp2+(1-p_boost(p2,s))*wp0; wpgo2<-p2*wp2+(1-p2)*wp0
  lin2<-q*go2b+(1-q)*xp-cost2; gb2<-100*(wpgo2-xp); best2<-pmax(wpgo2,xp); core2<-pmax(0,lin2-best2); rec2<-ifelse(gb2<THR,(1-d2$g2pt)*pmax(0,pmin(lin2,wpgo2)-xp),0)
  vpp<-100*(core2+rec2); vpp[!inplay]<-0; vpp[vpp<floor]<-0; list(all=sum(vpp)/n_ts, ex=sum(vpp[d2$mbin!="up 5+"])/n_ts, vpp=vpp) }

# ---- per-scenario point + CI ----
qi<-function(x) quantile(x,c(.025,.975),names=FALSE)
rows<-lapply(seq_len(nrow(SCEN)), function(i){ q<-SCEN$q[i]; s<-SCEN$s[i]
  p4<-val4_point(q,s,FLOOR); p2v<-val2_point(q,s,FLOOR)
  s4<-val4_mat(Pm,G,q,s,FLOOR); v2<-val2_mat(q,s,FLOOR); tot<-s4+v2$all; totx<-s4+v2$ex
  tibble(name=SCEN$name[i], q,s, fourth=p4$total, two_pt=p2v$all, total=p4$total+p2v$all, total_ex=p4$total+p2v$ex,
         wins=(p4$total+p2v$all)/100, wins_ex=(p4$total+p2v$ex)/100,
         total_lo=qi(tot)[1], total_hi=qi(tot)[2], total_ex_lo=qi(totx)[1], total_ex_hi=qi(totx)[2],
         core4=p4$core, reclaim4=p4$reclaim, forced4=p4$forced) })
res<-bind_rows(rows)
cat("\n=== FOUR SCENARIOS, FLOOR=0.25 (pp / team-season) ===\n")
print(as.data.frame(res %>% transmute(name,q,s, fourth=round(fourth,2), two_pt=round(two_pt,2),
  total=round(total,2), CI=sprintf("[%.1f, %.1f]",total_lo,total_hi), wins=round(wins,3),
  total_ex=round(total_ex,2), wins_ex=round(wins_ex,3))), row.names=FALSE)

# ---- reference detail (slightly conservative q=.60 s=.2) ----
bc<-val4_point(0.60,0.20,FLOOR); bc2<-val2_point(0.60,0.20,FLOOR)
byytg<-tibble(ytg=cut(d4$ydstogo,ytg_brk,labels=ytg_lab), v=bc$vpp) %>% group_by(ytg) %>% summarise(pp=sum(v)/n_ts,.groups="drop")
bymarg<-tibble(mbin=factor(d2$mbin,levels=MLAB), v=bc2$vpp) %>% group_by(mbin) %>% summarise(pp=sum(v)/n_ts,.groups="drop")
cat(sprintf("\nREFERENCE q=.60 s=.2 floor .25: 4th %.2f (core %.2f + reclaim %.2f, forced-go %.0f%%) + 2pt %.2f = %.2f (ex-up5 %.2f)\n",
  bc$total, bc$core, bc$reclaim, 100*bc$forced, bc2$all, bc$total+bc2$all, bc$total+bc2$ex))
cat("4th by ytg (pp/ts):\n"); print(as.data.frame(byytg %>% mutate(pp=round(pp,3))), row.names=FALSE)
cat("2pt by margin (pp/ts):\n"); print(as.data.frame(bymarg %>% mutate(pp=round(pp,3))), row.names=FALSE)

saveRDS(list(scenarios=res, base_byytg=byytg, base_bymarg=bymarg, floor=FLOOR, B=B,
  base=c(fourth=bc$total, core=bc$core, reclaim=bc$reclaim, two_pt=bc2$all, total=bc$total+bc2$all, total_ex=bc$total+bc2$ex, forced=bc$forced),
  built=Sys.time()), file.path(rds,"paper_numbers.rds"))

# ============================ figures =======================================
SCEN_ord <- res %>% mutate(name=factor(name, levels=c("Ultra-conservative","Slightly conservative","Base case","Aggressive")))
# P_headline: stacked 4th+2pt total + total CI + ex-up5 marker, by scenario
stk<-SCEN_ord %>% select(name,fourth,two_pt) %>% pivot_longer(c(fourth,two_pt),names_to="leg",values_to="pp") %>%
  mutate(leg=recode(leg, fourth="4th down", two_pt="2-pt"))
ph<-ggplot(stk, aes(name,pp,fill=leg))+geom_col(width=.6)+
  geom_errorbar(data=SCEN_ord, aes(x=name,ymin=total_lo,ymax=total_hi),inherit.aes=FALSE,width=.12,color="grey20")+
  geom_text(data=SCEN_ord, aes(x=name,y=total,label=sprintf("%.1f wp\n(%.2f wins)",total,wins)),inherit.aes=FALSE,vjust=-0.5,fontface="bold",size=3.8,lineheight=.85)+
  scale_fill_manual(values=c("4th down"=unname(OK["go"]),"2-pt"=unname(OK["FG"])),name=NULL)+ expand_limits(y=max(SCEN_ord$total)*1.18)+
  labs(title="What the line-up option is worth, across plausible assumptions",
       subtitle="win probability added per team per regular season (floor 0.25 wp); error bar = 95% estimation CI",
       x=NULL, y="win probability added (wp / team-season)",
       caption="q = P(good look -> go); s = perfect-look conversion boost. Reference: slightly conservative, q=0.60, s=0.20.")
savep(ph,"P_headline",9,5.6)

# P_value_by_ytg (base case)
pv<-ggplot(byytg, aes(ytg,pp))+geom_col(fill=unname(OK["NEW"]))+
  geom_text(aes(label=sprintf("%.1f",pp)),vjust=-0.3,size=3.8)+expand_limits(y=max(byytg$pp)*1.12)+
  labs(title="Where the 4th-down value comes from: 4th-&-3-to-8",
       subtitle="WP added per team-season by yards to go (reference q=0.60, s=0.20, floor 0.25)",
       x="yards to go", y="win probability added (wp / team-season)",
       caption="The medium-distance band is where teams punt too much and the read is most decision-relevant.")
savep(pv,"P_value_by_ytg")

# P_qs_heatmap at FLOOR (fresh grid, consistent with Table 2), mark the 4 scenarios
qgrid<-c(.50,.60,.65,.70); sgrid<-c(.18,.20,.22,.25)
fs<-expand_grid(q=qgrid,s=sgrid) %>% rowwise() %>%
  mutate(total=val4_point(q,s,FLOOR)$total+val2_point(q,s,FLOOR)$all) %>% ungroup()
markcells<-SCEN %>% select(q,s)
pq<-ggplot(fs, aes(factor(s),factor(q),fill=total))+geom_tile(color="white",linewidth=.5)+
  geom_text(aes(label=sprintf("%.1f\n%.2fw",total,total/100)),size=3.2,lineheight=.8)+
  geom_tile(data=markcells, aes(factor(s),factor(q)),inherit.aes=FALSE,fill=NA,color="black",linewidth=1.1)+
  scale_fill_viridis_c(name="wp / team-season",option="D")+
  labs(title="Season value across the two unmeasurable sliders (floor 0.25)",
       subtitle="Boxed cells are the four scenarios; q is the dominant lever",
       x="s (perfect-look conversion boost)", y="q (good-look frequency)",
       caption="Full total (incl. 2-pt up-5+). Values in wp and (wins).")
savep(pq,"P_qs_heatmap",9,5.2)
cat("\nDONE paper_numbers.rds + P_ figures\n")
