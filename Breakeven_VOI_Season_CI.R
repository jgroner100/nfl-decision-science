# ============================================================================
# Breakeven_VOI_Season_CI.R -- ESTIMATION-uncertainty band on the season WP add.
# Monte Carlo (B draws) over the two tractable, dominant sources:
#   (1) g_emp GAM posterior  -- coef ~ MVN(coef, Vp_unconditional), GLOBAL per draw
#       (correlated across plays -> does NOT average out in the season sum).
#   (2) nfl4th conversion CI -- first_down_prob ~ Beta(p*n_eff, (1-p)*n_eff),
#       per play (idiosyncratic -> largely AVERAGES OUT in the season sum).
# FIXED (held at point): cost C (decision-layer item #1 is 2nd-order here, since C
#   enters only the small `cost` subtraction), the nfl4th WP components
#   (wp_succeed/fail/fg/punt), and conv_2pt (its CI was never built). 2-pt leg gets
#   its g_emp2 bin uncertainty (Beta on bin n) only.
# Decomposes the 4th-down band into g_emp-only vs conversion-only at the default cell.
# -> breakeven_voi_season_ci.rds
# ============================================================================
suppressPackageStartupMessages({ library(tidyverse); library(mgcv); library(MASS) })
select <- dplyr::select
rds <- file.path(getwd(), "build")
B <- 1000; set.seed(42); FLOOR<-0.10; THR<-1.5; n_ts<-64
p_boost <- function(p,s) ifelse(p<=0.5, p*(1+s), p+s*(1-p))
wk <- function(g) suppressWarnings(as.integer(stringr::str_split_fixed(g,"_",4)[,2]))
MBREAKS<-c(-Inf,-5,-3,-2,-1,0,1,2,4,Inf); MLAB<-c("down 5+","down 3-4","down 2","down 1","tied","up 1","up 2","up 3-4","up 5+")
tbin_of<-function(g) dplyr::case_when(g<=120~"final 2 min",g<=360~"final 6 min",TRUE~"earlier")
cap_of<-function(g) dplyr::case_when(g>1800~14,g>900~10,TRUE~7)
qgrid <- expand_grid(q=c(0.5,0.6,0.65,0.7), s=c(0.18,0.20,0.22,0.25))

# ---------------- 4th-down inputs ----------------
g4o <- readRDS(file.path(rds,"behavioral_4th_gorate.rds")); g_gam <- g4o$model
conv <- readRDS(file.path(rds,"nfl4th_ci_conv.rds"))$ci %>% select(game_id, play_id, n_eff)
d4 <- readRDS(file.path(rds,"breakeven_voi.rds"))$data %>% mutate(week=wk(game_id)) %>% filter(week<=18) %>%
  left_join(conv, by=c("game_id","play_id"))
N <- nrow(d4)
ws<-d4$wp_succeed; wf<-d4$wp_fail; kick<-d4$kick_wp; Cpp<-d4$C_pp; ppt<-d4$first_down_prob; neff<-d4$n_eff
# point checks
gpt <- as.numeric(predict(g_gam, newdata=d4, type="response"))
cat(sprintf("g_emp point vs saved g_hat_15: max|diff| = %.4g\n", max(abs(gpt - d4$g_hat_15))))
cat(sprintf("go_boost reconstruction max|diff| = %.4g pp\n", max(abs(100*((ppt*ws+(1-ppt)*wf)-kick) - d4$go_boost))))

# draws: g_emp (global, correlated) and conversion (per play)
Lp <- predict(g_gam, newdata=d4, type="lpmatrix")
V  <- tryCatch(vcov(g_gam, unconditional=TRUE), error=function(e) vcov(g_gam))
coefB <- MASS::mvrnorm(B, coef(g_gam), V)                 # B x ncoef
G  <- plogis(Lp %*% t(coefB))                            # N x B  (g_emp draws)
a <- ppt*neff + 0.5; b <- (1-ppt)*neff + 0.5
P  <- matrix(rbeta(N*B, a, b), N, B)                     # N x B  (conversion draws)

# value engine for the 4th leg given conversion matrix Pm and g matrix Gm (N x B), one (q,s)
val4_mat <- function(Pm, Gm, q, s){
  costv <- pmin(1,1.5*(1-q))*Cpp/100
  Pb <- p_boost(Pm, s)
  WPgo <- Pm*ws + (1-Pm)*wf
  GOb  <- Pb*ws + (1-Pb)*wf
  LIN  <- q*GOb + (1-q)*kick - costv
  GB   <- 100*(WPgo - kick)
  BEST <- pmax(WPgo, kick)
  CORE <- pmax(0, LIN - BEST)
  REC  <- (GB < THR) * (1-Gm) * pmax(0, pmin(LIN, WPgo) - kick)
  VPP  <- 100*(CORE + REC); VPP[VPP < FLOOR] <- 0
  colSums(VPP)/n_ts
}
mat <- function(v) matrix(v, N, B)                       # recycle a point vector to N x B

# ---------------- 2-pt inputs (g_emp2 bin Beta; conv_2pt fixed) ----------------
g2tab <- readRDS(file.path(rds,"behavioral_2pt_gorate.rds"))$tables$thr_1.5
gt <- g2tab$g_table %>% mutate(mbin=as.character(mbin)); gm <- g2tab$g_margin %>% mutate(mbin=as.character(mbin))
margN <- gt %>% group_by(mbin) %>% summarise(nmarg=sum(n), .groups="drop")
d2 <- readRDS(file.path(rds,"breakeven_voi_2pt.rds"))$data %>% mutate(week=wk(game_id)) %>% filter(week<=18) %>%
  mutate(mbin=as.character(cut(score_differential,MBREAKS,labels=MLAB)), tbin=tbin_of(game_seconds_remaining),
         in_play=score_differential<=cap_of(game_seconds_remaining))
# per play: which bin (cell n>=30 else margin), its g_emp and n
d2 <- d2 %>% left_join(gt, by=c("mbin","tbin")) %>% left_join(gm %>% rename(g_marg=g_emp), by="mbin") %>%
  left_join(margN, by="mbin") %>%
  mutate(use_cell = !is.na(n) & n>=30,
         binid = if_else(use_cell, paste(mbin,tbin), paste0("marg:",mbin)),
         g2pt = if_else(use_cell, g_emp, g_marg),
         n2   = if_else(use_cell, as.numeric(n), as.numeric(nmarg)))
N2 <- nrow(d2)
# draw g2 per unique bin (within-bin correlated), map to plays
bins <- d2 %>% distinct(binid, g2pt, n2)
A2 <- bins$g2pt*bins$n2 + 0.5; B2 <- (1-bins$g2pt)*bins$n2 + 0.5
G2bins <- matrix(rbeta(nrow(bins)*B, A2, B2), nrow(bins), B)
rownames(G2bins) <- bins$binid
G2 <- G2bins[d2$binid, ]                                  # N2 x B
p2<-d2$conv_2pt; wp2<-d2$wp_2; wp0<-d2$wp_0; xp<-d2$wp_go1
inplay<-d2$in_play
val2_mat <- function(q,s){
  cost2 <- pmin(1,1.5*(1-q))*d2$C_pp/100
  go2b <- p_boost(p2,s)*wp2 + (1-p_boost(p2,s))*wp0
  wp_go2 <- p2*wp2+(1-p2)*wp0
  lin2 <- q*go2b + (1-q)*xp - cost2
  gb2 <- 100*(wp_go2-xp); best2<-pmax(wp_go2,xp)
  core2 <- pmax(0, lin2-best2)                            # fixed across draws
  CORE2 <- mat2(core2)
  REC2  <- (mat2(gb2)<THR) * (1-G2) * pmax(0, pmin(mat2(lin2), mat2(wp_go2)) - mat2(xp))
  VPP2  <- 100*(CORE2+REC2); VPP2[!inplay,] <- 0; VPP2[VPP2<FLOOR] <- 0
  list(all=colSums(VPP2)/n_ts, exup5=colSums(VPP2[d2$mbin!="up 5+",])/n_ts)
}
mat2 <- function(v) matrix(v, N2, B)

# ---------------- run: full (q,s) grid CI ----------------
qi <- function(x) quantile(x, c(.025,.5,.975), names=FALSE)
rows <- pmap(qgrid, function(q,s){
  s4 <- val4_mat(P, G, q, s)                              # both sources
  v2 <- val2_mat(q,s)
  tot <- s4 + v2$all; totx <- s4 + v2$exup5
  c4<-qi(s4); ct<-qi(tot); ctx<-qi(totx)
  tibble(q,s, fourth_pt=mean(s4), fourth_lo=c4[1], fourth_hi=c4[3],
         total_pt=mean(tot), total_lo=ct[1], total_md=ct[2], total_hi=ct[3],
         total_ex_pt=mean(totx), total_ex_lo=ctx[1], total_ex_hi=ctx[3]) })
res <- bind_rows(rows)

# ---------------- decomposition at default cell ----------------
dq<-0.5; ds<-0.20
both  <- val4_mat(P,           G,            dq, ds)
gonly <- val4_mat(mat(ppt),    G,            dq, ds)      # conversion fixed at point
conly <- val4_mat(P,           mat(gpt),     dq, ds)      # g_emp fixed at point
point4<- val4_mat(mat(ppt),    mat(gpt),     dq, ds)[1]   # all fixed = point estimate
v2d <- val2_mat(dq,ds)
hw <- function(x) (quantile(x,.975,names=FALSE)-quantile(x,.025,names=FALSE))/2

cat(sprintf("\n=== DEFAULT q=%.2f s=%.2f floor %.2f thr %.1f (B=%d) ===\n", dq,ds,FLOOR,THR,B))
cat(sprintf("4th-down: point %.2f | MC mean %.2f | 95%% CI [%.2f, %.2f]  (half-width %.2f)\n",
            point4, mean(both), quantile(both,.025,names=FALSE), quantile(both,.975,names=FALSE), hw(both)))
cat(sprintf("   decomposition of 4th-down band (95%% half-width):  g_emp-only %.3f | conversion-only %.3f | both %.3f\n",
            hw(gonly), hw(conly), hw(both)))
cat(sprintf("2-pt: point %.2f | 95%% CI [%.2f, %.2f]  (g_emp2 bins only; conv_2pt fixed)\n",
            mean(v2d$all), quantile(v2d$all,.025,names=FALSE), quantile(v2d$all,.975,names=FALSE)))
tot_d <- both + v2d$all
cat(sprintf("TOTAL: point %.2f | 95%% CI [%.2f, %.2f]   (~wins %.3f [%.3f, %.3f])\n",
            mean(tot_d), quantile(tot_d,.025,names=FALSE), quantile(tot_d,.975,names=FALSE),
            mean(tot_d)/100, quantile(tot_d,.025,names=FALSE)/100, quantile(tot_d,.975,names=FALSE)/100))

cat("\n=== TOTAL 95% CI per (q,s) cell (pp/team-season) ===\n")
print(as.data.frame(res %>% transmute(q,s, total=round(total_pt,2),
  lo=round(total_lo,2), hi=round(total_hi,2),
  ex_up5=round(total_ex_pt,2), ex_lo=round(total_ex_lo,2), ex_hi=round(total_ex_hi,2))), row.names=FALSE)

saveRDS(list(grid=res, B=B, default=list(q=dq,s=ds, fourth=both, twopt=v2d$all, total=tot_d,
  decomp_halfwidth=c(g_emp_only=hw(gonly), conversion_only=hw(conly), both=hw(both))),
  method="Estimation-uncertainty MC on the season WP add. Sources: g_emp GAM posterior (global, correlated) + nfl4th conversion Beta(p*n_eff) (per-play, averages out). Fixed: cost C, nfl4th WP components, conv_2pt. 2-pt: g_emp2 bin Beta only. q/s parameter uncertainty handled separately by the sweep (dominant).",
  built=Sys.time()), file.path(rds,"breakeven_voi_season_ci.rds"))
cat("\nSaved breakeven_voi_season_ci.rds\n")
