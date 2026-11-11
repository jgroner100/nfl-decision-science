# ============================================================================
# DecisionLayer_Assemble.R  —  THE DECISION LAYER.
#   cost_of_lining_up(state) = min( DOG_cost(action, state), TO_cost(state) )
# All legs in WP percentage points, calling-team margin (negative = trailing).
#   TO leg : H1 (gsr > 1800, re-anchored to V(score))   -> phase9b_h1_burncost_reanchored.rds
#            H2 (gsr <= 1800, DOG-escape stripped)        -> h2_phase7L_decisionlayer.rds
#   DOG leg: action-specific v8 grid                      -> dog_grid_lookup.rds
# Interp: bilinear in (gsr, margin); yardline interp for FG/punt; rating nearest
# (H2 only; H1 & DOG are league-average). Edge flags on the half-seam sub-gap
# (1800-1950 in-drill) and the late-game clamp (gsr<120). Saves combined grid +
# worked examples + meta. Reusable: source() this file to get the functions.
# ============================================================================
suppressPackageStartupMessages(library(tidyverse))
rds <- file.path(getwd(), "build")
h1  <- as.data.frame(readRDS(file.path(rds,"phase9b_h1_burncost_reanchored.rds")))
h2  <- as.data.frame(readRDS(file.path(rds,"h2_phase7L_decisionlayer.rds")))
dog <- as.data.frame(readRDS(file.path(rds,"dog_grid_lookup.rds"))$grid)
HALF_SEAM <- 1800; RATING_GRID <- c(-10,-6,-3,0,3,6,10)

# ---- bilinear interp on a regular (x,y,v) grid; clamp outside the hull -------
bilin <- function(df, xcol, ycol, vcol, xq, yq){
  xs <- sort(unique(df[[xcol]])); ys <- sort(unique(df[[ycol]]))
  xq <- min(max(xq,min(xs)),max(xs)); yq <- min(max(yq,min(ys)),max(ys))
  x0 <- max(xs[xs<=xq]); x1 <- min(xs[xs>=xq]); y0 <- max(ys[ys<=yq]); y1 <- min(ys[ys>=yq])
  gv <- function(a,b) df[[vcol]][df[[xcol]]==a & df[[ycol]]==b][1]
  tx <- if(x1==x0) 0 else (xq-x0)/(x1-x0); ty <- if(y1==y0) 0 else (yq-y0)/(y1-y0)
  (1-tx)*(1-ty)*gv(x0,y0) + (1-tx)*ty*gv(x0,y1) + tx*(1-ty)*gv(x1,y0) + tx*ty*gv(x1,y1)
}

# ---- TO leg (half-seam routing + edge flags) --------------------------------
to_cost <- function(gsr, margin, current_to, rating=0){
  if (gsr > HALF_SEAM){
    d <- h1[h1$current_to==current_to,]
    note <- if (gsr < 1950) "H1 clamped@1950 (in-drill)" else "H1"
    list(leg="H1", cost=bilin(d,"gsr","score_differential","burn_wp",gsr,margin), note=note)
  } else {
    rq <- RATING_GRID[which.min(abs(RATING_GRID-rating))]
    d  <- h2[h2$current_to==current_to & h2$rating_calling==rq,]
    note <- if (gsr < 120) "H2 clamped@120 (late; TO worth less)" else paste0("H2 r=",rq)
    list(leg="H2", cost=bilin(d,"gsr_q","score_margin","burn_cost_stripped",gsr,margin), note=note)
  }
}

# ---- DOG leg (action-specific; yardline interp for FG/punt) ------------------
dog_cost <- function(action, gsr, margin, yardline=NA){
  g <- dog[dog$action==action,]
  if (action=="2pt") return(bilin(g,"gsr","score_margin","dog_cost_wp",gsr,margin))
  yls <- sort(unique(g$yardline)); ylq <- min(max(yardline,min(yls)),max(yls))
  y0 <- max(yls[yls<=ylq]); y1 <- min(yls[yls>=ylq])
  v0 <- bilin(g[g$yardline==y0,],"gsr","score_margin","dog_cost_wp",gsr,margin)
  v1 <- bilin(g[g$yardline==y1,],"gsr","score_margin","dog_cost_wp",gsr,margin)
  ty <- if(y1==y0) 0 else (ylq-y0)/(y1-y0); (1-ty)*v0 + ty*v1
}

# ---- THE DECISION LAYER -----------------------------------------------------
compute_lineup_cost <- function(gsr, margin, current_to, action, yardline=NA, rating=0){
  to <- to_cost(gsr, margin, current_to, rating); dgc <- dog_cost(action, gsr, margin, yardline)
  tibble(gsr, margin, current_to, action, yardline, rating, to_leg=to$leg,
         to_cost=round(to$cost,3), dog_cost=round(dgc,3),
         lineup_cost=round(min(to$cost,dgc),3),
         cheaper_fallback=if_else(dgc<to$cost,"DOG","TO"), note=to$note)
}

# ---- worked examples (default set; calling team = team facing 4th down) ------
wx <- function(label,...) compute_lineup_cost(...) %>% mutate(scenario=label,.before=1)
WX <- bind_rows(
  wx("2pt PAT down-1, Q2 8:00,  hold 3", 2280,-1,3,"2pt"),
  wx("2pt PAT down-1, Q4 10:00, hold 3", 600,-1,3,"2pt"),
  wx("2pt PAT down-1, Q4 10:00, hold 1", 600,-1,1,"2pt"),
  wx("2pt PAT down-1, deciding 0:15,hold3",15,-1,3,"2pt"),
  wx("45-yd FG, Q2 8:00 tied,   hold 3", 2280,0,3,"FG",yardline=27),
  wx("45-yd FG, Q4 10:00 trail-3,hold 3",600,-3,3,"FG",yardline=27),
  wx("punt own-20, Q2 8:00 tied, hold 3",2280,0,3,"punt",yardline=80),
  wx("punt deadzone(opp40),Q2 tied,hold3",2280,0,3,"punt",yardline=40)
)
cat("=== WORKED EXAMPLES: cost of lining up = min(DOG, TO), WP% ===\n")
print(as.data.frame(WX %>% select(scenario,to_leg,to_cost,dog_cost,lineup_cost,cheaper_fallback,note)), row.names=FALSE)

# ---- combined grid (rating-neutral) for the article / downstream ------------
GSRs <- c(2700,2400,2280,2100,1950,1500,1200,900,600,360,240,120,60,15)
MARs <- c(-14,-10,-7,-4,-3,-1,0,1,3,4,7,10,14); TOs <- 1:3
to_tab <- expand_grid(gsr=GSRs, margin=MARs, current_to=TOs) %>% rowwise() %>%
  mutate(r=list(to_cost(gsr,margin,current_to,0))) %>%
  mutate(to_leg=r$leg, to_cost=r$cost, to_note=r$note) %>% ungroup() %>% select(-r)
dog_tab <- bind_rows(
  expand_grid(gsr=GSRs,margin=MARs) %>% rowwise() %>% mutate(action="2pt",yardline=NA_real_,dog_cost=dog_cost("2pt",gsr,margin)) %>% ungroup(),
  expand_grid(gsr=GSRs,margin=MARs,yardline=c(10,15,20,25,30,35)) %>% rowwise() %>% mutate(action="FG",dog_cost=dog_cost("FG",gsr,margin,yardline)) %>% ungroup(),
  expand_grid(gsr=GSRs,margin=MARs,yardline=c(40,50,60,70,80,90)) %>% rowwise() %>% mutate(action="punt",dog_cost=dog_cost("punt",gsr,margin,yardline)) %>% ungroup())
combined <- dog_tab %>% inner_join(to_tab, by=c("gsr","margin"), relationship="many-to-many") %>%
  mutate(lineup_cost=pmin(to_cost,dog_cost), cheaper_fallback=if_else(dog_cost<to_cost,"DOG","TO")) %>%
  transmute(action,yardline,gsr,margin,current_to,to_leg,
            to_cost=round(to_cost,3),dog_cost=round(dog_cost,3),lineup_cost=round(lineup_cost,3),
            cheaper_fallback,to_note)
cat(sprintf("\ncombined grid rows: %d  (rating=0)\n", nrow(combined)))

saveRDS(list(combined=combined, worked_examples=WX, half_seam=HALF_SEAM,
  method="cost_of_lining_up = min(DOG(action,state), TO(state)); TO=H1(gsr>1800,re-anchored)/H2(gsr<=1800,DOG-escape-stripped); DOG=v8 grid. WP%, calling-team margin (neg=trailing). bilinear (gsr,margin); rating nearest (H2 only). Edge clamps flagged.",
  built=Sys.time()), file.path(rds,"decisionlayer_lineup_cost.rds"))
cat("Saved decisionlayer_lineup_cost.rds (combined grid + worked examples).\n")
