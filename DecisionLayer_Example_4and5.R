# Read-only: the 4th-&-5 from own 50 example through the built decision layer.
# Fallback if you line up and bail = PUNT from midfield (yardline_100 = 50).
rds <- file.path(getwd(), "build")
cg <- readRDS(file.path(rds,"decisionlayer_lineup_cost.rds"))$combined
cat("=== 4th-&-5 from OWN 50 (yl100=50): line up & decide, fallback = PUNT, hold 3 ===\n")
ex <- subset(cg, action=="punt" & yardline==50 & current_to==3 & margin %in% c(-7,-3,0,3) & gsr %in% c(2700,1500,600))
ex <- ex[order(ex$margin, -ex$gsr), c("gsr","margin","to_leg","to_cost","dog_cost","lineup_cost","cheaper_fallback")]
print(ex, row.names=FALSE)
cat("\n=== the dead-zone effect: DOG-on-punt cost by field position (tied, gsr1500) ===\n")
cat("    (5-yd DOG barely moves a midfield punt; it bites only when backed up)\n")
ct <- subset(cg, action=="punt" & current_to==3 & margin==0 & gsr==1500)
ct$los <- ifelse(ct$yardline<=50, paste0("opp/mid ", ct$yardline), paste0("own ", 100-ct$yardline))
print(ct[order(ct$yardline), c("yardline","los","dog_cost")], row.names=FALSE)
