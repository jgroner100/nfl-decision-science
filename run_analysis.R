# Run from the repository root with Rscript --vanilla run_analysis.R.
stages <- list(
  setup = c(
    "nfl4th_Setup.R"
  ),
  delay = c(
    "DOG_v8_final.R",
    "DOG_Grid.R"
  ),
  timeouts = c(
    "H1_Build_Data.R",
    "Phase4_2Min_Probability.R",
    "Phase5_TO_Usage.R",
    "Phase6c_CallWeightedValue.R",
    "Phase6_TO_Survival.R",
    "Phase8_Bootstrap_Sensitivity.R",
    "Phase9_EarlyUse_H1.R",
    "Phase9b_H1_Reanchor.R",
    "H2/H2_Phase1_Calib.R",
    "H2/H2_Phase1_Overrides.R",
    "H2/H2_Phase4b_DefReachOpportunity.R",
    "H2/H2_Phase4a_OffReachOpportunity.R",
    "H2/H2_Phase5_TOUsage.R",
    "H2/H2_Phase4c_DefDemandModels.R",
    "H2/H2_Phase4a_RefitSimplified.R",
    "H2/H2_Phase6_TOSurvival.R",
    "H2/H2_Phase2_v11_CallWeightedFinal.R",
    "H2/H2_Phase2_v13_OffCells.R",
    "H2/H2_Phase7f_DriftCalibration.R",
    "H2/H2_Phase7k_TierEight.R",
    "H2/H2_Phase7L_DecisionLayer.R"
  ),
  lineup = c(
    "DecisionLayer_Assemble.R",
    "nfl4th_Baseline.R",
    "nfl4th_CI_conv.R",
    "FourthDown_Conversion.R",
    "FourthDown_Model.R",
    "nfl4th_CI_wp.R",
    "nfl4th_CI_competitive.R",
    "Breakeven_VOI.R",
    "Breakeven_VOI_2pt.R",
    "nfl4th_SynthGrid.R",
    "Behavioral_4th_GoRate.R",
    "Behavioral_2pt_GoRate.R",
    "Breakeven_VOI_2pt_Behavioral.R",
    "Breakeven_VOI_Season_Behavioral.R",
    "Breakeven_VOI_Season_QS_Sweep.R",
    "Breakeven_VOI_Season_QS_Floor_Sweep.R",
    "Breakeven_VOI_Season_CI.R",
    "Paper_Build.R",
    "Make_Figures.R"
  )
)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 1L || (length(args) == 1L && !args[[1L]] %in% c("delay", "timeouts", "all"))) {
  stop("Usage: Rscript --vanilla run_analysis.R [delay|timeouts|all]", call. = FALSE)
}
stage <- if (length(args)) args[[1L]] else "all"
scripts <- if (stage == "all") unlist(stages, use.names = FALSE) else stages[[stage]]
if (!all(file.exists(scripts))) {
  stop("Run this command from the repository root.", call. = FALSE)
}

packages <- c("nflfastR", "nflreadr", "nfl4th", "tidyverse", "dplyr",
              "stringr", "glue", "mgcv", "MASS", "xgboost", "pacman")
required <- if (stage == "all") packages else setdiff(packages, c("nfl4th", "xgboost"))
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  stop("Missing R packages: ", paste(missing, collapse = ", "),
       ". Run Rscript --vanilla install_packages.R first.", call. = FALSE)
}

dir.create("build", showWarnings = FALSE)
rscript <- file.path(R.home("bin"), "Rscript")
for (script in scripts) {
  message("\nRunning ", script)
  status <- system2(rscript, c("--vanilla", shQuote(script)))
  if (status != 0L) {
    stop(script, " exited with status ", status, ".", call. = FALSE)
  }
}
