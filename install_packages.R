packages <- c("nflfastR", "nflreadr", "nfl4th", "tidyverse", "dplyr",
              "stringr", "glue", "mgcv", "MASS", "xgboost", "pacman")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}
