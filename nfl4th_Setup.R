# ============================================================================
# nfl4th_Setup.R — guarantee nfl4th's xgboost models are cached locally so
# add_4th_probs()/add_2pt_probs() NEVER depend on a live GitHub fetch.
#
# nfl4th v1.0.7 loads its WP + 4th-down (FD) xgboost models from
#   tools::R_user_dir("nfl4th","cache")/{wp_model,fd_model}.rds
# fetching from GitHub releases (model_archive/) only if that cache is empty.
# This script keeps a PROJECT-LOCAL backup of those two files and restores the
# user cache from the backup, so the pipeline works even if (a) the user cache
# is wiped AND (b) the GitHub release later 404s. Idempotent; run once to seed.
# ============================================================================
suppressPackageStartupMessages(library(nfl4th))
proj       <- getwd()
backup_dir <- file.path(proj, "build", "nfl4th_models")
cache_dir  <- tools::R_user_dir("nfl4th", "cache")
url_base   <- "https://github.com/nflverse/nfl4th/releases/download/model_archive/"
if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)
if (!dir.exists(cache_dir))  dir.create(cache_dir,  recursive = TRUE)

cat("=== nfl4th model cache setup ===\n")
cat("nfl4th version:", as.character(packageVersion("nfl4th")), "\n")
cat("cache dir :", cache_dir, "\n")
cat("backup dir:", backup_dir, "\n\n")

ensure_model <- function(fn) {
  cache_path <- file.path(cache_dir, fn); backup_path <- file.path(backup_dir, fn)
  # (1) ensure a project-local backup exists: prefer copying the live cache,
  #     else download from GitHub once.
  if (!file.exists(backup_path)) {
    if (file.exists(cache_path)) {
      file.copy(cache_path, backup_path, overwrite = TRUE)
      cat(sprintf("  [%s] backup created from live cache\n", fn))
    } else {
      cat(sprintf("  [%s] absent in cache; downloading from GitHub...\n", fn))
      options(timeout = 180)
      saveRDS(readRDS(url(paste0(url_base, fn))), backup_path)
      cat(sprintf("  [%s] downloaded -> backup\n", fn))
    }
  }
  # (2) ensure the user cache has it (restore from backup if missing)
  if (!file.exists(cache_path)) {
    file.copy(backup_path, cache_path, overwrite = TRUE)
    cat(sprintf("  [%s] cache restored from backup\n", fn))
  }
  # (3) integrity: file must readRDS to a raw vector that xgboost can load
  raw_ok <- tryCatch(is.raw(readRDS(cache_path)), error = function(e) FALSE)
  xgb_ok <- tryCatch({ xgboost::xgb.load.raw(readRDS(cache_path)); TRUE },
                     error = function(e) { cat(sprintf("  [%s] xgb load ERR: %s\n", fn, conditionMessage(e))); FALSE })
  cat(sprintf("  [%s] cache=%.0fKB backup=%.0fKB  raw=%s  xgb_loads=%s\n", fn,
              file.info(cache_path)$size/1024, file.info(backup_path)$size/1024, raw_ok, xgb_ok))
  invisible(raw_ok && xgb_ok)
}

ok <- all(vapply(c("wp_model.rds", "fd_model.rds"), ensure_model, logical(1)))
cat(sprintf("\nSetup %s. add_4th_probs()/add_2pt_probs() will use the local cache.\n",
            if (ok) "OK" else "FAILED"))

if (!ok) stop("The nfl4th model cache could not be initialized.", call. = FALSE)
