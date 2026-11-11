# Load and validate every requested nflverse season before fitting a model.
load_play_by_play <- function(seasons) {
  plays <- nflreadr::load_pbp(seasons)
  missing <- setdiff(seasons, unique(plays[["season"]]))
  if (length(missing) || !"play_type" %in% names(plays)) {
    stop(
      "Could not load complete nflverse play-by-play for seasons ",
      paste(if (length(missing)) missing else seasons, collapse = ", "),
      ". Check access to the nflverse-data release assets on GitHub.",
      call. = FALSE
    )
  }
  plays
}
