#' Retrieve player picks for a given gameweek
#'
#' @param PlayerId Player ID. Must be a single integer.
#' @param GW Gameweek number. Must be a single integer.
#'
#' @return Parsed player picks data.
#' @details Throws an error if no data is returned.
FPLAPIGetPlayerGWPicks <- function(PlayerId, GW, ...) {
  .assert_single_integer(PlayerId, "PlayerId")
  .assert_single_integer(GW, "GW")
  res <- .FPLAPIGet(paste0("/entry/", PlayerId, "/event/", GW, "/picks"), ...)
  if (length(res) == 0) {
    stop("No picks data returned for player ", PlayerId, " in gameweek ", GW)
  }
  res
}

