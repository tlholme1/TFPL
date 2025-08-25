#' Retrieve player picks for a given gameweek
#'
#' @param PlayerId Player ID.
#' @param GW Gameweek number.
#'
#' @return Parsed player picks data.
FPLAPIGetPlayerGWPicks <- function(PlayerId, GW, ...) {
  .FPLAPIGet(paste0("/entry/", PlayerId, "/event/", GW, "/picks"), ...)
}

