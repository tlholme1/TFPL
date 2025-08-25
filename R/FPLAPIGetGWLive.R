#' Retrieve live gameweek data
#'
#' @param GW Gameweek number.
#'
#' @return Parsed live data.
FPLAPIGetGWLive <- function(GW, ...) {
  .FPLAPIGet(paste0("/event/", GW, "/live/"), ...)
}

