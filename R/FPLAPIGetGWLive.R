#' Retrieve live gameweek data
#'
#' @param GW Gameweek number. Must be a single integer.
#'
#' @return Parsed live data.
#' @details Throws an error if no data is returned for the gameweek.
FPLAPIGetGWLive <- function(GW, ...) {
  .assert_single_integer(GW, "GW")
  res <- .FPLAPIGet(paste0("/event/", GW, "/live/"), ...)
  if (length(res) == 0) {
    stop("No live data returned for gameweek ", GW)
  }
  res
}

