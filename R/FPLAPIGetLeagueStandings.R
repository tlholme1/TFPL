#' Retrieve standings for a classic league
#'
#' @param LeagueCode League code to query. Must be a single integer.
#'
#' @return Parsed league standings.
#' @details Throws an error if no data is returned.
FPLAPIGetLeagueStandings <- function(LeagueCode, ...) {
  .assert_single_integer(LeagueCode, "LeagueCode")
  res <- .FPLAPIGet(paste0("/leagues-classic/", LeagueCode, "/standings/"), ...)
  if (length(res) == 0) {
    stop("No league standings returned for code ", LeagueCode)
  }
  res
}

