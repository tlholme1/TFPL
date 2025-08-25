#' Retrieve standings for a classic league
#'
#' @param LeagueCode League code to query.
#'
#' @return Parsed league standings.
FPLAPIGetLeagueStandings <- function(LeagueCode, ...) {
  .FPLAPIGet(paste0("/leagues-classic/", LeagueCode, "/standings/"), ...)
}

