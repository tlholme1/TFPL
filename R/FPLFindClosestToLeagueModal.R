#' Compare league teams to the league modal XI
#'
#' Convenience wrapper around `FPLFindClosestToModalXI` using the
#' league modal XI supplier.
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table with similarity scores to the league's modal XI.
FPLFindClosestToLeagueModal <- function(LeagueCode, GW){
  FPLFindClosestToModalXI(LeagueCode, GW, FPLGetLeagueModalXI)
}
