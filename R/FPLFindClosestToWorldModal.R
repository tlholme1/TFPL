#' Compare league teams to the world modal XI
#'
#' Convenience wrapper around `FPLFindClosestToModalXI` using the
#' world modal XI supplier.
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table with similarity scores to the world modal XI.
FPLFindClosestToWorldModal <- function(LeagueCode, GW){
  FPLFindClosestToModalXI(LeagueCode, GW, FPLGetModalXIWorld)
}
