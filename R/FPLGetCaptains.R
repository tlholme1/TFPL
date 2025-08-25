#' Retrieve league captains for a gameweek
#'
#' Obtain a data table of players marked as captains in a given league and gameweek.
#'
#' @param LeagueCode FPL league code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table filtered to players flagged as captains for the specified gameweek.
#'
FPLGetCaptains <- function(LeagueCode, GW){
  Players <- FPLGetSelectedPlayersForALeague(LeagueCode,GW)
  Output <- Players[is_captain == TRUE]
  return(Output)
}
