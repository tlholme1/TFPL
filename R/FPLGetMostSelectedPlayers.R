#' Most selected players in a league
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table with player names and selection counts.
FPLGetMostSelectedPlayers <- function(LeagueCode, GW){
  Players <- FPLGetSelectedPlayersForALeague(LeagueCode,GW)
  Output <- Players[Status == "Selected", .N, by = Name][order(-N)]
  return(Output)
}
