#' Most selected players in a league
#'
#' @param LeagueCode League code to query. Must be a single integer.
#' @param GW Gameweek number. Must be a single integer.
#'
#' @return A data.table with player names and selection counts.
#' @details Throws an error if the data cannot be retrieved.
FPLGetMostSelectedPlayers <- function(LeagueCode, GW){
  .assert_single_integer(LeagueCode, "LeagueCode")
  .assert_single_integer(GW, "GW")
  Players <- FPLGetSelectedPlayersForALeague(LeagueCode,GW)
  if (!is.data.table(Players) || nrow(Players) == 0) {
    stop("No player data returned for league")
  }
  Output <- Players[Status == "Selected", .N, by = Name][order(-N)]
  return(Output)
}
