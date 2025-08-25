#' Retrieve league captains for a gameweek
#'
#' Obtain a data table of players marked as captains in a given league and gameweek.
#'
#' @param LeagueCode FPL league code to query. Must be a single integer.
#' @param GW Gameweek number. Must be a single integer.
#'
#' @return A data.table filtered to players flagged as captains for the specified gameweek.
#' @details Throws an error if the data cannot be retrieved.

FPLGetCaptains <- function(LeagueCode, GW){
  .assert_single_integer(LeagueCode, "LeagueCode")
  .assert_single_integer(GW, "GW")
  res <- FPLGetSelectedPlayersForALeague(LeagueCode, GW)
  if (!is.data.table(res) || nrow(res) == 0) {
    stop("No player data returned for league")
  }
  res[is_captain == TRUE]
}
