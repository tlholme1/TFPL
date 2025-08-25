#' Retrieve selected players for all teams in a league
#'
#' @param LeagueCode League code to query. Must be a single integer.
#' @param GW Gameweek number. Must be a single integer.
#'
#' @return A data.table with player selections for each manager.
#' @details Throws an error if the data cannot be retrieved.
FPLGetSelectedPlayersForALeague <- function(LeagueCode, GW){
  .assert_single_integer(LeagueCode, "LeagueCode")
  .assert_single_integer(GW, "GW")
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode)
  if (!is.data.table(LeagueInfo) || nrow(LeagueInfo) == 0) {
    stop("Failed to retrieve league information.")
  }
  PlayerIds <- LeagueInfo$PlayerId
  names(PlayerIds) <- LeagueInfo$player_name
  ListOfTeams <- lapply(X = PlayerIds, FUN = FPLGetUserTeam, GW = GW)
  TeamTable <- rbindlist(ListOfTeams, idcol = "player_name")
  if (!is.data.table(TeamTable) || nrow(TeamTable) == 0) {
    stop("No player selections returned for league")
  }
  TeamTable[, list(player_name, Name, Status, is_captain)]
}
