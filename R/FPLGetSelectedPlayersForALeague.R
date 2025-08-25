#' Retrieve selected players for all teams in a league
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table with player selections for each manager.
FPLGetSelectedPlayersForALeague <- function(LeagueCode, GW){
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode)
  PlayerIds <- LeagueInfo$PlayerId
  names(PlayerIds) <- LeagueInfo$player_name
  ListOfTeams <- lapply(X = PlayerIds, FUN = FPLGetUserTeam, GW = GW)
  TeamTable <- rbindlist(ListOfTeams, idcol = "player_name")
  Output <- TeamTable[, list(player_name, Name, Status, is_captain)]
  return(Output)
}
