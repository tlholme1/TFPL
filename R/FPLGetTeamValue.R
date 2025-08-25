#' Retrieve team values for a league and gameweek
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table of team values per manager.
FPLGetTeamValue <- function(LeagueCode,GW){
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode=LeagueCode)
  PlayerIds <- LeagueInfo$PlayerId
  names(PlayerIds) <- LeagueInfo$player_name
  ListOfTeams <- lapply(X = PlayerIds, FUN = function(x){
    y <- FPLAPIGetPlayerGWPicks(x, GW)
    y$entry_history$value
  })
  TeamTable <- t(data.frame(ListOfTeams))
  Output <- data.table(TeamTable,keep.rownames = TRUE)[order(-V1)]
  setnames(Output, "V1","TeamValue")
  return(Output)
}
