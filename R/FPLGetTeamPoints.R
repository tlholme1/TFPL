#' Retrieve team points for a league and gameweek
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table of total points per manager.
FPLGetTeamPoints <- function(LeagueCode,GW){
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode=LeagueCode)
  PlayerIds <- LeagueInfo$PlayerId
  names(PlayerIds) <- LeagueInfo$player_name
  ListOfTeams <- lapply(X = PlayerIds, FUN = function(x){
    y <- FPLAPIGetPlayerGWPicks(x, GW)
    y$entry_history$points
  })
  TeamTable <- t(data.frame(ListOfTeams))
  Output <- data.table(TeamTable,keep.rownames = TRUE)[order(-V1)]
  setnames(Output, "V1","Points")
  return(Output)
}
