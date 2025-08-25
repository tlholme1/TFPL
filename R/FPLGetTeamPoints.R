#' Retrieve team points for a league and gameweek
#'
#' @param LeagueCode League code to query. Must be a single integer.
#' @param GW Gameweek number. Must be a single integer.
#'
#' @return A data.table of total points per manager.
#' @details Throws an error if the data cannot be retrieved.
FPLGetTeamPoints <- function(LeagueCode,GW){
  .assert_single_integer(LeagueCode, "LeagueCode")
  .assert_single_integer(GW, "GW")
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode=LeagueCode)
  if (!is.data.table(LeagueInfo) || nrow(LeagueInfo) == 0) {
    stop("Failed to retrieve league information.")
  }
  PlayerIds <- LeagueInfo$PlayerId
  names(PlayerIds) <- LeagueInfo$player_name
  ListOfTeams <- lapply(X = PlayerIds, FUN = function(x){
    y <- FPLAPIGetPlayerGWPicks(x, GW)
    if (length(y) == 0 || is.null(y$entry_history$points)) return(NA)
    y$entry_history$points
  })
  TeamTable <- t(data.frame(ListOfTeams))
  Output <- data.table(TeamTable,keep.rownames = TRUE)[order(-V1)]
  setnames(Output, "V1","Points")
  return(Output)
}
