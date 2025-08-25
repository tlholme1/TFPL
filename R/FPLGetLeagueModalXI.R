#' Retrieve modal XI for a league
#'
#' Determines the eleven most selected players among all managers in a league
#' for a given gameweek.
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table with player identifiers and selection counts.
FPLGetLeagueModalXI <- function(LeagueCode, GW){
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode)
  Teams <- lapply(LeagueInfo$PlayerId, FPLGetUserTeam, GW = GW)
  TeamTable <- rbindlist(Teams)
  Counts <- TeamTable[Status == "Selected", .N, by = element][order(-N)]
  PlayerInfo <- FPLGetPlayerInfo()[, .(PlayerId = id, WebName = web_name)]
  Counts[PlayerInfo, on = list(element = PlayerId), WebName := i.WebName]
  setnames(Counts, old = c("element", "N"), new = c("PlayerId", "SelectedCount"))
  ModalXI <- Counts[1:min(11, .N)]
  return(ModalXI)
}
