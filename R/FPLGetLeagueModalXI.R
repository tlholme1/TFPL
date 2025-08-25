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
  Counts <- TeamTable[Status == "Selected", .N, by = element]
  PlayerInfo <- FPLGetPlayerInfo()[,
    .(PlayerId = id, WebName = web_name, ElementType = element_type)]
  Counts[PlayerInfo, on = list(element = PlayerId), `:=`(WebName = i.WebName,
                                                         ElementType = i.ElementType)]
  setnames(Counts, old = c("element", "N"), new = c("PlayerId", "SelectedCount"))

  .BuildModalXI(Counts, SelectedCount)
}
