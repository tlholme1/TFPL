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
  setorder(Counts, -SelectedCount)

  GK  <- Counts[ElementType == 1][1]
  DEF <- Counts[ElementType == 2][1:3]
  MID <- Counts[ElementType == 3][1]
  FWD <- Counts[ElementType == 4][1]
  Picked <- rbindlist(list(GK, DEF, MID, FWD))

  Remaining <- Counts[!PlayerId %in% Picked$PlayerId]
  SpotsLeft <- 11 - nrow(Picked)
  if (SpotsLeft > 0) {
    Picked <- rbind(Picked, Remaining[1:SpotsLeft])
  }

  return(Picked)
}
