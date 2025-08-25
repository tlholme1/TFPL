#' Retrieve gameweek totals for each manager in a league
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table of total points per manager including display names.
FPLGetLeagueGameweekTotals <- function(LeagueCode, GW){
  League <- FPLAPIGetLeagueStandings(LeagueCode)
  Standings <- rbindlist(League$standings$results)[, .(EntryId = entry, DisplayName = player_name)]
  PlayerIds <- Standings$EntryId
  ListOfTeams <- lapply(PlayerIds, function(id){
    Picks <- rbindlist(FPLAPIGetPlayerGWPicks(id, GW)$picks)
    Picks[, EntryId := id]
    return(Picks)
  })
  Picks <- rbindlist(ListOfTeams)
  setnames(Picks, "element", "PlayerId")

  PlayerTotals <- FPLGetGameweekPlayerStats(GW)[, .(PlayerId, Points = total_points)]
  Picks[PlayerTotals, on = "PlayerId", Points := i.Points]
  Picks[, GWPoints := Points * multiplier]

  Totals <- Picks[, .(Points = sum(GWPoints, na.rm = TRUE)), by = EntryId]
  Totals[Standings, on = "EntryId", DisplayName := i.DisplayName]
  Totals <- Totals[order(-Points)]
  return(Totals)
}
