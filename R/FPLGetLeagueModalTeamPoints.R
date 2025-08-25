#' Points for the league modal XI across multiple gameweeks
#'
#' For each gameweek in the provided range, computes the league modal XI and
#' totals the points scored by that XI.
#'
#' @param LeagueCode League code to query.
#' @param GWs Vector of gameweek numbers. Defaults to all completed gameweeks.
#'
#' @return A data.table with the points scored by the league modal XI for each
#'   gameweek and a cumulative tally.
FPLGetLeagueModalTeamPoints <- function(LeagueCode,
                                        GWs = seq_len(FPLGetCurrentGW()$current)){
  Results <- lapply(GWs, function(gw){
    data.table(GW = gw, Points = FPLGetLeagueXIPoints(LeagueCode, gw))
  })
  out <- rbindlist(Results)
  out[, CumulativePoints := cumsum(Points)]
  return(out)
}
