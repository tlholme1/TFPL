#' Points scored by the league modal XI in a gameweek
#'
#' Calculates the total points obtained by a league's modal XI for a specified
#' gameweek. The modal XI is determined using `FPLGetLeagueModalXI`, which
#' enforces basic formation rules.
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return Total points scored by the league modal XI in the gameweek.
FPLGetLeagueXIPoints <- function(LeagueCode, GW){
  XI <- FPLGetLeagueModalXI(LeagueCode, GW)[, .(PlayerId)]
  Points <- FPLGetGameweekPoints(GW)[, .(PlayerId = id, Points)]
  XI[Points, on = "PlayerId", Points := i.Points]
  XI[is.na(Points), Points := 0]
  return(sum(XI$Points))
}

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

