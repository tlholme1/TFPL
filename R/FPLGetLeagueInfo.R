#' Retrieve standings for an FPL league
#'
#' @param LeagueCode Integer league code to query.
#'
#' @return A data.table of league standings.
FPLGetLeagueInfo <- function(LeagueCode = 997983){
  LeagueInfo <- FPLAPIGetLeagueStandings(LeagueCode)
  LeaguesTable <- rbindlist(LeagueInfo$standings$results)
  setnames(LeaguesTable, "entry", "PlayerId")
  return(LeaguesTable)
}
