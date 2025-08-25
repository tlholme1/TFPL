#' Retrieve standings for an FPL league
#'
#' @param LeagueCode Integer league code to query. Must be a single integer.
#'
#' @return A data.table of league standings.
#' @details Throws an error if the data cannot be retrieved.
FPLGetLeagueInfo <- function(LeagueCode = 997983){
  .assert_single_integer(LeagueCode, "LeagueCode")
  LeagueInfo <- FPLAPIGetLeagueStandings(LeagueCode)
  if (length(LeagueInfo) == 0 || is.null(LeagueInfo$standings$results)) {
    stop("Failed to retrieve league information.")
  }
  LeaguesTable <- rbindlist(LeagueInfo$standings$results)
  setnames(LeaguesTable, "entry", "PlayerId")
  return(LeaguesTable)
}
