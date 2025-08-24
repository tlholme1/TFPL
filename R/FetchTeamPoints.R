#' Fetch Team Points
#'
#' Fetches team points data for each gameweek from 1 to LastGW using FPLGetTeamPoints().
#' Combines the results from each gameweek into a single data.table.
#'
#' @param LeagueCode A string representing the league code.
#' @param LastGW An integer representing the last gameweek to fetch.
#'
#' @return A data.table with team value data tagged by gameweek.
FetchTeamPoints <- function(LeagueCode, LastGW) {
  gws <- seq_len(as.integer(LastGW))
  fetched <- lapply(gws, function(gw) {
    res <- tryCatch(
      FPLGetTeamPoints(LeagueCode = LeagueCode, GW = gw),
      error = function(e) {
        stop(sprintf("Error fetching data for GW %s: %s", gw, e$message))
      }
    )
    if (is.null(res)) stop(sprintf("FPLGetTeamPoints returned NULL for GW %s", gw))
    return(res)
  })

  # Combine fetched results into a single data.table; each record tagged with its gameweek
  combined <- rbindlist(l = fetched, idcol = "GW")

  combined[order(rn), CumPoints := cumsum(Points), by = rn]

  return(combined)
}
