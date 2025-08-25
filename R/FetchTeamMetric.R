#' Fetch Team Metric
#'
#' Fetches team metric data for each gameweek from 1 to LastGW using FPLGetTeamMetric().
#' Combines the results from each gameweek into a single data.table.
#'
#' @param LeagueCode A string representing the league code. Must be a single integer.
#' @param LastGW An integer representing the last gameweek to fetch. Must be a single integer.
#' @param metric The metric to fetch, either "points" or "value".
#'
#' @return A data.table with team metric data tagged by gameweek.
#' @details Throws an error if data for any gameweek cannot be retrieved.
#' @keywords internal
.FetchTeamMetric <- function(LeagueCode, LastGW, metric = c("points", "value")) {
  metric <- match.arg(metric)
  .assert_single_integer(LeagueCode, "LeagueCode")
  .assert_single_integer(LastGW, "LastGW")
  gws <- seq_len(as.integer(LastGW))
  fetched <- lapply(gws, function(gw) {
    res <- FPLGetTeamMetric(LeagueCode = LeagueCode, GW = gw, metric = metric)
    if (!is.data.table(res) || nrow(res) == 0) {
      stop("No team ", metric, " returned for GW ", gw)
    }
    res
  })
  combined <- rbindlist(l = fetched, idcol = "GW", fill = TRUE)
  if (metric == "points") {
    combined[order(rn), CumPoints := cumsum(Points), by = rn]
  }
  return(combined)
}
