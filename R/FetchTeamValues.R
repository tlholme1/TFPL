#' Fetch Team Values
#'
#' Fetches team value data for each gameweek from 1 to LastGW using FPLGetTeamValue().
#' Combines the results from each gameweek into a single data.table.
#'
#' @param LeagueCode A string representing the league code. Must be a single integer.
#' @param LastGW An integer representing the last gameweek to fetch. Must be a single integer.
#'
#' @return A data.table with team value data tagged by gameweek.
#' @details Throws an error if data for any gameweek cannot be retrieved.
FetchTeamValues <- function(LeagueCode, LastGW) {
  .assert_single_integer(LeagueCode, "LeagueCode")
  .assert_single_integer(LastGW, "LastGW")
  gws <- seq_len(as.integer(LastGW))
  fetched <- lapply(gws, function(gw) {
    res <- FPLGetTeamValue(LeagueCode = LeagueCode, GW = gw)
    if (!is.data.table(res) || nrow(res) == 0) {
      stop("No team value returned for GW ", gw)
    }
    res
  })

  # Combine fetched results into a single data.table; each record tagged with its gameweek
  combined <- rbindlist(l = fetched, idcol = "GW", fill = TRUE)

  return(combined)
}
