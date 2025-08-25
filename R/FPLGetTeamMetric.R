#' Retrieve team metric for a league and gameweek
#'
#' @param LeagueCode League code to query. Must be a single integer.
#' @param GW Gameweek number. Must be a single integer.
#' @param metric One of "points" or "value".
#'
#' @return A data.table of the chosen metric per manager.
#' @details Throws an error if the data cannot be retrieved.
FPLGetTeamMetric <- function(LeagueCode, GW, metric = c("points", "value")) {
  metric <- match.arg(metric)
  .assert_single_integer(LeagueCode, "LeagueCode")
  .assert_single_integer(GW, "GW")
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode = LeagueCode)
  if (!is.data.table(LeagueInfo) || nrow(LeagueInfo) == 0) {
    stop("Failed to retrieve league information.")
  }
  PlayerIds <- LeagueInfo$PlayerId
  names(PlayerIds) <- LeagueInfo$player_name
  ListOfTeams <- lapply(X = PlayerIds, FUN = function(x) {
    y <- FPLAPIGetPlayerGWPicks(x, GW)
    field <- switch(metric, points = "points", value = "value")
    if (length(y) == 0 || is.null(y$entry_history[[field]])) return(NA)
    y$entry_history[[field]]
  })
  TeamTable <- t(data.frame(ListOfTeams))
  Output <- data.table(TeamTable, keep.rownames = TRUE)[order(-V1)]
  name <- if (metric == "points") "Points" else "TeamValue"
  setnames(Output, "V1", name)
  return(Output)
}
