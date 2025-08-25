#' Retrieve live player points for a gameweek
#'
#' @param GW Gameweek number. Must be a single integer.
#'
#' @return A data.table of player points for the specified gameweek.
#' @details Throws an error if the data cannot be retrieved.
FPLGetGameweekPoints <- function(GW){
  .assert_single_integer(GW, "GW")
  y <- FPLAPIGetGWLive(GW)
  if (length(y) == 0 || is.null(y$elements)) {
    stop("Failed to retrieve gameweek points.")
  }
  ListOfData <- lapply(X = y$elements, function(x){
    data.table(id = x$id, Points = x$stats$total_points)
  })
  Picks <- rbindlist(ListOfData)
  y <- FPLGetPlayerInfo()[, list(id, web_name)]
  Picks[y, on = "id", Name := i.web_name]
  Output <- Picks[order(-Points)]
  return(Output)
}
