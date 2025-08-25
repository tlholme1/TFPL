#' Retrieve live player points for a gameweek
#'
#' @param GW Gameweek number.
#'
#' @return A data.table of player points for the specified gameweek.
FPLGetGameweekPoints <- function(GW){
  y <- FPLAPIGetGWLive(GW)
  ListOfData <- lapply(X = y$elements, function(x){
    data.table(id = x$id, Points = x$stats$total_points)
  })
  Picks <- rbindlist(ListOfData)
  y <- FPLGetPlayerInfo()[, list(id, web_name)]
  Picks[y, on = "id", Name := i.web_name]
  Output <- Picks[order(-Points)]
  return(Output)
}
