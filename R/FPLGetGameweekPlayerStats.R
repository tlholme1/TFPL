#' Retrieve player statistics for a gameweek
#'
#' @param GW Gameweek number.
#'
#' @return A data.table with player statistics for the specified gameweek.
FPLGetGameweekPlayerStats <- function(GW){
  LiveData <- FPLAPIGetGWLive(GW)
  StatsList <- lapply(LiveData$elements, function(x){
    data.table(PlayerId = x$id, x$stats)
  })
  Stats <- rbindlist(StatsList, fill = TRUE)
  PlayerInfo <- FPLGetPlayerInfo()[, .(PlayerId = id, web_name)]
  Stats[PlayerInfo, on = "PlayerId", Name := i.web_name]
  return(Stats)
}
