#' Retrieve a user's team for a gameweek
#'
#' @param PlayerId Player ID of the manager.
#' @param GW Gameweek number.
#'
#' @return A data.table of the user's squad with names and selection status.
FPLGetUserTeam <- function(PlayerId, GW){
  y <- FPLAPIGetPlayerGWPicks(PlayerId, GW)
  Picks <- rbindlist(y$picks)
  PlayerInfo <- FPLGetPlayerInfo()
  Picks[PlayerInfo, on = list(element = id),Name := i.web_name]
  Picks[, Status := fifelse(position %in% 1:11, "Selected","Bench")]
  return(Picks)
}
