#' Retrieve a user's team for a gameweek
#'
#' @param PlayerId Player ID of the manager. Must be a single integer.
#' @param GW Gameweek number. Must be a single integer.
#'
#' @return A data.table of the user's squad with names and selection status.
#' @details Throws an error if the data cannot be retrieved.
FPLGetUserTeam <- function(PlayerId, GW){
  .assert_single_integer(PlayerId, "PlayerId")
  .assert_single_integer(GW, "GW")
  y <- FPLAPIGetPlayerGWPicks(PlayerId, GW)
  if (length(y) == 0 || is.null(y$picks)) {
    stop("Failed to retrieve user team.")
  }
  Picks <- rbindlist(y$picks)
  PlayerInfo <- FPLGetPlayerInfo()
  if (!is.data.table(PlayerInfo) || nrow(PlayerInfo) == 0) {
    stop("Player information unavailable.")
  }
  Picks[PlayerInfo, on = list(element = id),Name := i.web_name]
  Picks[, Status := fifelse(position %in% 1:11, "Selected","Bench")]
  return(Picks)
}
