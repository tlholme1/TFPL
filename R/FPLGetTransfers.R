#' Retrieve transfers made by managers in a league
#'
#' @param LeagueCode League code to query. Must be a single integer.
#' @param GW Gameweek number. Must be a single integer.
#'
#' @return A data.table detailing transfers for the specified gameweek.
#' @details Throws an error if the data cannot be retrieved.
FPLGetTransfers <- function(LeagueCode, GW){
  .assert_single_integer(LeagueCode, "LeagueCode")
  .assert_single_integer(GW, "GW")
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode=LeagueCode)
  if (!is.data.table(LeagueInfo) || nrow(LeagueInfo) == 0) {
    stop("Failed to retrieve league information.")
  }
  PlayerIds <- LeagueInfo$PlayerId
  names(PlayerIds) <- LeagueInfo$player_name
  TransfersList  <- lapply(X = PlayerIds, FUN = function(x){
    y <- FPLAPIGetPlayerTransfers(x)
    if (length(y) == 0) return(data.table())
    rbindlist(y)
  })
  Transfers <- rbindlist(TransfersList)
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode)
  PlayerInfo <- FPLGetPlayerInfo()
  Transfers[LeagueInfo, on = "PlayerId",Name := i.player_name]
  Transfers[PlayerInfo, on = list(element_in = id), PlayerIn := i.web_name]
  Transfers[PlayerInfo, on = list(element_out = id), PlayerOut := i.web_name]
  setnames(Transfers, "event", "GW")
  res <- Transfers[GW == ..GW, list(Name, PlayerOut, CostOut = element_out_cost/10, PlayerIn, CostIn = element_in_cost/10)]
  if (!is.data.table(res) || nrow(res) == 0) {
    stop("No transfers found for gameweek ", GW)
  }
  return(res)
}
