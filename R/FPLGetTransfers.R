#' Retrieve transfers made by managers in a league
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table detailing transfers for the specified gameweek.
FPLGetTransfers <- function(LeagueCode, GW){
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode=LeagueCode)
  PlayerIds <- LeagueInfo$PlayerId
  names(PlayerIds) <- LeagueInfo$player_name
  TransfersList  <- lapply(X = PlayerIds, FUN = function(x){
    y <- FPLAPIGetPlayerTransfers(x)
    rbindlist(y)
  })
  Transfers <- rbindlist(TransfersList)
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode)
  PlayerInfo <- FPLGetPlayerInfo()
  Transfers[LeagueInfo, on = "PlayerId",Name := i.player_name]
  Transfers[PlayerInfo, on = list(element_in = id), PlayerIn := i.web_name]
  Transfers[PlayerInfo, on = list(element_out = id), PlayerOut := i.web_name]
  setnames(Transfers, "event", "GW")
  Output <- Transfers[GW == ..GW, list(Name, PlayerOut, CostOut = element_out_cost/10, PlayerIn, CostIn = element_in_cost/10)]
  return(Output)
}
