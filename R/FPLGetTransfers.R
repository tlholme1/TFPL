FPLGetTransfers <- function(LeagueCode, GW){
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode=LeagueCode)

  EntriesToLoop <- LeagueInfo$entry
  names(EntriesToLoop) <- LeagueInfo$player_name

  TransfersList  <- lapply(X = EntriesToLoop,FUN = function(x){
    x <- GET(url = paste0("https://fantasy.premierleague.com/api/entry/",x,"/transfers"))

    y <- content(x)
    Output <- rbindlist(y)
    return(Output)
  })

  Transfers <- rbindlist(TransfersList)
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode)
  PlayerInfo <- FPLGetPlayerInfo()
  Transfers[LeagueInfo, on = "entry",Name := i.player_name]
  Transfers[PlayerInfo, on = list(element_in = id), PlayerIn := i.web_name]
  Transfers[PlayerInfo, on = list(element_out = id), PlayerOut := i.web_name]

  Output <- Transfers[event == GW]

  return(Output)
}
