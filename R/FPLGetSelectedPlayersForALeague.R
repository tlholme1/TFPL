FPLGetSelectedPlayersForALeague <- function(LeagueCode, GW){

  LeagueInfo <- FPLGetLeagueInfo(LeagueCode)

  EntriesToLoop <- LeagueInfo$entry
  names(EntriesToLoop) <- LeagueInfo$player_name

  ListOfTeams <- lapply(X = EntriesToLoop,FUN = FPLGetUserTeam, GW = GW)

  TeamTable <- rbindlist(ListOfTeams,idcol = "player_name")

  Output <- TeamTable[, list(player_name, Name,Status,is_captain)]

  return(Output)
}
