FPLGetCaptains <- function(LeagueCode, GW){
  Players <- FPLGetSelectedPlayersForALeague(LeagueCode,GW)
  Output <- Players[is_captain == TRUE]
  return(Output)
}
