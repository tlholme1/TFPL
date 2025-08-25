#' Compare league teams to the league modal XI
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table with similarity scores to the league's modal XI.
FPLFindClosestToLeagueModal <- function(LeagueCode, GW){
  ModalLeague <- FPLGetLeagueModalXI(LeagueCode, GW)$PlayerId
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode)
  setnames(LeagueInfo, "player_name", "PlayerName")
  PlayerIds <- LeagueInfo$PlayerId
  names(PlayerIds) <- LeagueInfo$PlayerName
  Teams <- lapply(PlayerIds, FPLGetUserTeam, GW = GW)
  Sims <- rbindlist(lapply(names(Teams), function(Nm){
    Team <- Teams[[Nm]][Status == "Selected", element]
    data.table(Manager = Nm,
               Sim = FPLCompareTeams(Team, ModalLeague))
  }))
  setorder(Sims, -Sim)
  return(Sims)
}
