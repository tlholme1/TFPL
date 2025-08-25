#' Find the most similar teams within a league
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return A data.table of pairwise similarity scores.
FPLFindMostSimilarTeams <- function(LeagueCode, GW){
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode)
  setnames(LeagueInfo, "player_name", "PlayerName")
  PlayerIds <- LeagueInfo$PlayerId
  names(PlayerIds) <- LeagueInfo$PlayerName
  Teams <- lapply(PlayerIds, FPLGetUserTeam, GW = GW)
  Teams <- lapply(Teams, function(Dt) Dt[Status == "Selected", element])
  Combos <- combn(names(Teams), 2, simplify = FALSE)
  Sims <- rbindlist(lapply(Combos, function(X){
    data.table(Manager1 = X[1],
               Manager2 = X[2],
               Sim = FPLCompareTeams(Teams[[X[1]]], Teams[[X[2]]]))
  }))
  setorder(Sims, -Sim)
  return(Sims)
}
