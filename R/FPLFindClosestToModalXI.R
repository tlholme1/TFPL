#' Compare league teams to a supplied modal XI
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#' @param modal_fun Function returning a data.table with a PlayerId column.
#'
#' @return A data.table with similarity scores to the modal XI.
FPLFindClosestToModalXI <- function(LeagueCode, GW, modal_fun){
  args <- list(GW = GW)
  if ("LeagueCode" %in% names(formals(modal_fun))) {
    args$LeagueCode <- LeagueCode
  }
  ModalXI <- do.call(modal_fun, args)$PlayerId
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode)
  setnames(LeagueInfo, "player_name", "PlayerName")
  PlayerIds <- LeagueInfo$PlayerId
  names(PlayerIds) <- LeagueInfo$PlayerName
  Teams <- lapply(PlayerIds, FPLGetUserTeam, GW = GW)
  Sims <- rbindlist(lapply(names(Teams), function(Nm){
    Team <- Teams[[Nm]][Status == "Selected", element]
    data.table(Manager = Nm,
               Sim = FPLCompareTeams(Team, ModalXI))
  }))
  setorder(Sims, -Sim)
  return(Sims)
}
