#' Points scored by the league modal XI in a gameweek
#'
#' Wrapper around `.FPLGetModalXIPoints` using `FPLGetLeagueModalXI` to supply the
#' league modal XI.
#'
#' @param LeagueCode League code to query.
#' @param GW Gameweek number.
#'
#' @return Total points scored by the league modal XI in the gameweek.
FPLGetLeagueXIPoints <- function(LeagueCode, GW){
  .FPLGetModalXIPoints(GW = GW,
                       modal_fun = FPLGetLeagueModalXI,
                       LeagueCode = LeagueCode)
}

