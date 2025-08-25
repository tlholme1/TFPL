#' Points scored by the world modal XI in a gameweek
#'
#' Calculates the total points obtained by the world modal XI for a
#' specified gameweek. The world modal XI is determined using global
#' selection percentages and adheres to basic squad rules.
#'
#' @param GW Gameweek number.
#'
#' @return Total points scored by the world modal XI in the gameweek.
FPLGetWorldXIPoints <- function(GW){
  XI <- FPLGetModalXIWorld(GW)[, .(PlayerId, WebName)]
  Points <- FPLGetGameweekPoints(GW)[, .(PlayerId = id, Points)]
  XI[Points, on = "PlayerId", Points := i.Points]
  XI[is.na(Points), Points := 0]
  return(sum(XI$Points))
}

