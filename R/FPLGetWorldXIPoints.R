#' Points scored by the world modal XI in a gameweek
#'
#' Wrapper around `.FPLGetModalXIPoints` using `FPLGetModalXIWorld` to supply the
#' global modal XI.
#'
#' @param GW Gameweek number.
#'
#' @return Total points scored by the world modal XI in the gameweek.
FPLGetWorldXIPoints <- function(GW){
  .FPLGetModalXIPoints(GW = GW, modal_fun = FPLGetModalXIWorld)
}

