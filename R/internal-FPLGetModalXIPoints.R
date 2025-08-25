#' Points scored by a modal XI in a gameweek
#'
#' Calculates the total points obtained by a modal XI for a specified
#' gameweek. The modal XI is supplied by the user via a function that
#' determines the squad to evaluate.
#'
#' @param GW Gameweek number.
#' @param modal_fun Function that returns a data.table describing the modal XI.
#'   It must accept a `GW` argument and may take additional parameters passed
#'   through `...`.
#' @param ... Additional arguments forwarded to `modal_fun`.
#'
#' @return Total points scored by the modal XI in the gameweek.
#' @noRd
.FPLGetModalXIPoints <- function(GW, modal_fun, ...){
  XI <- modal_fun(GW = GW, ...)
  XI <- XI[, .(PlayerId)]
  Points <- FPLGetGameweekPoints(GW)[, .(PlayerId = id, Points)]
  XI[Points, on = "PlayerId", Points := i.Points]
  XI[is.na(Points), Points := 0]
  return(sum(XI$Points))
}

