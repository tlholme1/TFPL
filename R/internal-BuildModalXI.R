#' Build a modal XI from candidate players
#'
#' Helper for selecting a starting XI based on a selection metric while
#' honouring squad composition rules (one goalkeeper, three defenders, one
#' midfielder and one forward). Remaining spots are filled by the highest
#' selection metric regardless of position.
#'
#' @param players data.table of candidate players. Must contain columns
#'   `PlayerId`, `ElementType` and the column referenced by `metric`.
#' @param metric Column containing the selection metric (unquoted).
#'
#' @return data.table of the chosen XI.
#' @noRd
.BuildModalXI <- function(players, metric) {
  metric_col <- deparse(substitute(metric))
  setorder(players, -get(metric_col))

  GK  <- players[ElementType == 1][1]
  DEF <- players[ElementType == 2][1:3]
  MID <- players[ElementType == 3][1]
  FWD <- players[ElementType == 4][1]
  picked <- rbindlist(list(GK, DEF, MID, FWD))

  remaining <- players[!PlayerId %in% picked$PlayerId]
  spots_left <- 11 - nrow(picked)
  if (spots_left > 0) {
    picked <- rbind(picked, remaining[1:spots_left])
  }

  picked
}
