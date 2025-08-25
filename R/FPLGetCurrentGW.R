#' Retrieve current, next, and last finished gameweeks
#'
#' Queries the FPL API for global gameweek information.
#'
#' @return A list with elements `current`, `next`, and `finished` containing
#'   the respective gameweek numbers.
FPLGetCurrentGW <- function() {
  data <- FPLAPIGetHeaderData()
  GW <- as.data.table(data$events)
  Output <- list(
    current = GW[is_current == TRUE, id],
    `next` = GW[is_next == TRUE, id],
    finished = GW[finished == TRUE, max(id, na.rm = TRUE)]
  )
  return(Output)
}
