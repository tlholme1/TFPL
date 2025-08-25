#' Retrieve current, next, and last finished gameweeks
#'
#' Queries the FPL API for global gameweek information.
#'
#' @return A list with elements `current`, `next`, and `finished` containing
#'   the respective gameweek numbers.
#' @details Throws an error if the data cannot be retrieved.
FPLGetCurrentGW <- function() {
  data <- FPLAPIGetHeaderData()
  if (length(data) == 0) {
    stop("Failed to retrieve current gameweek information.")
  }
  GW <- as.data.table(data$events)
  list(
    current = GW[is_current == TRUE, id],
    `next` = GW[is_next == TRUE, id],
    finished = GW[finished == TRUE, max(id, na.rm = TRUE)]
  )
}
