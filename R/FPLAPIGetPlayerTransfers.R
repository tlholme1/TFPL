#' Retrieve transfers for a player
#'
#' @param PlayerId Player ID. Must be a single integer.
#'
#' @return Parsed transfer data.
#' @details Throws an error if no data is returned.
FPLAPIGetPlayerTransfers <- function(PlayerId, ...) {
  .assert_single_integer(PlayerId, "PlayerId")
  res <- .FPLAPIGet(paste0("/entry/", PlayerId, "/transfers"), ...)
  if (length(res) == 0) {
    stop("No transfer data returned for player ", PlayerId)
  }
  res
}

