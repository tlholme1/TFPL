#' Retrieve transfers for a player
#'
#' @param PlayerId Player ID.
#'
#' @return Parsed transfer data.
FPLAPIGetPlayerTransfers <- function(PlayerId, ...) {
  .FPLAPIGet(paste0("/entry/", PlayerId, "/transfers"), ...)
}

