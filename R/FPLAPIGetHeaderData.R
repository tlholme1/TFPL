#' Retrieve bootstrap header information
#'
#' @return Parsed bootstrap header data.
#' @details Throws an error if no data is returned.
FPLAPIGetHeaderData <- function(...) {
  res <- .FPLAPIGet("/bootstrap-static/", ...)
  if (length(res) == 0) {
    stop("No header data returned.")
  }
  res
}

