#' Retrieve bootstrap header information
#'
#' @return Parsed bootstrap header data.
FPLAPIGetHeaderData <- function(...) {
  .FPLAPIGet("/bootstrap-static/", ...)
}

