#' Perform an FPL API GET request
#'
#' Wrapper around httr::GET with a base URL for the FPL API and
#' automatic status checking.
#'
#' @param endpoint Path to append to the base API URL. Must begin with '/'.
#' @param ... Additional arguments passed to httr::GET.
#'
#' @return Parsed response content.
#' @noRd
.FPLAPIGet <- function(endpoint, ...) {
  base <- "https://fantasy.premierleague.com/api"
  url <- paste0(base, endpoint)
  resp <- GET(url = url, ...)
  stop_for_status(resp)
  content(resp)
}

