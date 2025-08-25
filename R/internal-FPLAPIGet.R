#' Perform an FPL API GET request
#'
#' Wrapper around httr::GET with a base URL for the FPL API and
#' automatic status checking.
#'
#' @param endpoint Path to append to the base API URL. Must begin with '/'.
#' @param ... Additional arguments passed to httr::GET.
#'
#' @return Parsed response content.
#' @details Throws an informative error if the request fails.
#' @noRd
.FPLAPIGet <- function(endpoint, ...) {
  base <- "https://fantasy.premierleague.com/api"
  url <- paste0(base, endpoint)
  tryCatch({
    resp <- GET(url = url, ...)
    stop_for_status(resp)
    content(resp)
  }, error = function(e) {
    stop("API request failed: ", e$message)
  })
}

