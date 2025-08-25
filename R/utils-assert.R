#' Validate a length-one integer parameter
#'
#' Ensures that a numeric parameter is a length-one integer.
#'
#' @param x The object to validate.
#' @param name Parameter name for informative error messages.
#'
#' @return TRUE invisibly if validation passes; otherwise an error is thrown.
#' @noRd
.assert_single_integer <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || x %% 1 != 0) {
    stop(paste0(name, " must be a length-one integer."), call. = FALSE)
  }
  invisible(TRUE)
}
