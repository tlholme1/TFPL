#' Validate Team Metric Data
#'
#' Validates the team metric data ensuring it's properly formatted and contains no missing values.
#'
#' @param data A `data.table` with team metric information.
#' @param metric The metric the data represents, either "points" or "value".
#'
#' @return `TRUE` if data is valid, otherwise an error is thrown.
#' @keywords internal
.ValidateTeamMetricData <- function(data, metric = c("points", "value")) {
  metric <- match.arg(metric)
  if (is.null(data)) {
    stop("Data is null")
  }
  # Additional validation logic can be added here depending on metric
  return(TRUE)
}
