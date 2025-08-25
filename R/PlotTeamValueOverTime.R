#' Plot Team Value Over Time
#'
#' Wrapper around `.PlotTeamMetricOverTime` to visualise team value.
#'
#' @param LeagueCode A string representing the league code. Must be a single integer.
#' @param LastGW An integer representing the last game week to fetch and visualize. Must be a single integer.
#'
#' @return A ggplot object visualizing team value trends with team-specific colors.
PlotTeamValueOverTime <- function(LeagueCode, LastGW) {
  .PlotTeamMetricOverTime(LeagueCode, LastGW, metric = "value")
}
