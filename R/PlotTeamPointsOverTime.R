#' Plot Team Points Over Time
#'
#' Wrapper around `.PlotTeamMetricOverTime` to visualise cumulative team points.
#'
#' @param LeagueCode A string representing the league code. Must be a single integer.
#' @param LastGW An integer representing the last game week to fetch and visualize. Must be a single integer.
#' @param Average Logical. If TRUE adjusts for average points per GW.
#'
#' @return A ggplot object visualizing team points trends with team-specific colors.
PlotTeamPointsOverTime <- function(LeagueCode, LastGW, Average = FALSE) {
  .PlotTeamMetricOverTime(LeagueCode, LastGW, metric = "points", Average = Average)
}
