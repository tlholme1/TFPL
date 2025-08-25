#' Plot Team Metric Over Time
#'
#' Integrates data fetching, validation, plotting, and color selection into a single function to produce a refined plot of team metrics over time.
#'
#' @param LeagueCode A string representing the league code. Must be a single integer.
#' @param LastGW An integer representing the last game week to fetch and visualize. Must be a single integer.
#' @param metric The metric to visualise, either "points" or "value".
#' @param Average Logical. If TRUE and metric is "points", adjusts for average points per GW.
#'
#' @return A ggplot object visualizing team metric trends with team-specific colors.
#' @keywords internal
.PlotTeamMetricOverTime <- function(LeagueCode, LastGW, metric = c("points", "value"), Average = FALSE) {
  metric <- match.arg(metric)
  .assert_single_integer(LeagueCode, "LeagueCode")
  .assert_single_integer(LastGW, "LastGW")
  data <- .FetchTeamMetric(LeagueCode, LastGW, metric = metric)
  valid <- .ValidateTeamMetricData(data, metric = metric)
  if (!valid) stop('Invalid team ', metric, ' data')
  plot <- .BuildTeamMetricPlot(data, metric = metric, Average = Average)
  team <- if (exists('getTeamName')) getTeamName(LeagueCode) else 'defaultTeam'
  colors <- ChooseTeamColors(team)
  plot <- plot + scale_color_manual(values = colors)
  return(plot)
}
