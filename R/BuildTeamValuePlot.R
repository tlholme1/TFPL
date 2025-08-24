#' Build Team Value Plot
#'
#' Constructs a ggplot object visualizing team values over the gameweeks.
#'
#' @param data A data.table with team value information.
#'
#' @return A ggplot object representing the team value trend.
BuildTeamValuePlot <- function(data) {
  library(ggplot2)
  p <- ggplot(data, aes(x = GW, y = team_value)) +
    geom_line(color = 'blue') +
    labs(title = 'Team Value Over Time', x = 'Game Week', y = 'Team Value') +
    theme_minimal()
  return(p)
}