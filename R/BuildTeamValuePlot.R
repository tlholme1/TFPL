#' Build Team Value Plot
#'
#' Constructs a ggplot object visualizing team values over the gameweeks.
#'
#' @param data A data.table with team value information.
#'
#' @return A ggplot object representing the team value trend.
BuildTeamValuePlot <- function(data) {

  p <- ggplot(data, aes(x = GW, y = TeamValue/10, group = rn,color = rn)) +
    geom_line() +
    labs(title = 'Team Value Over Time', x = 'Game Week', y = 'Team Value') +
    theme_minimal()

  return(p)
}

