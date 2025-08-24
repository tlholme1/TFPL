#' Build Team Points Plot
#'
#' Constructs a ggplot object visualizing team Pointss over the gameweeks.
#'
#' @param data A data.table with team Points information.
#'
#' @return A ggplot object representing the team Points trend.
BuildTeamPointsPlot <- function(data) {

  p <- ggplot(data, aes(x = GW, y = CumPoints, group = rn,color = rn)) +
    geom_line() +
    labs(title = 'Team Points Over Time', x = 'Game Week', y = 'Team Points') +
    theme_minimal()+
    scale_y_continuous(limits = c(0,NA))

  return(p)
}

