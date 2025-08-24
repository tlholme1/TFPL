#' Build Team Points Plot
#'
#' Constructs a ggplot object visualizing team Pointss over the gameweeks.
#'
#' @param data A data.table with team Points information.
#'
#' @return A ggplot object representing the team Points trend.
BuildTeamPointsPlot <- function(data, Average = FALSE) {

  zero_points <- unique(data[, list(rn)])
  zero_points[, `:=`(GW = 0,Points = 0, CumPoints = 0)]

  plot_data <- rbindlist(list(zero_points, data),use.names=TRUE)

  if (Average) {
    plot_data[, AvgPoints := mean(Points), by = GW]

    plot_data[, AdjPoints := Points - AvgPoints]
    plot_data[order(GW), CumPoints := cumsum(AdjPoints), by = rn]
  }

  p <- ggplot(plot_data, aes(x = GW, y = CumPoints, group = rn,color = rn)) +
    geom_line() +
    geom_point() +
    labs(title = 'Team Points Over Time', x = 'Game Week', y = 'Team Points') +
    theme_minimal()+

    scale_x_continuous(breaks = unique(plot_data$GW))

  if(!Average){
    p <- p +  scale_y_continuous(limits = c(0,NA))
  }

  return(p)
}

