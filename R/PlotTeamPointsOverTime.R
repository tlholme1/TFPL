#' Integrates data fetching, validation, plotting, and color selection into a single function to produce a refined plot of team points over time.
#'
#' @param LeagueCode A string representing the league code.
#' @param LastGW An integer representing the last game week to fetch and visualize.
#'
#' @return A ggplot object visualizing team points trends with team-specific colors.
PlotTeamPointsOverTime <- function(LeagueCode, LastGW, Average = FALSE) {
  # Fetch team value data
  data <- FetchTeamPoints(LeagueCode, LastGW)

  # Validate data
  valid <- ValidateTeamPointsData(data)
  if (!valid) stop('Invalid team points data')

  # Build plot
  plot <- BuildTeamPointsPlot(data, Average = Average)

  # Choose colors (assuming getTeamName function exists and returns a team name)
  team <- if(exists('getTeamName')) getTeamName(LeagueCode) else 'defaultTeam'
  colors <- ChooseTeamColors(team)

  # Adjust plot colors if needed
  plot <- plot + scale_color_manual(values = colors)

  return(plot)
}
