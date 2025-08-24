#' FPL Plot Team Value Over Time
#'
#' Integrates data fetching, validation, plotting, and color selection into a single function to produce a refined plot of team values over time.
#'
#' @param LeagueCode A string representing the league code.
#' @param LastGW An integer representing the last game week to fetch and visualize.
#'
#' @return A ggplot object visualizing team value trends with team-specific colors.
FPLPlotTeamValueOverTime <- function(LeagueCode, LastGW) {
  # Fetch team value data
  data <- FPLFetchTeamValues(LeagueCode, LastGW)
  
  # Validate data
  valid <- FPLValidateTeamValueData(data)
  if (!valid) stop('Invalid team value data')
  
  # Build plot
  plot <- FPLBuildTeamValuePlot(data)
  
  # Choose colors (assuming FPLGetTeamName function exists and returns a team name)
  team <- if(exists('FPLgetTeamName')) FPLgetTeamName(LeagueCode) else 'defaultTeam'
  colors <- FPLChooseTeamColors(team)
  
  # Adjust plot colors if needed
  plot <- plot + scale_color_manual(values = colors)
  
  return(plot)
}