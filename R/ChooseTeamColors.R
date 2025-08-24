#' Choose Team Colors
#'
#' Selects an appropriate color palette for a team based on the team name or preference.
#'
#' @param team The name of the team.
#' @param palette (Optional) A predefined palette name. Defaults to 'default'.
#'
#' @return A vector of colors representing the team colors.
ChooseTeamColors <- function(team, palette = 'default') {
  if(palette == 'default') {
    # Default palette
    colors <- c('red', 'blue', 'green')
  } else {
    colors <- palette
  }
  return(colors)
}