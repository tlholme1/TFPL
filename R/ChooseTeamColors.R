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
    colors <- c("#5B9BD5", "#ED7D31", "#A5A5A5", "#FFC000", "#ff33ba", "#70AD47", "#255E91", "#9E480E")
  } else {
    colors <- palette
  }
  return(colors)
}
