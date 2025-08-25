#' Compute similarity between two teams
#'
#' Uses the Jaccard similarity coefficient on player ids.
#'
#' @param Team1 Vector of player ids for the first team.
#' @param Team2 Vector of player ids for the second team.
#'
#' @return Numeric similarity between 0 and 1.
FPLCompareTeams <- function(Team1, Team2){
  Intersection <- length(intersect(Team1, Team2))
  Union <- length(union(Team1, Team2))
  Intersection / Union
}
