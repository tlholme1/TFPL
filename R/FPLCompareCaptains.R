#' Compare Captains Across Gameweeks
#'
#' Downloads captain choices for the current and previous gameweek, and compares them.
#'
#' @param LeagueCode Integer, the FPL league code. Must be a single integer.
#' @param GW Integer, the current gameweek (must be > 1). Must be a single integer.
#'
#' @return A data.table showing each team's captain choice this week vs last week.
#' @examples
#' \dontrun{
#'   CompareCaptains(721349, GW = 3)
#' }
FPLCompareCaptains <- function(LeagueCode, GW) {
  .assert_single_integer(LeagueCode, "LeagueCode")
  .assert_single_integer(GW, "GW")
  if (GW <= 1) {
    stop("Gameweek must be greater than 1 to compare.")
  }

  # captains this GW
  this_week <- FPLGetCaptains(LeagueCode, GW)
  this_week[, GW := GW]

  # captains last GW
  last_week <- FPLGetCaptains(LeagueCode, GW - 1)
  last_week[, GW := GW - 1]

  # join on team identifier
  comparison <- merge(
    last_week[, .(player_name, LastCaptain = Name)],
    this_week[, .(player_name, ThisCaptain = Name)],
    by = "player_name", all = TRUE
  )

  # flag changes
  comparison[, Changed := ThisCaptain != LastCaptain]

  return(comparison[])
}
