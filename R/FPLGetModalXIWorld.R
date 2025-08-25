#' Retrieve global modal XI
#'
#' Returns the eleven most selected players worldwide based on the current
#' selection percentages available from the FPL bootstrap data. The output
#' honours the basic squad rules by ensuring at least one goalkeeper, three
#' defenders, one midfielder and one forward are included. Remaining spots are
#' filled by the next most selected players regardless of position.
#'
#' @param GW Gameweek number (currently unused but included for interface
#'   consistency).
#'
#' @return A data.table with player identifiers and metadata.
FPLGetModalXIWorld <- function(GW = FPLGetCurrentGW()$current){
  Info <- FPLGetPlayerInfo()[,
    .(PlayerId = id,
      WebName = web_name,
      Team = team,
      ElementType = element_type,
      SelectedByPercent = as.numeric(selected_by_percent))]
  setorder(Info, -SelectedByPercent)

  GK  <- Info[ElementType == 1][1]
  DEF <- Info[ElementType == 2][1:3]
  MID <- Info[ElementType == 3][1]
  FWD <- Info[ElementType == 4][1]
  Picked <- rbindlist(list(GK, DEF, MID, FWD))

  Remaining <- Info[!PlayerId %in% Picked$PlayerId]
  SpotsLeft <- 11 - nrow(Picked)
  if (SpotsLeft > 0) {
    Picked <- rbind(Picked, Remaining[1:SpotsLeft])
  }

  return(Picked)
}
