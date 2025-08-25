#' Retrieve global modal XI
#'
#' Returns the eleven most selected players worldwide based on the current
#' selection percentages available from the FPL bootstrap data.
#'
#' @return A data.table with player identifiers and metadata.
FPLGetModalXIWorld <- function(){
  Info <- FPLGetPlayerInfo()[,
    .(PlayerId = id,
      WebName = web_name,
      Team = team,
      ElementType = element_type,
      SelectedByPercent = as.numeric(selected_by_percent))]
  setorder(Info, -SelectedByPercent)
  ModalXI <- Info[1:11]
  return(ModalXI)
}
