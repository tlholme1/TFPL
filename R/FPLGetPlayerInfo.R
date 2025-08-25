#' Retrieve static player information
#'
#' @return A data.table with player metadata.
FPLGetPlayerInfo <- function(){
  StaticInfo <- FPLAPIGetHeaderData()
  StaticInfoTable <- rbindlist(StaticInfo$elements)
  return(StaticInfoTable)
}
