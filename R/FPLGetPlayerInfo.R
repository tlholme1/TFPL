#' Retrieve static player information
#'
#' @return A data.table with player metadata.
#' @details Throws an error if the data cannot be retrieved.
FPLGetPlayerInfo <- function(){
  StaticInfo <- FPLAPIGetHeaderData()
  if (length(StaticInfo) == 0 || is.null(StaticInfo$elements)) {
    stop("Failed to retrieve player information.")
  }
  StaticInfoTable <- rbindlist(StaticInfo$elements)
  return(StaticInfoTable)
}
