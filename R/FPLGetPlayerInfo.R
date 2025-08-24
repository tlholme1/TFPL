FPLGetPlayerInfo <- function(){
  StaticInfoResponse <- GET(url = "https://fantasy.premierleague.com/api/bootstrap-static/")

  StaticInfoList <- content(StaticInfoResponse)

  StaticInfoTable <- data.table::rbindlist(StaticInfoList$elements)

  return(StaticInfoTable)
}
