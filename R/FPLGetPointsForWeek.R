FPLGetPointsForWeek <- function(GW){
  x <- GET(paste0("https://fantasy.premierleague.com/api/event/",GW,"/live/"))

  y <- content(x)
  ListOfData <- lapply(X = y$elements, function(x){data.table(id = x$id,Points = x$stats$total_points)})

  Picks <- data.table::rbindlist(ListOfData)

  y <- FPLGetPlayerInfo()[, list(id, web_name)]

  Picks[y, on = "id", Name := i.web_name]

  Output <- Picks[order(-Points)]

  return(Output)
}

