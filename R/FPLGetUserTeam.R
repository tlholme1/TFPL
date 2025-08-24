FPLGetUserTeam <- function(UserID, GW){
  x <- GET(url = paste0("https://fantasy.premierleague.com/api/entry/",UserID,"/event/",GW,"/picks"))

  y <- content(x)

  Picks <- data.table::rbindlist(y$picks)

  PlayerInfo <- FPLGetPlayerInfo()

  Picks[PlayerInfo, on = list(element = id),Name := i.web_name]

  Picks[, Status := fifelse(position %in% 1:11, "Selected","Bench")]

  return(Picks)
}
