FPLGetTeamPoints <- function(LeagueCode,GW){
  LeagueInfo <- FPLGetLeagueInfo(LeagueCode=LeagueCode)

  EntriesToLoop <- LeagueInfo$entry
  names(EntriesToLoop) <- LeagueInfo$player_name

  ListOfTeams <- lapply(X = EntriesToLoop,FUN = function(x){
    x <- GET(url = paste0("https://fantasy.premierleague.com/api/entry/",x,"/event/",GW,"/picks"))

    y <- content(x)
    y$entry_history$points
  })

  TeamTable <- t(data.frame(ListOfTeams))
  Output <- data.table(TeamTable,keep.rownames = TRUE)[order(-V1)]

  setnames(Output, "V1","Points")

  return(Output)
}
