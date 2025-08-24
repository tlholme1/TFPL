FPLGetLeagueInfo <- function(LeagueCode = 997983){

  LeagueInfoResponse <- GET(url = paste0("https://fantasy.premierleague.com/api/leagues-classic/",LeagueCode,"/standings/"))

  LeaguesTable <- data.table::rbindlist(content(LeagueInfoResponse)$standings$results)

  return(LeaguesTable)
}
