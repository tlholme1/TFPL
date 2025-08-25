library(THFPL)

LeagueInfo <- FPLGetLeagueInfo(721349)

CurrentGameWeek <- FPLGetCurrentGW()$current

FPLCompareCaptains(LeagueCode = 721349, GW = CurrentGameWeek)

SelectedPlayers <- FPLGetMostSelectedPlayers(721349, CurrentGameWeek - 1)

FPLGetUserTeam(PlayerId = 7572777, GW = 2)

FPLGetTransfers(721349, 2)

PlotTeamValueOverTime(LeagueCode = 721349,
                      LastGW = 2)

PlotTeamPointsOverTime(LeagueCode = 721349,
                      LastGW = 2)

PlotTeamPointsOverTime(LeagueCode = 721349,
                       LastGW = 2,
                       Average = TRUE)

SquadsInclBench <- FPLGetSelectedPlayersForALeague(721349, 1)
PlayerPoints <- FPLGetGameweekPoints(1)

SquadsInclBench[PlayerPoints, on = "Name", Points := i.Points]

SquadsInclBench[Status == "Bench", sum(Points), by = player_name]
