library(THFPL)
library(httr)
library(ggplot2)
library(data.table)

LeagueInfo <- FPLGetLeagueInfo(721349)

CurrentGameWeek <- FPLGetCurrentGW()$current

FPLCompareCaptains(LeagueCode = 721349, GW = CurrentGameWeek)

SelectedPlayers <- FPLGetMostSelectedPlayers(721349,CurrentGameWeek-1)

OldSelectedPlayers <- FPLGetMostSelectedPlayers(721349,CurrentGameWeek)#

FPLGetUserTeam(PlayerId = 7572777, GW = 2)

FPLGetSelectedPlayersForALeague(721349,CurrentGameWeek)
LeagueCode <- 721349
GW <- 7
LeagueInfo <- FPLGetLeagueInfo(LeagueCode)


FPLGetTransfers(721349,2)

PlotTeamValueOverTime(LeagueCode = 721349,
                      LastGW = 2)

PlotTeamPointsOverTime(LeagueCode = 721349,
                      LastGW = 2)

PlotTeamPointsOverTime(LeagueCode = 721349,
                       LastGW = 2,
                       Average = TRUE)



SquadsInclBench <- FPLGetSelectedPlayersForALeague(721349,1)
setnames(SquadsInclBench, "player_name", "PlayerName")
PlayerPoints <- FPLGetGameweekPoints(1)

SquadsInclBench[PlayerPoints, on = "Name", Points := i.Points]

SquadsInclBench[Status == "Bench", sum(Points), by = PlayerName]


# Examples for modal XI utilities
FPLGetModalXIWorld()
FPLGetLeagueModalXI(721349, 1)
FPLCompareTeams(1:11, 2:12)
FPLFindMostSimilarTeams(721349, 1)
FPLFindClosestToWorldModal(721349, 1)
FPLFindClosestToLeagueModal(721349, 1)
