library(THFPL)
library(httr)
library(ggplot2)

FPLGetPlayerInfo()

LeagueInfo <- FPLGetLeagueInfo(721349)


ThisWeekCaptains <- FPLGetMostCaptainedPlayers(LeagueCode = 721349,GW = 2)


LastWeekCaptains <- FPLGetMostCaptainedPlayers(997983,10)

SelectedPlayers <- FPLGetMostSelectedPlayers(721349,1)
SelectedPlayers

OldSelectedPlayers <- FPLGetMostSelectedPlayers(997983,10)#

LeagueCode <- 997983
GW <- 7
LeagueInfo <- FPLGetLeagueInfo(LeagueCode)


FPLGetTransfers(LeagueCode,24)


LastGW <- 33

ChartData <- rbindlist(l = lapply(X = 1:LastGW,FUN = function(x){FPLGetTeamValue(LeagueCode = LeagueCode,GW = x)}),idcol = "GW")

ggplot(data = ChartData,mapping = aes(x = GW, y = TeamValue/10,group = rn, color = rn))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  scale_color_manual(values = c("#5B9BD5", "#ED7D31", "#A5A5A5", "#FFC000", "#ff33ba", "#70AD47", "#255E91", "#9E480E"))+
  labs(x = "Gameweek",y = "Team Value (£)", title = "Team Value Over Time",color = NULL)+
  theme(
    legend.position = c(0.025, 0.975),  # Position the legend inside the plot
    legend.justification = c(0, 1),   # Align it to the top-left corner
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Add background and border
    legend.title = element_text(face = "bold"),  # Make legend title bold
    legend.text = element_text(size = 10),  # Adjust the size of legend text
    legend.margin = margin(6, 6, 6, 6)  # Add some padding around the legend
  )+
  scale_x_continuous(breaks = 1:LastGW)


ChartData <- rbindlist(l = lapply(X = 1:LastGW,FUN = function(x){FPLGetTeamPoints(LeagueCode = LeagueCode,GW = x)}),idcol = "GW")

ChartData[order(GW), AggPoints := cumsum(Points), by = rn]

ZeroPoint <- ChartData[GW == 1, list(GW = 0, rn, Points = 0,AggPoints = 0)]

ChartData <- rbind(ZeroPoint,ChartData)
ChartData[GW==23]
ggplot(data = ChartData,mapping = aes(x = GW, y = AggPoints,group = rn, color = rn))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  scale_color_manual(values = c("#5B9BD5", "#ED7D31", "#A5A5A5", "#FFC000", "#ff33ba", "#70AD47", "#255E91", "#9E480E"))+
  labs(x = "Gameweek",y = "Points", title = "Points over  Time",color = NULL)+
  theme(
    legend.position = c(0.025, 0.975),  # Position the legend inside the plot
    legend.justification = c(0, 1),   # Align it to the top-left corner
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Add background and border
    legend.title = element_text(face = "bold"),  # Make legend title bold
    legend.text = element_text(size = 10),  # Adjust the size of legend text
    legend.margin = margin(6, 6, 6, 6)  # Add some padding around the legend
  )+
  scale_x_continuous(breaks = 1:LastGW)

ChartData[, AveragePoints := mean(Points), by = GW]

ChartData[,AdjustedPoints := Points - AveragePoints]

ChartData[order(GW), AggAdjPoints := cumsum(AdjustedPoints), by = rn]

ChartData <- ChartData[, list(GW, rn, AggAdjPoints)]

ZeroPoint <- ChartData[GW == 1, list(GW = 0, rn, AggAdjPoints = 0)]

ChartData <- rbind(ZeroPoint,ChartData)

ggplot(data = ChartData,mapping = aes(x = GW, y = AggAdjPoints,group = rn, color = rn))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  scale_color_manual(values = c("#5B9BD5", "#ED7D31", "#A5A5A5", "#FFC000", "#ff33ba", "#70AD47", "#255E91", "#9E480E"))+
  labs(x = "Gameweek",y = "Points", title = "Points over  Time (relative to group average performance)",color = NULL)+
  theme(
    legend.position = c(0.025, 0.975),  # Position the legend inside the plot
    legend.justification = c(0, 1),   # Align it to the top-left corner
    legend.background = element_rect(fill = "white", color = "black", size = 0.5),  # Add background and border
    legend.title = element_text(face = "bold"),  # Make legend title bold
    legend.text = element_text(size = 10),  # Adjust the size of legend text
    legend.margin = margin(6, 6, 6, 6)  # Add some padding around the legend
  )+
  scale_x_continuous(breaks = 1:LastGW)
