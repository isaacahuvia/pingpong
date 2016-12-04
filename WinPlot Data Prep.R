########### WinPlot Data Prep
library(tidyr); library(dplyr)

load(file = "Manipulated Data.RData")

dfWinPlot <- dfMatch

#Add row of leading zeros so the plot starts at 0,0 - but have to do Date separately because you'll
#get an error for simply throwing a zero at the beginning of a date vector
dfWinPlot <- cbind(
  rbind(rep(0, length(dplyr::select(dfWinPlot, -Date))), dplyr::select(dfWinPlot, -Date)),
  c(NA, dfWinPlot$Date)
)
names(dfWinPlot)[length(dfWinPlot)] <- "Date"

#This next chunk creates new objects to build lines in the graph. 
#To be included in the plot, each object needs to include the coordinates of line endpoints, arranged in a matrix
 
#Current lead line
Lead_X <- rep(max(dfWinPlot$Match),2)
Lead_Y <- c(dfWinPlot$IsaacCumulativeWins[dfWinPlot$Match == max(dfWinPlot$Match)], dfWinPlot$TimiCumulativeWins[dfWinPlot$Match == max(dfWinPlot$Match)])
CurrentLead_XY <- as.data.frame(cbind(Lead_X, Lead_Y))
CurrentLead_XY$Type <- "Current"
 
#Isaac largest lead line
Lead_X <- rep(dfWinPlot$Match[dfWinPlot$IsaacLead == max(dfWinPlot$IsaacLead)], 2)
Lead_Y <- c(dfWinPlot$IsaacCumulativeWins[dfWinPlot$IsaacLead == max(dfWinPlot$IsaacLead)], dfWinPlot$TimiCumulativeWins[dfWinPlot$IsaacLead == max(dfWinPlot$IsaacLead)])
IsaacLead_XY <- as.data.frame(cbind(Lead_X, Lead_Y))
IsaacLead_XY <- dplyr::arrange(IsaacLead_XY, Lead_X)
IsaacLead_XY <- tail(IsaacLead_XY, 2)
IsaacLead_XY$Type <- "Isaac"

#Timi largest lead line
Lead_X <- rep(dfWinPlot$Match[dfWinPlot$TimiLead == max(dfWinPlot$TimiLead)], 2)
Lead_Y <- c(dfWinPlot$IsaacCumulativeWins[dfWinPlot$TimiLead == max(dfWinPlot$TimiLead)], dfWinPlot$TimiCumulativeWins[dfWinPlot$TimiLead == max(dfWinPlot$TimiLead)])
TimiLead_XY <- as.data.frame(cbind(Lead_X, Lead_Y))
TimiLead_XY <- dplyr::arrange(TimiLead_XY, Lead_X)
TimiLead_XY <- tail(TimiLead_XY, 2)
TimiLead_XY$Type <- "Timi"
 
#Put all lines into one dfMatch, so they can be easily included in the plot
AllLeads_XY <- rbind(CurrentLead_XY, IsaacLead_XY, TimiLead_XY)

#Build a separate matrix that will label the lead lines
dfWinPlot$LeadFlag <- 0
dfWinPlot$LeadFlag[dfWinPlot$Match == max(dfWinPlot$Match)] <- 1
dfWinPlot$LeadFlag[dfWinPlot$IsaacLead == min(dfWinPlot$IsaacLead) & dfWinPlot$Match == max(dfWinPlot$Match[dfWinPlot$IsaacLead == min(dfWinPlot$IsaacLead)])] <- 1
dfWinPlot$LeadFlag[dfWinPlot$IsaacLead == max(dfWinPlot$IsaacLead) & dfWinPlot$Match == max(dfWinPlot$Match[dfWinPlot$IsaacLead == max(dfWinPlot$IsaacLead)])] <- 1
dfTextLead <- filter(dfWinPlot, LeadFlag == 1)
for(i in 1:length(dfTextLead$Match)) {
 dfTextLead$LowValue[i] <- min(dfTextLead$IsaacCumulativeWins[i], dfTextLead$TimiCumulativeWins[i])
}
dfTextLead$IsaacLead <- abs(dfTextLead$IsaacLead)
 

#Isaac longest streak line
Streak_X <- c(max(dfWinPlot$Match[dfWinPlot$IsaacStreak == max(dfWinPlot$IsaacStreak)]) - max(dfWinPlot$IsaacStreak), max(dfWinPlot$Match[dfWinPlot$IsaacStreak == max(dfWinPlot$IsaacStreak)]))
Streak_Y <- rep(max(dfWinPlot$IsaacCumulativeWins[dfWinPlot$Match == Streak_X[2]], dfWinPlot$TimiCumulativeWins[dfWinPlot$Match == Streak_X[2]]) + 5, 2)
IsaacStreak_XY <- as.data.frame(cbind(Streak_X, Streak_Y))
IsaacStreak_XY <- dplyr::arrange(IsaacStreak_XY, Streak_X)
IsaacStreak_XY$Type <- "Isaac"

#Timi longest streak line
Streak_X <- c(max(dfWinPlot$Match[dfWinPlot$TimiStreak == max(dfWinPlot$TimiStreak)]) - max(dfWinPlot$TimiStreak), max(dfWinPlot$Match[dfWinPlot$TimiStreak == max(dfWinPlot$TimiStreak)]))
Streak_Y <- rep(max(dfWinPlot$IsaacCumulativeWins[dfWinPlot$Match == Streak_X[2]], dfWinPlot$TimiCumulativeWins[dfWinPlot$Match == Streak_X[2]]) + 5, 2)
TimiStreak_XY <- as.data.frame(cbind(Streak_X, Streak_Y))
TimiStreak_XY <- dplyr::arrange(TimiStreak_XY, Streak_X)
TimiStreak_XY$Type <- "Timi"

#Put all streak into one dfMatch, so they can be easily included in the plot
AllStreaks_XY <- rbind(IsaacStreak_XY, TimiStreak_XY)

#for labels
dfTextStreak <- cbind(
  rbind(
    AllStreaks_XY[2,],
    AllStreaks_XY[4,]
  ),
  c(max(dfWinPlot$IsaacStreak), max(dfWinPlot$TimiStreak))
)
names(dfTextStreak)[length(dfTextStreak)] <- "Text"