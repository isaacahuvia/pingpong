# Really-Important-Stuff
pong analytics 

library(dplyr); library(ggplot2); library(tidyr)

#                           O .
#                        _/|\_-O
#                      ___|_______
#                     /     |     \
#                    /      |      \
#                   __________________
#                  /    _ ( )|        \
#                 /    ( ) | |         \
#                /   \  |_/  |          \
#               /_____\/|____|___________\
#                  |   |             |
#                  |  / \            |
#                  | /   \           |
#                  _/    /_


Isaac <- c(0,0,0,1,0,1,0,1,1,0,1,0,0,1,1,1,1,1,1,1,0,1,0,1,0,1,0,1,0,0,1,0,1,1,0,1,1,0,1,1,0,0,1,0,0,1,0,0,1,1,
           0,1,0,1,1,0,0,1,0,1,0,1,1,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,1,0,1,0,1,0,1,0,1,0,0,0,0,1,1,1,
           #END OF THE FIRST EPOCH - Best of 100 won by Isaac on tiebreak 51-50 *eye roll*
           0,1,0,0,0,0,1,0,0
)

#### Automatic ####
Timi <- 1 - Isaac
df <- as.data.frame(cbind(Isaac, Timi))
df$IsaacCumulative = df$Isaac
df$TimiCumulative = df$Timi
df$Match = 1
# print(Timi)
for(i in 1:length(df$Isaac)) {
  if(i == 1) {
    df$IsaacCumulative[i] = df$Isaac[i]
    df$TimiCumulative[i] = df$Timi[i]
  } else {
    df$IsaacCumulative[i] = df$IsaacCumulative[i-1] + df$Isaac[i]
    df$TimiCumulative[i] = df$TimiCumulative[i-1] + df$Timi[i]
    df$Match[i] = i
  }
  # print(paste0(i, ": ", df$TimiCumulative[i]))
}
df <- rbind(c(0,0,0,0,0), df)
df$Lead <- df$IsaacCumulative - df$TimiCumulative

LineX <- rep(max(df$Match),2)
LineY <- c(df$IsaacCumulative[df$Match == max(df$Match)], df$TimiCumulative[df$Match == max(df$Match)])
dfLine <- as.data.frame(cbind(LineX, LineY))
dfLine$Type <- "Current"

LineX <- rep(df$Match[df$Lead == max(df$Lead)], 2)
LineY <- c(df$IsaacCumulative[df$Lead == max(df$Lead)], df$TimiCumulative[df$Lead == max(df$Lead)])
dfLine_I <- as.data.frame(cbind(LineX, LineY))
dfLine_I <- dplyr::arrange(dfLine_I, LineX)
dfLine_I <- tail(dfLine_I, 2)
dfLine_I$Type <- "Isaac"

LineX <- rep(df$Match[df$Lead == min(df$Lead)], 2)
LineY <- c(df$IsaacCumulative[df$Lead == min(df$Lead)], df$TimiCumulative[df$Lead == min(df$Lead)])
dfLine_T <- as.data.frame(cbind(LineX, LineY))
dfLine_T <- dplyr::arrange(dfLine_T, LineX)
dfLine_T <- tail(dfLine_T, 2)
dfLine_T$Type <- "Timi"

dfLines <- rbind(dfLine, dfLine_I, dfLine_T)

dfalt <- filter(df, Match == max(Match))

limmax = length(Isaac)

df$LeadFlag <- 0
df$LeadFlag[df$Match == max(df$Match)] <- 1
df$LeadFlag[df$Lead == min(df$Lead) & df$Match == max(df$Match[df$Lead == min(df$Lead)])] <- 1
df$LeadFlag[df$Lead == max(df$Lead) & df$Match == max(df$Match[df$Lead == max(df$Lead)])] <- 1
dfTextLead <- filter(df, LeadFlag == 1)
for(i in 1:length(dfTextLead$Match)) {
  dfTextLead$LowValue[i] <- min(dfTextLead$IsaacCumulative[i], dfTextLead$TimiCumulative[i])
}
dfTextLead$Lead <- abs(dfTextLead$Lead)

##Add largest streak line and label
df$CurrentStreak <- 1
for(i in 2:length(df$Match)) {
  if(df$Isaac[i] == 1 & df$Isaac[i-1] == 1) {
    df$CurrentStreak[i] <- df$CurrentStreak[i-1] + 1
  } else if(df$Timi[i] == 1 & df$Timi[i-1] == 1) {
    df$CurrentStreak[i] <- df$CurrentStreak[i-1] + 1
  }
}
dfTextStreak <- df[df$CurrentStreak == max(df$CurrentStreak),]
Streak_X <- c(dfTextStreak$Match, dfTextStreak$Match - dfTextStreak$CurrentStreak)
Streak_Y <- rep(max(dfTextStreak$IsaacCumulative, dfTextStreak$TimiCumulative) + 5, 2)
dfLineStreak <- as.data.frame(cbind(Streak_X, Streak_Y))
dfTextStreak <- filter(dfLineStreak, Streak_X == min(Streak_X))
dfTextStreak$Text <- paste0("Longest Streak = ", max(dfLineStreak$Streak_X) - min(dfLineStreak$Streak_X))

WinPlot <- ggplot() +
  geom_line(data = df, aes(Match, TimiCumulative, color = "Blue")) +
  geom_line(data = df, aes(Match, IsaacCumulative, color = "Red")) +
  geom_line(data = dfLines, aes(LineX, LineY, linetype = Type)) +
  geom_line(data = dfLineStreak, aes(Streak_X, Streak_Y)) +
  geom_text(data = dfTextLead, aes(Match, LowValue, label = Lead), hjust = 0, vjust = 1) +
  geom_text(data = dfTextStreak, aes(Streak_X, Streak_Y, label = Text), hjust = 0, vjust = -1) +
  scale_colour_discrete(name = paste0("Player (Wins; Pct of ", length(Isaac), ")"), labels = c(paste0("Timi (", sum(Isaac == 0), "; ", round(sum(Isaac == 0) * 100 / length(Isaac), 0), "%)"), paste0("Isaac (", sum(Isaac == 1), "; ", round(sum(Isaac == 1) * 100 / length(Isaac), 0), "%)"))) +
  scale_linetype_discrete(name = "Largest Lead") +
  ylab("Matches Won") +
  xlim(0, limmax) + ylim(0, limmax)
WinPlot

# df$IsaacNormalized <- df$IsaacCumulative - (df$Match / 2)
# df$TimiNormalized <- df$TimiCumulative - (df$Match / 2)
# 
# WinPlotNormalized <- ggplot() +
#   geom_line(data = df, aes(Match, TimiNormalized, color = "Blue")) +
#   geom_line(data = df, aes(Match, IsaacNormalized, color = "Red")) +
#   scale_colour_discrete(name = "Player", labels = c("Timi", "Isaac")) +
#   ylab("Lead (Matches)")
# WinPlotNormalized


IsaacLag <- NULL
for(i in 1:length(Isaac)) {
  IsaacLag[i] <- Isaac[i-1]
}
SameAsLast[IsaacLag[2:length(IsaacLag)] != Isaac[2:length(Isaac)]] <- 0
SameAsLast[IsaacLag[2:length(IsaacLag)] == Isaac[2:length(Isaac)]] <- 1
SameAsLast
mean(SameAsLast)
binom.test(sum(SameAsLast == 1), n = length(SameAsLast), p = 0.5, alternative = "two.sided")

##Win Percentage over time
TimiwinPercentage <- df$TimiCumulative/df$Match
df$winpercent <- TimiwinPercentage
IsaacwinPercentage <- df$IsaacCumulative/df$Match
df$winpercent <- IsaacwinPercentage

percentplot <- ggplot() +
  geom_line(data = df, aes(Match, TimiwinPercentage, color = "Blue")) +
  geom_smooth(data = df, aes(Match, TimiwinPercentage)) +
  geom_line(data = df, aes(Match, IsaacwinPercentage, color = "Red")) +
  geom_smooth(data = df, aes(Match, IsaacwinPercentage)) +
  scale_colour_discrete(name = "Contenders",
                        breaks = c("Blue", "Red"), 
                        labels = c("Timi", "Issac")) 
percentplot

#### Post Isaac Streak ####
Isaac <- c(0,1,0,1,0,1,0,1,0,0,1,0,1,1,0,1,1,0,1,1,0,0,1,0,0,1,0,0,1,1,
           0,1,0,1,1,0,0,1,0,1,0,1,1,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,1,0,1,0,1,0,1,0,1,0,0,0,0,1,1,1,
           #END OF THE FIRST EPOCH - Best of 100 won by Isaac on tiebreak 51-50 *eye roll*
           0,1,0,0,0,0,1,0,0
)
#### Automatic ####
Timi <- 1 - Isaac
df <- as.data.frame(cbind(Isaac, Timi))
df$IsaacCumulative = df$Isaac
df$TimiCumulative = df$Timi
df$Match = 1
for(i in 1:length(df$Isaac)) {
  if(i == 1) {
    df$IsaacCumulative[i] = df$Isaac[i]
    df$TimiCumulative[i] = df$Timi[i]
  } else {
    df$IsaacCumulative[i] = df$IsaacCumulative[i-1] + df$Isaac[i]
    df$TimiCumulative[i] = df$TimiCumulative[i-1] + df$Timi[i]
    df$Match[i] = i
  }
}
df <- rbind(c(0,0,0,0,0), df)
df$Lead <- df$IsaacCumulative - df$TimiCumulative

LineX <- rep(max(df$Match),2)
LineY <- c(df$IsaacCumulative[df$Match == max(df$Match)], df$TimiCumulative[df$Match == max(df$Match)])
dfLine <- as.data.frame(cbind(LineX, LineY))
dfLine$Type <- "Current"

LineX <- rep(df$Match[df$Lead == max(df$Lead)], 2)
LineY <- c(df$IsaacCumulative[df$Lead == max(df$Lead)], df$TimiCumulative[df$Lead == max(df$Lead)])
dfLine_I <- as.data.frame(cbind(LineX, LineY))
dfLine_I <- dplyr::arrange(dfLine_I, LineX)
dfLine_I <- tail(dfLine_I, 2)
dfLine_I$Type <- "Isaac"

LineX <- rep(df$Match[df$Lead == min(df$Lead)], 2)
LineY <- c(df$IsaacCumulative[df$Lead == min(df$Lead)], df$TimiCumulative[df$Lead == min(df$Lead)])
dfLine_T <- as.data.frame(cbind(LineX, LineY))
dfLine_T <- dplyr::arrange(dfLine_T, LineX)
dfLine_T <- tail(dfLine_T, 2)
dfLine_T$Type <- "Timi"

dfLines <- rbind(dfLine, dfLine_I, dfLine_T)

dfalt <- filter(df, Match == max(Match))

limmax = length(Isaac)

df$LeadFlag <- 0
df$LeadFlag[df$Match == max(df$Match)] <- 1
df$LeadFlag[df$Lead == min(df$Lead) & df$Match == max(df$Match[df$Lead == min(df$Lead)])] <- 1
df$LeadFlag[df$Lead == max(df$Lead) & df$Match == max(df$Match[df$Lead == max(df$Lead)])] <- 1
dfTextLead <- filter(df, LeadFlag == 1)
for(i in 1:length(dfTextLead$Match)) {
  dfTextLead$LowValue[i] <- min(dfTextLead$IsaacCumulative[i], dfTextLead$TimiCumulative[i])
}
dfTextLead$Lead <- abs(dfTextLead$Lead)

##Add largest streak line and label
df$CurrentStreak <- 1
for(i in 2:length(df$Match)) {
  if(df$Isaac[i] == 1 & df$Isaac[i-1] == 1) {
    df$CurrentStreak[i] <- df$CurrentStreak[i-1] + 1
  } else if(df$Timi[i] == 1 & df$Timi[i-1] == 1) {
    df$CurrentStreak[i] <- df$CurrentStreak[i-1] + 1
  }
}
dfTextStreak <- df[df$CurrentStreak == max(df$CurrentStreak),]
Streak_X <- c(dfTextStreak$Match, dfTextStreak$Match - dfTextStreak$CurrentStreak)
Streak_Y <- rep(max(dfTextStreak$IsaacCumulative, dfTextStreak$TimiCumulative) + 5, 2)
dfLineStreak <- as.data.frame(cbind(Streak_X, Streak_Y))
dfTextStreak <- filter(dfLineStreak, Streak_X == min(Streak_X))
dfTextStreak$Text <- paste0("Longest Streak = ", max(dfLineStreak$Streak_X) - min(dfLineStreak$Streak_X))

WinPlotPostStreak <- ggplot() +
  geom_line(data = df, aes(Match, TimiCumulative, color = "Blue")) +
  geom_line(data = df, aes(Match, IsaacCumulative, color = "Red")) +
  geom_line(data = dfLines, aes(LineX, LineY, linetype = Type)) +
  geom_line(data = dfLineStreak, aes(Streak_X, Streak_Y)) +
  geom_text(data = dfTextLead, aes(Match, LowValue, label = Lead), hjust = 0, vjust = 1) +
  geom_text(data = dfTextStreak, aes(Streak_X, Streak_Y, label = Text), hjust = 0, vjust = -1) +
  scale_colour_discrete(name = paste0("Player (Wins; Pct of ", length(Isaac), ")"), labels = c(paste0("Timi (", sum(Isaac == 0), "; ", round(sum(Isaac == 0) * 100 / length(Isaac), 0), "%)"), paste0("Isaac (", sum(Isaac == 1), "; ", round(sum(Isaac == 1) * 100 / length(Isaac), 0), "%)"))) +
  scale_linetype_discrete(name = "Largest Lead") +
  ylab("Matches Won") +
  xlim(0, limmax) + ylim(0, limmax)
WinPlotPostStreak  

##Win Percentage over time
TimiwinPercentage <- df$TimiCumulative/df$Match
df$winpercent <- TimiwinPercentage
IsaacwinPercentage <- df$IsaacCumulative/df$Match
df$winpercent <- IsaacwinPercentage

PSpercentplot <- ggplot() +
  geom_line(data = df, aes(Match, TimiwinPercentage, color = "Blue")) +
  geom_line(data = df, aes(Match, IsaacwinPercentage, color = "Red")) +
  scale_colour_discrete(name = "Contenders",
                        breaks = c("Blue", "Red"), 
                        labels = c("Timi", "Issac")) 
PSpercentplot

