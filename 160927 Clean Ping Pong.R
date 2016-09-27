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

#####  Input Data  #####
df <- cbind(
  #Wins (1: Isaac, 0: Timi)
  c(0,0,0,1,0,1,0,1,1,0,1,0,0,1,1,1,1,1,1,1,0,1,0,1,0,1,0,1,0,0,1,0,1,1,0,1,1,0,1,1,0,0,1,0,0,1,0,0,1,1,
    0,1,0,1,1,0,0,1,0,1,0,1,1,1,0,0,1,0,1,0,0,1,0,0,1,0,1,0,0,1,0,0,1,0,1,1,0,1,0,1,0,1,0,1,0,0,0,0,1,1,1,
                                    #END OF THE FIRST EPOCH - Best of 100 won by Isaac on tiebreak 51-50
    0,1,0,0,0,0,1,0,0,1
    ),
  #Winning scores
  c(rep(NA, 101),
    21,21,22,21,21,21,21,21,21,26
    ),
  #Losing scores
  c(rep(NA, 101), 
    17,17,20,18,15,18,19,12,13,24
    )
) %>% as.data.frame


#####  Data Manipulation  #####
#Rename, add match-independent variables
names(df) <- c("Isaac", "WinningScore", "LosingScore")

df$Timi <- 1 - df$Isaac #Games that Timi wins Isaac does not

df$Match <- seq(1, nrow(df), 1) #Keep track of match index 

df$Winner <- "Isaac"
df$Winner[df$Isaac == 0] <- "Timi"

#Build cumulative wins, lead variables
df$IsaacCumulative = df$Isaac
df$TimiCumulative = df$Timi
for(i in 1:length(df$Isaac)) {
  if(i == 1) {
    df$IsaacCumulative[i] = df$Isaac[i]
    df$TimiCumulative[i] = df$Timi[i]
  } else {
    df$IsaacCumulative[i] = df$IsaacCumulative[i-1] + df$Isaac[i]
    df$TimiCumulative[i] = df$TimiCumulative[i-1] + df$Timi[i]
  }
}

df$IsaacLead <- df$IsaacCumulative - df$TimiCumulative #Create lead variables
df$TimiLead <- df$TimiCumulative - df$IsaacCumulative

df <- rbind(seq(0, 0, length.out = length(df)), df) #Add a row of leading zeroes, so that the plot shows a (0,0) data point
df$WinningScore[1] <- NA; df$LosingScore[1] <- NA; df$Winner[1] <- NA #Keep leading values of score, winner variables NA


#####  Build Plot (Messy)  #####
LineX <- rep(max(df$Match),2)
LineY <- c(df$IsaacCumulative[df$Match == max(df$Match)], df$TimiCumulative[df$Match == max(df$Match)])
dfLine <- as.data.frame(cbind(LineX, LineY))
dfLine$Type <- "Current"

LineX <- rep(df$Match[df$IsaacLead == max(df$IsaacLead)], 2)
LineY <- c(df$IsaacCumulative[df$IsaacLead == max(df$IsaacLead)], df$TimiCumulative[df$IsaacLead == max(df$IsaacLead)])
dfLine_I <- as.data.frame(cbind(LineX, LineY))
dfLine_I <- dplyr::arrange(dfLine_I, LineX)
dfLine_I <- tail(dfLine_I, 2)
dfLine_I$Type <- "Isaac"

LineX <- rep(df$Match[df$IsaacLead == min(df$IsaacLead)], 2)
LineY <- c(df$IsaacCumulative[df$IsaacLead == min(df$IsaacLead)], df$TimiCumulative[df$IsaacLead == min(df$IsaacLead)])
dfLine_T <- as.data.frame(cbind(LineX, LineY))
dfLine_T <- dplyr::arrange(dfLine_T, LineX)
dfLine_T <- tail(dfLine_T, 2)
dfLine_T$Type <- "Timi"

dfLines <- rbind(dfLine, dfLine_I, dfLine_T)

dfalt <- filter(df, Match == max(Match))

limmax = nrow(df)

df$LeadFlag <- 0
df$LeadFlag[df$Match == max(df$Match)] <- 1
df$LeadFlag[df$IsaacLead == min(df$IsaacLead) & df$Match == max(df$Match[df$IsaacLead == min(df$IsaacLead)])] <- 1
df$LeadFlag[df$IsaacLead == max(df$IsaacLead) & df$Match == max(df$Match[df$IsaacLead == max(df$IsaacLead)])] <- 1
dfTextLead <- filter(df, LeadFlag == 1)
for(i in 1:length(dfTextLead$Match)) {
  dfTextLead$LowValue[i] <- min(dfTextLead$IsaacCumulative[i], dfTextLead$TimiCumulative[i])
}
dfTextLead$IsaacLead <- abs(dfTextLead$IsaacLead)

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
  geom_text(data = dfTextLead, aes(Match, LowValue, label = IsaacLead), hjust = 0, vjust = 1) +
  geom_text(data = dfTextStreak, aes(Streak_X, Streak_Y, label = Text), hjust = 0, vjust = -1) +
  scale_colour_discrete(name = paste0("Player (Wins; Pct of ", length(df$Isaac), ")"), labels = c(paste0("Timi (", sum(df$Isaac == 0), "; ", round(sum(df$Isaac == 0) * 100 / length(df$Isaac), 0), "%)"), paste0("Isaac (", sum(df$Isaac == 1), "; ", round(sum(df$Isaac == 1) * 100 / length(df$Isaac), 0), "%)"))) +
  scale_linetype_discrete(name = "Largest Lead") +
  ylab("Matches Won") +
  xlim(0, limmax) + ylim(0, limmax)
WinPlot


#####  Extra Analyses  #####
### Binomial test of alternation
SameAsLast <- 0
IsaacLag <- NULL
for(i in 1:length(df$Isaac)) {
  IsaacLag[i] <- df$Isaac[i-1]
}
SameAsLast[IsaacLag[2:length(IsaacLag)] != df$Isaac[2:length(df$Isaac)]] <- 0
SameAsLast[IsaacLag[2:length(IsaacLag)] == df$Isaac[2:length(df$Isaac)]] <- 1
SameAsLast
mean(SameAsLast)
binom.test(sum(SameAsLast == 1), n = length(SameAsLast), p = 0.5, alternative = "two.sided")

### Score analysis
df %>%
  filter(!is.na(WinningScore)) %>%
  group_by(Winner) %>%
  summarise(TotalWins = n(),
            AvgMargin = mean(WinningScore - LosingScore),
            LargestMargin = max(WinningScore - LosingScore),
            HighestWinningScore = max(WinningScore))