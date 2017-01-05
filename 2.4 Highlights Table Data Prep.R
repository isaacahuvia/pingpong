###############################
##  2.4 Create dfHighlights  ##
###############################

library(dplyr)
load(file = "dfMatch.RData")

dfHighlights_Isaac <- dfMatch %>%
  dplyr::summarize(BiggestWin = max(IsaacFinalScore - TimiFinalScore, na.rm = T),
                   BiggestComeback = max(IsaacComebackSize, na.rm = T),
                   MostPointsScored = max(IsaacFinalScore, na.rm = T),
                   LongestWinStreak = max(IsaacStreak, na.rm = T),
                   LargestLeadGames = max(IsaacLead))

dfHighlights_Timi <- dfMatch %>%
  dplyr::summarize(BiggestWin = max(TimiFinalScore - IsaacFinalScore, na.rm = T),
                   BiggestComeback = max(TimiComebackSize, na.rm = T),
                   MostPointsScored = max(TimiFinalScore, na.rm = T),
                   LongestWinStreak = max(TimiStreak, na.rm = T),
                   LargestLeadGames = max(TimiLead))

dfHighlights_Names <- c("Biggest Win",
                        "Biggest Comeback",
                        "Most Points Scored",
                        "Longest Win Streak",
                        "Largest Lead (Games)")

dfHighlights_IsaacIcons <- rep("", length(dfHighlights_Isaac))
dfHighlights_IsaacIcons[dfHighlights_Isaac > dfHighlights_Timi] <- "<img src='IsaacImage.jpg' height=50 width=50></img>"

dfHighlights_TimiIcons <- rep("", length(dfHighlights_Timi))
dfHighlights_TimiIcons[dfHighlights_Isaac < dfHighlights_Timi] <- "<img src='TimiImage.jpg' height=50 width=50></img>"

dfHighlights <- as.data.frame(cbind(t(dfHighlights_Isaac),
                                    dfHighlights_IsaacIcons,
                                    dfHighlights_Names,
                                    dfHighlights_TimiIcons,
                                    t(dfHighlights_Timi)))

names(dfHighlights) <- c("Isaac", "", "Metric", "", "Timi")