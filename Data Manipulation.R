####### Data manipulation
rm(list = ls())
library(tidyr); library(dplyr)

load(file = "Cleaned Data.RData")

#recreate old variables necessary for plots

dfMatch$Match <- seq(length = nrow(dfMatch)) #Keep track of match index 

dfMatch$IsaacScore <- NA
dfMatch$IsaacScore[dfMatch$Winner == "Isaac"] <- dfMatch$WinningScore[dfMatch$Winner == "Isaac"]
dfMatch$IsaacScore[dfMatch$Winner == "Timi"] <- dfMatch$LosingScore[dfMatch$Winner == "Timi"]
dfMatch$IsaacScore <- as.numeric(dfMatch$IsaacScore)

dfMatch$TimiScore <- NA
dfMatch$TimiScore[dfMatch$Winner == "Timi"] <- dfMatch$WinningScore[dfMatch$Winner == "Timi"]
dfMatch$TimiScore[dfMatch$Winner == "Isaac"] <- dfMatch$LosingScore[dfMatch$Winner == "Isaac"]
dfMatch$TimiScore <- as.numeric(dfMatch$TimiScore)

#cumulative variables
dfMatch$IsaacCumulativeWins <- cumsum(dfMatch$Winner == "Isaac")
dfMatch$TimiCumulativeWins <- cumsum(dfMatch$Winner == "Timi")

dfMatch$IsaacLead <- dfMatch$IsaacCumulativeWins - dfMatch$TimiCumulativeWins
dfMatch$TimiLead <- dfMatch$TimiCumulativeWins - dfMatch$IsaacCumulativeWins

dfMatch$IsaacStreak <- 0
dfMatch$IsaacStreak[dfMatch$Winner == "Isaac"] <- unlist(lapply(rle(dfMatch$Winner == "Isaac")$lengths[rle(dfMatch$Winner == "Isaac")$values == TRUE], seq_len))

dfMatch$TimiStreak <- 0
dfMatch$TimiStreak[dfMatch$Winner == "Timi"] <- unlist(lapply(rle(dfMatch$Winner == "Timi")$lengths[rle(dfMatch$Winner == "Timi")$values == TRUE], seq_len))

#date
dfMatch$Date <- as.Date(gsub(x = dfMatch$Timestamp, pattern = "\\s\\S+$", replacement = ""), format = "%m/%d/%Y")
#time stored as a HHMM number
dfMatch$Time <- as.numeric(gsub(x = dfMatch$Timestamp, pattern = "^\\S+\\s|:\\S\\S$|:", replacement = ""))
#game number in day
dfMatch$GameInDay <- unlist(lapply(rle(as.character(dfMatch$Date))$lengths, seq_len))
dfMatch$GameInDay[is.na(dfMatch$Timestamp)] <- NA
#morning/afternoon game (before/after 1pm)
dfMatch$TimeOfDay <- NA
dfMatch$TimeOfDay[dfMatch$Time < 1300] <- "Morning"
dfMatch$TimeOfDay[dfMatch$Time >= 1300] <- "Afternoon"


save(dfMatch, file = "Manipulated Data.RData")