##########################
##  1.5 Update dfMatch  ##
##########################

rm(list = ls())
library(tidyr); library(dplyr)
load(file = "dfMatch.RData")
load(file = "Raw Point Data.RData")

for(i in min(dfPoint$Match[!is.na(dfPoint$IsaacInGameScore)]):length(dfMatch$Match)) {
  
  dfMatch$IsaacFinalScoreRecorded[dfMatch$Match == i] <- max(dfPoint$IsaacInGameScore[dfPoint$Match == i])
  dfMatch$TimiFinalScoreRecorded[dfMatch$Match == i] <- max(dfPoint$TimiInGameScore[dfPoint$Match == i])
  dfMatch$IsaacComebackSize[dfMatch$Match == i & dfMatch$Winner == "Isaac"] <- (-1)*min(dfPoint$IsaacInGameLead[dfPoint$Match == i & dfPoint$ServeIndex >= 20 & dfPoint$Winner == "Isaac"])
  dfMatch$TimiComebackSize[dfMatch$Match == i & dfMatch$Winner == "Timi"] <- (-1)*min(dfPoint$TimiInGameLead[dfPoint$Match == i & dfPoint$ServeIndex >= 20 & dfPoint$Winner == "Timi"])
  
}

dfMatch$IsaacFinalScoreRecorded[dfMatch$IsaacFinalScoreRecorded == 0] <- NA
dfMatch$TimiFinalScoreRecorded[dfMatch$TimiFinalScoreRecorded == 0] <- NA
dfMatch$IsaacComebackSize[dfMatch$IsaacComebackSize < 1] <- NA
dfMatch$TimiComebackSize[dfMatch$TimiComebackSize < 1] <- NA


#### Save ####
save(dfPoint, file = "dfPoint.RData")
save(dfMatch, file = "dfMatch.RData")