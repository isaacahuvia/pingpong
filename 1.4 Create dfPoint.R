##########################
##  1.4 Create dfPoint  ##
##########################
#### Create dfPoint ####

rm(list = ls())
library(tidyr); library(dplyr)
load(file = "dfMatch.RData")

dfPoint <- dfMatch %>%
  gather(Service, Outcome, Service1Serve1:ExtraServiceServe20) %>%
  arrange(Match)

#Create serve index for max 60 serves
dfPoint$ServeIndex <- unlist(lapply(rle(dfPoint$Match)$lengths, seq_len))

#Fill in Server for normal time
dfPoint$Server[dfPoint$WonRally == "Isaac" & dfPoint$ServeIndex %in% c(1:5, 11:15, 21:25, 31:35)] <- "Isaac"
dfPoint$Server[dfPoint$WonRally == "Timi" & dfPoint$ServeIndex %in% c(1:5, 11:15, 21:25, 31:35)] <- "Timi"

dfPoint$Server[dfPoint$WonRally == "Isaac" & dfPoint$ServeIndex %in% c(6:10, 16:20, 26:30, 36:40)] <- "Timi"
dfPoint$Server[dfPoint$WonRally == "Timi" & dfPoint$ServeIndex %in% c(6:10, 16:20, 26:30, 36:40)] <- "Isaac"

#Fill in Server for extra time
dfPoint$Server[dfPoint$Outcome == "Timi: Isaac Serve"] <- "Timi"
dfPoint$Server[dfPoint$Outcome == "Timi: Isaac Rally"] <- "Timi"
dfPoint$Server[dfPoint$Outcome == "Timi: Timi Rally"] <- "Timi"
dfPoint$Server[dfPoint$Outcome == "Timi: Timi Serve"] <- "Timi"

dfPoint$Server[dfPoint$Outcome == "Isaac: Isaac Serve"] <- "Isaac"
dfPoint$Server[dfPoint$Outcome == "Isaac: Isaac Rally"] <- "Isaac"
dfPoint$Server[dfPoint$Outcome == "Isaac: Timi Rally"] <- "Isaac"
dfPoint$Server[dfPoint$Outcome == "Isaac: Timi Serve"] <- "Isaac"

## Create PointType Variable
dfPoint$PointType[dfPoint$Outcome == "Timi Serve"] <- "Serve"
dfPoint$PointType[dfPoint$Outcome == "Timi Rally"] <- "Rally"
dfPoint$PointType[dfPoint$Outcome == "Isaac Serve"] <- "Serve"
dfPoint$PointType[dfPoint$Outcome == "Isaac Rally"] <- "Rally"

dfPoint$PointType[dfPoint$Outcome == "Timi: Isaac Serve"] <- "Serve"
dfPoint$PointType[dfPoint$Outcome == "Timi: Isaac Rally"] <- "Rally"
dfPoint$PointType[dfPoint$Outcome == "Timi: Timi Rally"] <- "Rally"
dfPoint$PointType[dfPoint$Outcome == "Timi: Timi Serve"] <- "Serve"

dfPoint$PointType[dfPoint$Outcome == "Isaac: Isaac Serve"] <- "Serve"
dfPoint$PointType[dfPoint$Outcome == "Isaac: Isaac Rally"] <- "Rally"
dfPoint$PointType[dfPoint$Outcome == "Isaac: Timi Rally"] <- "Rally"
dfPoint$PointType[dfPoint$Outcome == "Isaac: Timi Serve"] <- "Serve"

## Create WonPoint variable
#Regular time
dfPoint$WonPoint[dfPoint$Outcome == "Isaac Serve"] <- "Isaac"
dfPoint$WonPoint[dfPoint$Outcome == "Isaac Rally"] <- "Isaac"

dfPoint$WonPoint[dfPoint$Outcome == "Timi Serve"] <- "Timi"
dfPoint$WonPoint[dfPoint$Outcome == "Timi Rally"] <- "Timi"

#Extra time
dfPoint$WonPoint[dfPoint$Outcome == "Timi: Isaac Serve"] <- "Isaac"
dfPoint$WonPoint[dfPoint$Outcome == "Timi: Isaac Rally"] <- "Isaac"
dfPoint$WonPoint[dfPoint$Outcome == "Timi: Timi Rally"] <- "Timi"
dfPoint$WonPoint[dfPoint$Outcome == "Timi: Timi Serve"] <- "Timi"

dfPoint$WonPoint[dfPoint$Outcome == "Isaac: Isaac Serve"] <- "Isaac"
dfPoint$WonPoint[dfPoint$Outcome == "Isaac: Isaac Rally"] <- "Isaac"
dfPoint$WonPoint[dfPoint$Outcome == "Isaac: Timi Rally"] <- "Timi"
dfPoint$WonPoint[dfPoint$Outcome == "Isaac: Timi Serve"] <- "Timi"

## Create in-match score-based variables
dfPoint <- dfPoint %>%
  dplyr::group_by(Match) %>%
  dplyr::mutate(TimiInGameScore = cumsum(ifelse(is.na(WonPoint), 0, WonPoint == "Timi")),
                IsaacInGameScore = cumsum(ifelse(is.na(WonPoint), 0, WonPoint == "Isaac"))) %>%
  dplyr::mutate(TimiInGameLead = TimiInGameScore - IsaacInGameScore,
                IsaacInGameLead = IsaacInGameScore - TimiInGameScore) 

## Mark comeback potential, size of deficit
dfPoint$TimiComebackDeficit <- NA
dfPoint$IsaacComebackDeficit <- NA
dfPoint$TimiComebackDeficit[dfPoint$IsaacInGameLead >= 5 & dfPoint$IsaacInGameScore >= 15] <- dfPoint$TimiInGameLead[dfPoint$IsaacInGameLead >= 5 & dfPoint$IsaacInGameScore >= 15]
dfPoint$IsaacComebackDeficit[dfPoint$TimiInGameLead >= 5 & dfPoint$TimiInGameScore >= 15] <- dfPoint$IsaacInGameLead[dfPoint$TimiInGameLead >= 5 & dfPoint$TimiInGameScore >= 15]

#### Save ####
save(dfPoint, file = "Raw Point Data.RData")
save(dfMatch, file = "dfMatch.RData")