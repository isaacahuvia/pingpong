#############################
## 2.3 PointPlot Data Prep ##
#############################

library(tidyr); library(dplyr)
load(file = "dfPoint.RData")

##Create Spaghetti Data.frame with NA Service removed
dfPointSpaghetti <- subset(dfPoint, is.na(Outcome) == FALSE)

## Fill in first 40 entries in Serve Index Based off Who Won Rally ##
dfPointSpaghetti$ServeIndex <- unlist(lapply(rle(dfPointSpaghetti$Match)$lengths, seq_len))

dfPointSpaghetti$Server[dfPointSpaghetti$WonRally == "Isaac" & dfPointSpaghetti$ServeIndex %in% c(1:5, 11:15, 21:25, 31:35)] <- "Isaac"
dfPointSpaghetti$Server[dfPointSpaghetti$WonRally == "Timi" & dfPointSpaghetti$ServeIndex %in% c(1:5, 11:15, 21:25, 31:35)] <- "Timi"

dfPointSpaghetti$Server[dfPointSpaghetti$WonRally == "Isaac" & dfPointSpaghetti$ServeIndex %in% c(6:10, 16:20, 26:30, 36:40)] <- "Timi"
dfPointSpaghetti$Server[dfPointSpaghetti$WonRally == "Timi" & dfPointSpaghetti$ServeIndex %in% c(6:10, 16:20, 26:30, 36:40)] <- "Isaac"

##Fill in Server for Extra Time ##
dfPointSpaghetti$Server[dfPointSpaghetti$Outcome == "Timi: Isaac Serve"] <- "Timi"
dfPointSpaghetti$Server[dfPointSpaghetti$Outcome == "Timi: Isaac Rally"] <- "Timi"
dfPointSpaghetti$Server[dfPointSpaghetti$Outcome == "Timi: Timi Rally"] <- "Timi"
dfPointSpaghetti$Server[dfPointSpaghetti$Outcome == "Timi: Timi Serve"] <- "Timi"

dfPointSpaghetti$Server[dfPointSpaghetti$Outcome == "Isaac: Isaac Serve"] <- "Isaac"
dfPointSpaghetti$Server[dfPointSpaghetti$Outcome == "Isaac: Isaac Rally"] <- "Isaac"
dfPointSpaghetti$Server[dfPointSpaghetti$Outcome == "Isaac: Timi Rally"] <- "Isaac"
dfPointSpaghetti$Server[dfPointSpaghetti$Outcome == "Isaac: Timi Serve"] <- "Isaac"

## Add Player and InGameScore
dfPointSpaghettiNA <- tidyr::gather(dfPointSpaghetti, Player, InGameScore, TimiInGameScore:IsaacInGameScore)
dfPointSpaghettiNA$Player[dfPointSpaghettiNA$Player == "TimiInGameScore"] <- "Timi"
dfPointSpaghettiNA$Player[dfPointSpaghettiNA$Player == "IsaacInGameScore"] <- "Isaac"

##Create In-Game Statistics
dfPointSpaghetti <- dfPointSpaghetti %>%
  dplyr::group_by(Match) %>%
  dplyr::mutate(TimiServeUnreturned = sum(Server == "Timi" & PointType == "Serve" & WonPoint == "Timi")) %>%
  dplyr::mutate(IsaacServeUnreturned = sum(Server == "Isaac" & PointType == "Serve" & WonPoint == "Isaac")) %>%
  dplyr::mutate(TimiServeError = sum(Server == "Timi" & PointType == "Serve" & WonPoint == "Isaac")) %>%
  dplyr::mutate(IsaacServeError = sum(Server == "Isaac" & PointType == "Serve" & WonPoint == "Timi"))
  

#Merge on comeback size
dfPointSpaghetti <- merge(dfPointSpaghetti, dplyr::select(dfMatch, Match, IsaacComebackSize, TimiComebackSize), by = "Match")

