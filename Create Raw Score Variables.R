################################
## Create Raw Score Variables ##
################################

rm(list = ls())
library(tidyr); library(dplyr)
load(file = "Raw Point Data.RData")
load(file = "dfMatch.RData")


#### Create Variable to Indicate if Timi of Isaac is Serving ####

## Fill in first 40 entries in Serve Index Based off Who Won Rally ##
dfPoint$ServeIndex <- unlist(lapply(rle(dfPoint$Match)$lengths, seq_len))

dfPoint$Server[dfPoint$WonRally == "Isaac" & dfPoint$ServeIndex %in% c(1:5, 11:15, 21:25, 31:35)] <- "Isaac"
dfPoint$Server[dfPoint$WonRally == "Timi" & dfPoint$ServeIndex %in% c(1:5, 11:15, 21:25, 31:35)] <- "Timi"

dfPoint$Server[dfPoint$WonRally == "Isaac" & dfPoint$ServeIndex %in% c(6:10, 16:20, 26:30, 36:40)] <- "Timi"
dfPoint$Server[dfPoint$WonRally == "Timi" & dfPoint$ServeIndex %in% c(6:10, 16:20, 26:30, 36:40)] <- "Isaac"

##Fill in Server for Extra Time ##
dfPoint$Server[dfPoint$Outcome == "Timi: Isaac Serve"] <- "Timi"
dfPoint$Server[dfPoint$Outcome == "Timi: Isaac Rally"] <- "Timi"
dfPoint$Server[dfPoint$Outcome == "Timi: Timi Rally"] <- "Timi"
dfPoint$Server[dfPoint$Outcome == "Timi: Timi Serve"] <- "Timi"

dfPoint$Server[dfPoint$Outcome == "Isaac: Isaac Serve"] <- "Isaac"
dfPoint$Server[dfPoint$Outcome == "Isaac: Isaac Rally"] <- "Isaac"
dfPoint$Server[dfPoint$Outcome == "Isaac: Timi Rally"] <- "Isaac"
dfPoint$Server[dfPoint$Outcome == "Isaac: Timi Serve"] <- "Isaac"

## Create PointType Variable ##
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


#### Create Variable to Tally Score at Each Point ####

## Create and Fill WonPoint Variable for Regular Time ##
dfPoint$WonPoint[dfPoint$Outcome == "Isaac Serve"] <- "Isaac"
dfPoint$WonPoint[dfPoint$Outcome == "Isaac Rally"] <- "Isaac"

dfPoint$WonPoint[dfPoint$Outcome == "Timi Serve"] <- "Timi"
dfPoint$WonPoint[dfPoint$Outcome == "Timi Rally"] <- "Timi"

## Fill in WonPoint Variable for Extra Time ##
dfPoint$WonPoint[dfPoint$Outcome == "Timi: Isaac Serve"] <- "Isaac"
dfPoint$WonPoint[dfPoint$Outcome == "Timi: Isaac Rally"] <- "Isaac"
dfPoint$WonPoint[dfPoint$Outcome == "Timi: Timi Rally"] <- "Timi"
dfPoint$WonPoint[dfPoint$Outcome == "Timi: Timi Serve"] <- "Timi"

dfPoint$WonPoint[dfPoint$Outcome == "Isaac: Isaac Serve"] <- "Isaac"
dfPoint$WonPoint[dfPoint$Outcome == "Isaac: Isaac Rally"] <- "Isaac"
dfPoint$WonPoint[dfPoint$Outcome == "Isaac: Timi Rally"] <- "Timi"
dfPoint$WonPoint[dfPoint$Outcome == "Isaac: Timi Serve"] <- "Timi"


## Create In-Match Score ##
dfPoint$WonPoint[is.na(dfPoint$WonPoint)] <- 0

dfPoint <- dfPoint %>%
                dplyr::group_by(Match) %>%
                dplyr::mutate(TimiInGameScore = cumsum(WonPoint == "Timi")) %>%
                dplyr::mutate(IsaacInGameScore = cumsum(WonPoint == "Isaac")) 
# 
# ##dfPointTest
# dfPointTest <- dfPoint[12181:length(dfPoint$WonPoint), ]
# 
# dfPointTest <- dfPointTest %>%
#                     dplyr::group_by(Match) %>%
#                     dplyr::mutate(TimiInGameScore = cumsum(WonPoint == "Timi"))
#                     dplyr::mutate(IsaacInGameScore = cumsum(WonPoint == "Isaac"))
#   
#                    
# 
#   
#   
#   cumsum(dfPoint$WonPoint[12181:length(dfPoint$WonPoint)] == "Timi")
# # dfPoint <- dfPoint %>%
#               group_by(Match) %>%
#               mutate(TimiInGameScore = cumsum(WonPoint == "Timi")) %>%
#               mutate(IsaacInGameScore = cumsum(WonPoint == "Timi"))
## Comback Indicator Variable ##
 
  
  

# PointOutcome <- dfPoint$Outcome
# SplitPointOutcome <- strsplit(PointOutcome, " ")
# PointOutcome <- sapply(SplitPointOutcome, "[", 1)
# PointOutcome <- sub(":", "", PointOutcome)
# dfPoint$PointOutcome <- PointOutcome

save(dfPoint, file = "Raw Score Data.RData")
save(dfMatch, file = "dfMatch.RData")
