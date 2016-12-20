################################
## Point Data Quality Control ##
################################

rm(list = ls())
library(tidyr); library(dplyr)
load(file = "Raw Score Data.RData")
load(file = "dfMatch.RData")

#### Check to ensure Final Score and Service Data Align ####

# QualityControl <- function(x) {
#     if(dfPoint$ServeIndex == 60 & dfPoint$TimiInGameScore == dfPoint$TimiFinalScore & !is.na(dfPoint$Timestamp)) {
#       dfPoint$TimiPointTotal <- TRUE
#     }else{
#       dfPoint$TimiPointTotal <- FALSE
#     }
#   }
QualityControl <- function(x) {
  if(dfPoint$ServeIndex == 60 & dfPoint$TimiInGameScore == dfPoint$TimiFinalScore & !is.na(dfPoint$Timestamp)) {
    dfPoint$TimiPointTotal <- TRUE
  }else if(dfPoint$ServeIndex == 60 & dfPoint$TimiInGameScore != dfPoint$TimiFinalScore & !is.na(dfPoint$Timestamp)){
    dfPoint$TimiPointTotal <- FALSE
  }
}

unlist(lapply(dfPoint, QualityControl))

## Flag Whether Timi In-Game Score or Isaac In-Game Score Matches with recorded FinalScore ##
dfPoint$TimiPointCorrect[dfPoint$ServeIndex == 60 & dfPoint$TimiInGameScore == dfPoint$TimiFinalScore & !is.na(dfPoint$Timestamp)] <- TRUE
dfPoint$TimiPointCorrect[dfPoint$ServeIndex == 60 & dfPoint$TimiInGameScore != dfPoint$TimiFinalScore & !is.na(dfPoint$Timestamp)] <- FALSE

dfPoint$IsaacPointCorrect[dfPoint$ServeIndex == 60 & dfPoint$IsaacInGameScore == dfPoint$IsaacFinalScore & !is.na(dfPoint$Timestamp)] <- TRUE
dfPoint$IsaacPointCorrect[dfPoint$ServeIndex == 60 & dfPoint$IsaacInGameScore != dfPoint$IsaacFinalScore & !is.na(dfPoint$Timestamp)] <- FALSE

## Flag if both Timi In-Game Score and Isaac In-Game Score Accurate ##
dfPoint$GamePointCorrect[dfPoint$TimiPointCorrect == TRUE & dfPoint$IsaacPointCorrect == TRUE] <- TRUE
dfPoint$GamePointCorrect[dfPoint$TimiPointCorrect == FALSE | dfPoint$IsaacPointCorrect == FALSE] <- FALSE

    
save(dfPoint, file = "Point Quality Control Data.RData")
save(dfMatch, file = "dfMatch.RData")

