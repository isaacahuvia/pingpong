##########
##  QC  ##
##########

dfMatch$QCFlag <- FALSE
dfMatch$QCFlag[dfMatch$IsaacFinalScore != dfMatch$IsaacFinalScoreRecorded] <- TRUE
dfMatch$QCFlag[dfMatch$TimiFinalScore != dfMatch$TimiFinalScoreRecorded] <- TRUE