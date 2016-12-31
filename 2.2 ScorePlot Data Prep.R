###############################
##  2.2 ScorePlot Data Prep  ##
###############################

library(tidyr); library(dplyr)
load(file = "dfMatch.RData")

dfScorePlot <- dfMatch

rollingSum <- function(x, n) {
  
  stats::filter(x, rep(1, n), sides = 1)
  
}

dfScorePlot <- dfScorePlot %>%
  filter(!is.na(WinningScore)) %>%
  mutate(ScoreDiff = IsaacFinalScore / (TimiFinalScore + IsaacFinalScore),
         ScoreDiffAll = cumsum(IsaacFinalScore) / (cumsum(TimiFinalScore) + cumsum(IsaacFinalScore)),
         ScoreDiff5 = rollingSum(IsaacFinalScore, 5) / (rollingSum(TimiFinalScore, 5) + rollingSum(IsaacFinalScore, 5)),
         ScoreDiff10 = rollingSum(IsaacFinalScore, 10) / (rollingSum(TimiFinalScore, 10)  + rollingSum(IsaacFinalScore, 10)),
         DIIsaac = ((rollingSum(IsaacFinalScore, 1) / (rollingSum(IsaacFinalScore, 1) + rollingSum(TimiFinalScore, 1))) +
                      (rollingSum(IsaacFinalScore, 2) / (rollingSum(IsaacFinalScore, 2) + rollingSum(TimiFinalScore, 2))) +
                      (rollingSum(IsaacFinalScore, 3) / (rollingSum(IsaacFinalScore, 3) + rollingSum(TimiFinalScore, 3))) +
                      (rollingSum(IsaacFinalScore, 4) / (rollingSum(IsaacFinalScore, 4) + rollingSum(TimiFinalScore, 4))) +
                      (rollingSum(IsaacFinalScore, 5) / (rollingSum(IsaacFinalScore, 5) + rollingSum(TimiFinalScore, 5))) +
                      (rollingSum(IsaacFinalScore, 6) / (rollingSum(IsaacFinalScore, 6) + rollingSum(TimiFinalScore, 6))) +
                      (rollingSum(IsaacFinalScore, 7) / (rollingSum(IsaacFinalScore, 7) + rollingSum(TimiFinalScore, 7))) +
                      (rollingSum(IsaacFinalScore, 8) / (rollingSum(IsaacFinalScore, 8) + rollingSum(TimiFinalScore, 8))) +
                      (rollingSum(IsaacFinalScore, 9) / (rollingSum(IsaacFinalScore, 9) + rollingSum(TimiFinalScore, 9))) +
                      (rollingSum(IsaacFinalScore, 10) / (rollingSum(IsaacFinalScore, 10) + rollingSum(TimiFinalScore, 10)))) / 10)