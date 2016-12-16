########### ScorePlot Data Prep
library(tidyr); library(dplyr)

load(file = "Manipulated Data.RData")

dfScorePlot <- dfMatch

rollingSum <- function(x, n) {
  
  stats::filter(x, rep(1, n), sides = 1)
  
}

dfScorePlot <- dfScorePlot %>%
  filter(!is.na(WinningScore)) %>%
  mutate(ScoreDiff = IsaacFinalScore / (TimiFinalScore + IsaacFinalScore),
         ScoreDiffAll = cumsum(IsaacFinalScore) / (cumsum(TimiFinalScore) + cumsum(IsaacFinalScore)),
         ScoreDiff5 = rollingSum(IsaacFinalScore, 5) / (rollingSum(TimiFinalScore, 5) + rollingSum(IsaacFinalScore, 5)),
         ScoreDiff10 = rollingSum(IsaacFinalScore, 10) / (rollingSum(TimiFinalScore, 10)  + rollingSum(IsaacFinalScore, 10)))