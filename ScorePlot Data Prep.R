########### ScorePlot Data Prep
library(tidyr); library(dplyr)

load(file = "Manipulated Data.RData")

dfScorePlot <- dfMatch

rollingSum <- function(x, n) {
  
  stats::filter(x, rep(1, n), sides = 1)
  
}

dfScorePlot <- dfScorePlot %>%
  filter(!is.na(WinningScore)) %>%
  mutate(ScoreDiff = IsaacScore / (TimiScore + IsaacScore),
         ScoreDiffAll = cumsum(IsaacScore) / (cumsum(TimiScore) + cumsum(IsaacScore)),
         ScoreDiff5 = rollingSum(IsaacScore, 5) / (rollingSum(TimiScore, 5) + rollingSum(IsaacScore, 5)),
         ScoreDiff10 = rollingSum(IsaacScore, 10) / (rollingSum(TimiScore, 10)  + rollingSum(IsaacScore, 10)))