########### DIPlot Data Prep
library(tidyr); library(dplyr)

load(file = "Manipulated Data.RData")

dfDIPlot <- dfMatch

dfDIPlot$DIIsaac <- NA

for(i in 10:nrow(dfDIPlot)) {
  
  if(!is.na(dfDIPlot$IsaacScore[i-9])) {
    
    dfDIPlot$DIIsaac[i] <- (dfDIPlot$IsaacScore[i] - dfDIPlot$TimiScore[i])*(10/10) + 
      (dfDIPlot$IsaacScore[i-1] - dfDIPlot$TimiScore[i-1])*(9/10) +
      (dfDIPlot$IsaacScore[i-2] - dfDIPlot$TimiScore[i-2])*(8/10) +
      (dfDIPlot$IsaacScore[i-3] - dfDIPlot$TimiScore[i-3])*(7/10) +
      (dfDIPlot$IsaacScore[i-4] - dfDIPlot$TimiScore[i-4])*(6/10) +
      (dfDIPlot$IsaacScore[i-5] - dfDIPlot$TimiScore[i-5])*(5/10) +
      (dfDIPlot$IsaacScore[i-6] - dfDIPlot$TimiScore[i-6])*(4/10) +
      (dfDIPlot$IsaacScore[i-7] - dfDIPlot$TimiScore[i-7])*(3/10) +
      (dfDIPlot$IsaacScore[i-8] - dfDIPlot$TimiScore[i-8])*(2/10) +
      (dfDIPlot$IsaacScore[i-9] - dfDIPlot$TimiScore[i-9])*(1/10)
  }
  
}