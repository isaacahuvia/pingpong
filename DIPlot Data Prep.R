########### DIPlot Data Prep
library(tidyr); library(dplyr)

load(file = "Manipulated Data.RData")

dfDIPlot <- dfMatch

dfDIPlot$DIIsaac <- NA

for(i in 10:nrow(dfDIPlot)) {
  
  if(!is.na(dfDIPlot$IsaacFinalScore[i-9])) {
    
    dfDIPlot$DIIsaac[i] <- (dfDIPlot$IsaacFinalScore[i] - dfDIPlot$TimiFinalScore[i])*(10/10) + 
      (dfDIPlot$IsaacFinalScore[i-1] - dfDIPlot$TimiFinalScore[i-1])*(9/10) +
      (dfDIPlot$IsaacFinalScore[i-2] - dfDIPlot$TimiFinalScore[i-2])*(8/10) +
      (dfDIPlot$IsaacFinalScore[i-3] - dfDIPlot$TimiFinalScore[i-3])*(7/10) +
      (dfDIPlot$IsaacFinalScore[i-4] - dfDIPlot$TimiFinalScore[i-4])*(6/10) +
      (dfDIPlot$IsaacFinalScore[i-5] - dfDIPlot$TimiFinalScore[i-5])*(5/10) +
      (dfDIPlot$IsaacFinalScore[i-6] - dfDIPlot$TimiFinalScore[i-6])*(4/10) +
      (dfDIPlot$IsaacFinalScore[i-7] - dfDIPlot$TimiFinalScore[i-7])*(3/10) +
      (dfDIPlot$IsaacFinalScore[i-8] - dfDIPlot$TimiFinalScore[i-8])*(2/10) +
      (dfDIPlot$IsaacFinalScore[i-9] - dfDIPlot$TimiFinalScore[i-9])*(1/10)
  }
  
}