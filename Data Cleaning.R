####### Data Cleaning
rm(list = ls())
library(tidyr); library(dplyr)

load(file = "Scraped Data.RData")

dfMatch <- as.data.frame(
    lapply(dfMatch, function(x){
    x <- c(strsplit(x[1], " ")[[1]], x[2:length(x)])
  }),
  stringsAsFactors = FALSE
)

names(dfMatch) <- dfMatch[1,]

dfMatch <- dfMatch[2:nrow(dfMatch),] 

rownames(dfMatch) <- seq(length=nrow(dfMatch))

dfMatch[dfMatch == "NA"] <- NA


save(dfMatch, file = "Cleaned Data.RData")