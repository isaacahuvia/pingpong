####### Data Cleaning
rm(list = ls())
library(tidyr); library(dplyr); library(plyr)

load(file = "Scraped Pre-Form Data.RData")
load(file = "Scraped Form Response Data.RData")

dfPre <- dfPre[,1:7]
dfRes <- dfRes[,1:67]

dfPre <- as.data.frame(
    lapply(dfPre, function(x){
    x <- c(strsplit(x[1], " ")[[1]], x[2:length(x)])
  }),
  stringsAsFactors = FALSE
)

names(dfPre) <- dfPre[1,]
names(dfRes) <- gsub(x = dfRes[1,], pattern = "\\s|\\[|\\]", replacement = "")
  
dfPre <- dfPre[2:nrow(dfPre),]
dfRes <- dfRes[2:nrow(dfRes),]

rownames(dfPre) <- seq(length = nrow(dfPre))
rownames(dfRes) <- seq(length = nrow(dfRes))

dfPre[dfPre == "NA"] <- NA
dfRes[dfRes == "NA"] <- NA

x <- dfRes[nrow(dfRes),length(dfRes)]
dfRes[,8:length(dfRes)][dfRes[,8:length(dfRes)] == x] <- NA

dfMatch <- as.data.frame(rbind.fill(dfPre, dfRes))

dfMatch <- dfMatch[,c(1:4,6:length(dfMatch))]

save(dfMatch, file = "Cleaned Data.RData")