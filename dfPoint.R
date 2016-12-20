############################
## Create Point Dataframe ##
############################

#### Create dfPoint ####

rm(list = ls())
library(tidyr); library(dplyr)
load(file = "Manipulated Data.RData")

## Create dfPoint dataframe ##
# 
# #Test
# dfPointTest <- dfMatch[204:207, ]
# 
# dfPointTest <-dfPointTest %>%
#                  gather(Service, Outcome, Service1Serve1:ExtraServiceServe20) %>%
#                  arrange(Match)
# dfPointTest <- dfPointTest[, c("Match", "Service", "Outcome")]

#Actual

dfPoint <- dfMatch



dfPoint <-dfPoint %>%
                gather(Service, Outcome, Service1Serve1:ExtraServiceServe20) %>%
                arrange(Match)

save(dfPoint, file = "Raw Point Data.RData")
save(dfMatch, file = "dfMatch.RData")
                     