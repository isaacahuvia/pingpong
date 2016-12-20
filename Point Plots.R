#########################
## In-Game Point Plots ##
#########################

rm(list = ls())
library(tidyr); library(dplyr); library(ggplot2); library (shiny)
load(file = "Point Quality Control Data.RData")

dfPointTest <- dfPoint[12901:12946, ]
#Match 216
ggplot() +
  geom_line(data = dfPointTest, aes(ServeIndex, TimiInGameScore, color = "Blue")) +
  geom_line(data = dfPointTest, aes(ServeIndex, IsaacInGameScore, color = "Red")) 

ggplot() +
  geom_line(data = subset(dfPoint, Match == 205 & is.na(Outcome) == FALSE), aes(ServeIndex, TimiInGameScore, color = "Blue")) +
  geom_smooth(data = subset(dfPoint, Match == 205 & is.na(Outcome) == FALSE), aes(ServeIndex, TimiInGameScore, color = "Blue")) +
  geom_line(data = subset(dfPoint, Match == 205 & is.na(Outcome) == FALSE), aes(ServeIndex, IsaacInGameScore, color = "Red")) +
  geom_smooth(data = subset(dfPoint, Match == 205 & is.na(Outcome) == FALSE), aes(ServeIndex, IsaacInGameScore, color = "Red"))

ggplot(data = subset(dfPoint, is.na(Outcome) == FALSE), aes(ServeIndex, TimiInGameScore, colour = Match)) +
  geom_path()
  geom_point(data = subset(dfPoint, is.na(Outcome) == FALSE), aes(ServeIndex, IsaacInGameScore, color = "Red")) +
    
## Wrong but looks good
ggplot() + 
    geom_path(data = subset(dfPoint, is.na(Outcome) == FALSE), aes(ServeIndex, TimiInGameScore, colour = "Blue")) +
    geom_path(data = subset(dfPoint, is.na(Outcome) == FALSE), aes(ServeIndex, IsaacInGameScore, colour = "Red")) +
    geom_smooth(data = subset(dfPoint, is.na(Outcome) == FALSE), aes(ServeIndex, IsaacInGameScore, colour = "Blue"), span = .5) +
    geom_smooth(data = subset(dfPoint, is.na(Outcome) == FALSE), aes(ServeIndex, TimiInGameScore, colour = "Red"), span = .5)
  
##spaghetti plot
ggplot() + 
        geom_line(data = subset(dfPoint, is.na(Outcome) == FALSE), 
                  aes(ServeIndex, TimiInGameScore, group = Match, colour = "Blue")) +
        geom_line(data = subset(dfPoint, is.na(Outcome) == FALSE), 
                  aes(ServeIndex, IsaacInGameScore, group = Match, colour = "Red")) 

##spaghetti plot smoothed All Games
SpagAll <-  ggplot() + 
                  geom_line(data = subset(dfPoint, is.na(Outcome) == FALSE), 
                      aes(ServeIndex, TimiInGameScore, group = Match, colour = "Blue"), 
                      stat = "smooth", method = "loess", alpha = .7, se = FALSE, span = 0.5) +
                  geom_line(data = subset(dfPoint, is.na(Outcome) == FALSE), 
                      aes(ServeIndex, IsaacInGameScore, group = Match, colour = "Red"), 
                      stat = "smooth", method = "loess", alpha = .7, se = FALSE, span = 0.5) +
                  scale_colour_manual(name = "Player", values = c( "Blue" = "Blue", "Red" = "Red"), labels = c("Timi", "Isaac"))
SpagAll

##Spaghetti Highlight a few lines
SpagHighlight <- SpagAll + aes(alpha = alpha, group = factor(Match)) + guides(alpha = FALSE)
SpagHighlight
##Only Smoothed (2 lines)
ggplot() + 
  geom_line(data = subset(dfPoint, is.na(Outcome) == FALSE), stat = "smooth", method = "loess", size = 2, alpha = .4, colour = "Blue", 
              aes(ServeIndex, TimiInGameScore), se = FALSE, span = 0.2) +
  geom_line(data = subset(dfPoint, is.na(Outcome) == FALSE), stat = "smooth", method = "loess", size = 2, alpha = .4, colour = "Red",
              aes(ServeIndex, IsaacInGameScore, colour = "Red"), se = FALSE, span = 0.2)
##Spaghetti Transparent Base
SpagBase <-  ggplot() + 
  geom_line(data = subset(dfPoint, is.na(Outcome) == FALSE), 
            aes(ServeIndex, TimiInGameScore, group = Match, colour = "Blue"), 
            stat = "smooth", method = "loess", alpha = .2, se = FALSE, span = 0.5) +
  geom_line(data = subset(dfPoint, is.na(Outcome) == FALSE), 
            aes(ServeIndex, IsaacInGameScore, group = Match, colour = "Red"), 
            stat = "smooth", method = "loess", alpha = .2, se = FALSE, span = 0.5) +
  scale_colour_manual(name = "Player", values = c( "Blue" = "Blue", "Red" = "Red"), labels = c("Timi", "Isaac"))

## Match 205
Spag205 <-  ggplot() + 
  geom_line(data = subset(dfPoint, Match == 205 & is.na(Outcome) == FALSE), 
            aes(ServeIndex, TimiInGameScore, group = Match, colour = "Blue"), 
            stat = "smooth", method = "loess", size = 2, se = FALSE, span = 0.5) +
  geom_line(data = subset(dfPoint, Match == 205 & is.na(Outcome) == FALSE), 
            aes(ServeIndex, IsaacInGameScore, group = Match, colour = "Red"), 
            stat = "smooth", method = "loess", size = 2, se = FALSE, span = 0.5) +
  scale_colour_manual(name = "Player", values = c( "Blue" = "Blue", "Red" = "Red"), labels = c("Timi", "Isaac"))
Spag205
## Base + Match 205 Highlighted
Spag_205 <-  ggplot() + 
                geom_line(data = subset(dfPoint, is.na(Outcome) == FALSE), 
                          aes(ServeIndex, TimiInGameScore, group = Match, colour = "Blue"), 
                          stat = "smooth", method = "loess", alpha = .2, se = FALSE, span = 0.5) +
                geom_line(data = subset(dfPoint, is.na(Outcome) == FALSE), 
                          aes(ServeIndex, IsaacInGameScore, group = Match, colour = "Red"), 
                          stat = "smooth", method = "loess", alpha = .2, se = FALSE, span = 0.5) +
                geom_line(data = subset(dfPoint, Match == 205 & is.na(Outcome) == FALSE), 
                          aes(ServeIndex, TimiInGameScore, group = Match, colour = "Blue"), 
                          stat = "smooth", method = "loess", alpha = 0.6, size = 2, se = FALSE, span = 0.5) +
                geom_line(data = subset(dfPoint, Match == 205 & is.na(Outcome) == FALSE), 
                          aes(ServeIndex, IsaacInGameScore, group = Match, colour = "Red"), 
                          stat = "smooth", method = "loess", alpha = 0.6, size = 2, se = FALSE, span = 0.5) +
                scale_colour_manual(name = "Player", values = c( "Blue" = "Blue", "Red" = "Red"), labels = c("Timi", "Isaac"))

Spag_205  
