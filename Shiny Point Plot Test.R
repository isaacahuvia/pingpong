############################
## Shiny Point Plots Test ##
############################

library(shiny); library(ggplot2)


setwd("C:\\Users\\timikoyejo\\Documents\\Ping Pong 2.0\\Really-Important-Stuff-Point-Based-Metrics\\Really-Important-Stuff-Point-Based-Metrics")
##Prepare dfMatch and dfPont
source("Scraping.R")

source("Data Cleaning.R")

source("Data Manipulation.R")

source("dfPoint.R")

source("Create Raw Score Variables.R")

source("Point Data Quality Control.R")

##Plot Data Prep
source("Shiny Point Plot Data Prep.R") 

ui <- fluidPage(
  sliderInput(inputId = "Match", label = "Choose Match",
  value = 204, min = 204, max = max(dfPointSpaghetti$Match), step = 1, round = TRUE),
  plotOutput("HighlightMatch")
)


server <- function(input, output) {
  output$HighlightMatch <- renderPlot({
    ##Transparent Base Spaghetti + Select Match
    ggplot() +
      geom_line(data = dfPointSpaghetti,
                aes(ServeIndex, TimiInGameScore, group = Match, colour = "Blue"),
                stat = "smooth", method = "loess", alpha = .15, se = FALSE, span = 0.2) +
      geom_line(data = dfPointSpaghetti,
                aes(ServeIndex, IsaacInGameScore, group = Match, colour = "Red"),
                stat = "smooth", method = "loess", alpha = .15, se = FALSE, span = 0.2) +
      geom_line(data = subset(dfPointSpaghetti, Match == input$Match),
                aes(ServeIndex, TimiInGameScore, group = Match, colour = "Blue"),
                stat = "smooth", method = "loess", alpha = 0.6, size = 1.5, se = FALSE, span = 0.2) +
      geom_line(data = subset(dfPointSpaghetti, Match == input$Match),
                aes(ServeIndex, IsaacInGameScore, group = Match, colour = "Red"),
                stat = "smooth", method = "loess", alpha = 0.6, size = 1.5, se = FALSE, span = 0.2) +
      scale_colour_manual(name = "Player", values = c( "Blue" = "Blue", "Red" = "Red"), labels = c("Timi", "Isaac")) +
      ggtitle(paste("Progression of Match", input$Match)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(x = "Serve", y = "Score")
    })
    
}

##spaghetti plot Smoothed All Games
# SpagAll <-  ggplot() +
#   geom_line(data = subset(dfPoint, Match == input$Match & is.na(Outcome) == FALSE),
#             aes(ServeIndex, TimiInGameScore, group = Match, colour = "Blue"),
#             stat = "smooth", method = "loess", alpha = .7, se = FALSE, span = 0.5) +
#   geom_line(data = subset(dfPoint, Match == input$Match & is.na(Outcome) == FALSE),
#             aes(ServeIndex, IsaacInGameScore, group = Match, colour = "Red"),
#             stat = "smooth", method = "loess", alpha = .7, se = FALSE, span = 0.5) +
#   scale_colour_manual(name = "Player", values = c( "Blue" = "Blue", "Red" = "Red"), labels = c("Timi", "Isaac"))
# SpagAll

# ggplot() +
#   geom_line(data = dfPointSpaghetti,
#             aes(ServeIndex, InGameScore, colour = Player),
#             stat = "smooth", method = "loess", alpha = .15, se = FALSE, span = 0.2) +
#   geom_line(data = dfPointSpaghetti,
#             aes(ServeIndex, IsaacInGameScore, group = Match, colour = "Red"),
#             stat = "smooth", method = "loess", alpha = .15, se = FALSE, span = 0.2) +
#   geom_line(data = subset(dfPointSpaghetti, Match == input$Match),
#             aes(ServeIndex, TimiInGameScore, group = Match, colour = "Blue"),
#             stat = "smooth", method = "loess", alpha = 0.6, size = 1.5, se = FALSE, span = 0.2) +
#   geom_line(data = subset(dfPointSpaghetti, Match == input$Match),
#             aes(ServeIndex, IsaacInGameScore, group = Match, colour = "Red"),
#             stat = "smooth", method = "loess", alpha = 0.6, size = 1.5, se = FALSE, span = 0.2) +
#   scale_colour_manual(name = "Player", values = c( "Blue" = "Blue", "Red" = "Red"), labels = c("Timi", "Isaac")) +
#   ggtitle(paste("Progression of Match", input$Match)) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   labs(x = "Serve", y = "Score")

#Updated with "Player" doesn't show full background strands
# ggplot() + 
#   geom_line(data = dfPointSpaghetti, 
#             aes(ServeIndex, InGameScore, colour = Player), 
#             stat = "smooth", method = "loess", alpha = .15, se = FALSE, span = 0.2) +
#   geom_line(data = subset(dfPointSpaghetti, Match == input$Match), 
#             aes(ServeIndex, InGameScore, colour = Player), 
#             stat = "smooth", method = "loess", alpha = 0.6, size = 1.5, se = FALSE, span = 0.2) +
#   ggtitle(paste("Progression of Match", input$Match)) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   labs(x = "Serve", y = "Score")

shinyApp(ui = ui, server = server)