########### Shiny
rm(list = ls())
library(shiny)
library(ggplot2)
library(shinyBS)


###  Prepare dfMatch and dfPoint  ###
#These files load and save the datasets in order
source("Scraping.R")

source("Data Cleaning.R")

source("Data Manipulation.R")

#source("Create dfPoint.R")

#source("Update dfMatch.R")


###  Plot Dataprep  ###
#These files use the most up-to-date dfMatch and dfPoint datasets; they are run wihtout saving the 
#new files, or clearing the working directory, unlike the data prep files
source("WinPlot Data Prep.R")

source("ScorePlot Data Prep.R")

source("DIPlot Data Prep.R")



ui <- tagList(

navbarPage(
  
  title = "Office Ping Pong",
  
  tabPanel(title = "Win Plot",
  
    #WinPlot x-axis limit slider
    sliderInput(             

      inputId = "WinPlotXAxis", label = "Win Plot X Axis",
      round = TRUE, min = 1, max = nrow(dfMatch), value = c(1, nrow(dfMatch))
      
      ),
             
    plotOutput(outputId = "WinPlot")
      
  ),
    
  tabPanel(title = "Score Plot",
    
    #ScorePlot x-axis limit slider
    sliderInput(
      
      inputId = "ScorePloxXAxis", label = "Score Plot X Axis",
      round = TRUE, min = min(dfScorePlot$Match[!is.na(dfScorePlot$IsaacScore)]), max = nrow(dfMatch), value = c(min(dfScorePlot$Match[!is.na(dfScorePlot$IsaacScore)]), nrow(dfMatch))
      
    ),
    
    #ScorePlot lagged score inclusion checkboxes
    checkboxGroupInput(
      
      inputId = "ScorePlotCheckbox", label = "Metrics", 
      choices = c("5-Game Lag" = "_5",
                  "10-Game Lag" = "_10",
                  "Overall" = "_All"),
      selected = c("_5", "_10", "_All")
      
    ),
    
    plotOutput(outputId = "ScorePlot")

  ),
  
  tabPanel(title = "Dominance Index",
           
    #DIPlot x-axis limit slider     
    sliderInput(
      
      inputId = "DIPlotXAxis", label = "DI Plot X Axis",
      round = TRUE, min = min(dfDIPlot$Match[!is.na(dfDIPlot$DIIsaac)]), max = nrow(dfMatch), value = c(min(dfDIPlot$Match[!is.na(dfDIPlot$DIIsaac)]), nrow(dfMatch))
      
      ),
    
    plotOutput(outputId = "DIPlot")
    
  )
  
),

#Signature footer - make sticky to bottom?

HTML('

     <div>
     <hr></hr>
     &nbsp &nbsp Co-Founder, Chief Technical Director, and Director of Match-Based Programming Isaac Ahuvia
     <br>
     &nbsp &nbsp Co-Founder, Chief Thought Leader, and Director of Points-Based Programming Timi Koyejo
     <br>
     <br>
     &nbsp &nbsp Version 0.9 Consent Waived
     </div>
     
     ')

)



server <- function(input, output) {
  
  output$WinPlot <- renderPlot({
    
    ggplot() +
      #Timi's win line
      geom_line(data = dfWinPlot, aes(Match, TimiCumulativeWins, color = "Blue")) +
      #Isaac's win line
      geom_line(data = dfWinPlot, aes(Match, IsaacCumulativeWins, color = "Red")) +
      #Adds all of the vertical largest/current lead lines
      geom_line(data = AllLeads_XY, aes(Lead_X, Lead_Y, linetype = Type)) +
      #Adds text labels to largest/current lead lines
      geom_text(data = dfTextLead, aes(Match, LowValue, label = IsaacLead), hjust = 0, vjust = 1) +
      #Adds longest streak line
      geom_line(data = AllStreaks_XY, aes(Streak_X, Streak_Y, linetype = Type)) +
      #Adds text label to longest streak liness
      geom_text(data = dfTextStreak, aes(Streak_X, Streak_Y, label = Text), hjust = 0, vjust = -1) +
      #Adds legend on side describing total wins
      scale_colour_discrete(name = paste0("Player (Wins; Pct of ", max(dfWinPlot$Match), ")"), labels = c(paste0("Timi (", sum(dfWinPlot$Winner == "Timi"), "; ", round(sum(dfWinPlot$Winner == "Timi") * 100 / length(dfWinPlot$Winner), 0), "%)"), paste0("Isaac (", sum(dfWinPlot$Winner == "Isaac"), "; ", round(sum(dfWinPlot$Winner == "Isaac") * 100 / length(dfWinPlot$Winner), 0), "%)"))) +
      #Labels largest lead legend
      scale_linetype_discrete(name = "Largest Lead/Streak") +
      #Labels y axis
      ylab("Matches Won") +
      #Titles graph
      labs(title = "Matches Won") +
      #Sets axis limits
      ##  NOTE  making the y limit reactive costs a significant amount of load-time; may want to get rid of that if we're loading too slowly
      xlim(if(input$WinPlotXAxis[1] == 1) {0} else {input$WinPlotXAxis[1]}, input$WinPlotXAxis[2]) + 
      ylim(max(min(dfWinPlot$IsaacCumuativeWins[input$WinPlotXAxis[1]], dfWinPlot$TimiCumulativeWins[input$WinPlotXAxis[1]]) - 5, 0),
           max(dfWinPlot$IsaacCumulativeWins[input$WinPlotXAxis[2]], dfWinPlot$TimiCumulativeWins[input$WinPlotXAxis[2]]) + 5)
    
  })
  
  output$DIPlot <- renderPlot({
    
    dfDIPlot %>%
      dplyr::filter(!is.na(DIIsaac)) %>%
      ggplot(aes(Match, DIIsaac)) +
      geom_line(color = "firebrick") +
      geom_abline(slope = 0, intercept = 0, linetype = "dashed") +
      xlim(input$DIPlotXAxis[1], input$DIPlotXAxis[2]) + 
      ylim(-25,25) +
      ylab("Dominance (Isaac)") +
      labs(title = "Dominance Index")
    
  })
  
  output$ScorePlot <- renderPlot({
    
    ScorePlot <- ggplot(data = dfScorePlot) +
      xlim(input$ScorePloxXAxis[1], input$ScorePloxXAxis[2]) + 
      ylim(.4,.6) +
      labs(title = "Points Won Over Time") +
      ylab("Points Won (Isaac)")
    
    if("_5" %in% input$ScorePlotCheckbox) {
      
      ScorePlot <- ScorePlot + geom_line(aes(Match, ScoreDiff5, color = "5-Match Lag"))
      
    }
    
    if("_10" %in% input$ScorePlotCheckbox) {
      
      ScorePlot <- ScorePlot + geom_line(aes(Match, ScoreDiff10, color = "10-Match Lag"))
      
    }
    
    if("_All" %in% input$ScorePlotCheckbox) {
      
      ScorePlot <- ScorePlot + geom_line(aes(Match, ScoreDiffAll, color = "Overall"))
      
    }
    
    ScorePlot
    
  })
  
}

shinyApp(ui = ui, server = server)
