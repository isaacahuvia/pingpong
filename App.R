##########################
## Ping Pong Shiny App  ##
##########################

####  Startup  ####
rm(list = ls())
library(shiny)
library(ggplot2)
library(shinyBS)

## Prepare dfMatch and dfPoint
#These files load and save the datasets in order
source("Scraping.R")

source("Data Cleaning.R")

source("Data Manipulation.R")

# source("dfPoint.R")

#source("Create dfPoint.R")

#source("Update dfMatch.R")

## Plot Dataprep
#These files use the most up-to-date dfMatch and dfPoint datasets; they are run wihtout saving the 
#new files, or clearing the working directory, unlike the data prep files
source("WinPlot Data Prep.R")

source("ScorePlot Data Prep.R")

source("DIPlot Data Prep.R")

####  ui  ####
ui <- tagList( #tagList used to combine navbarPage (the core app) and HTML at bottom (signature)

navbarPage(
  
  title = "Office Ping Pong",
  
  tabPanel(title = "Wins Over Time",
  
    #WinPlot x-axis limit slider
    sliderInput(inputId = "WinPlotXAxis", label = "Win Plot X Axis",
                round = TRUE, min = 1, max = nrow(dfMatch), value = c(1, nrow(dfMatch))),
    
    #WinPlot itself         
    plotOutput(outputId = "WinPlot",
               hover = hoverOpts(id = "WinPlotHover", delay = 100, delayType = "debounce")),
    
    #WinPlot slider
    uiOutput(outputId = "WinPlotHoverInfo")
      
  ),
    
  tabPanel(title = "Final Scores Over Time",
    
    #ScorePlot x-axis limit slider
    sliderInput(inputId = "ScorePloxXAxis", label = "Score Plot X Axis",
      round = TRUE, min = min(dfScorePlot$Match[!is.na(dfScorePlot$IsaacFinalScore)]), max = nrow(dfMatch), value = c(min(dfScorePlot$Match[!is.na(dfScorePlot$IsaacFinalScore)]), nrow(dfMatch))),
    
    #ScorePlot lagged score inclusion checkboxes
    checkboxGroupInput(inputId = "ScorePlotCheckbox", label = "Metrics",
                       choices = c("5-Game Lag" = "_5",
                                   "10-Game Lag" = "_10",
                                   "Overall" = "_All"),
                       selected = c("_5", "_10", "_All")),
    
    #ScorePlot
    plotOutput(outputId = "ScorePlot",
               hover = hoverOpts(id = "ScorePlotHover", delay = 100, delayType = "debounce")),

    #ScorePlot slider
    uiOutput(outputId = "ScorePlotHoverInfo")
    
  ),
  
  tabPanel(title = "Dominance Index",
           
    #DIPlot x-axis limit slider     
    sliderInput(inputId = "DIPlotXAxis", label = "DI Plot X Axis",
                round = TRUE, min = min(dfDIPlot$Match[!is.na(dfDIPlot$DIIsaac)]), max = nrow(dfMatch), value = c(min(dfDIPlot$Match[!is.na(dfDIPlot$DIIsaac)]), nrow(dfMatch))),
    
    #DIPlot
    plotOutput(outputId = "DIPlot",
               hover = hoverOpts(id = "DIPlotHover", delay = 100, delayType = "debounce")),
    
    #DIPlot slider
    uiOutput(outputId = "DIPlotHoverInfo")
    
  ),
  
  tabPanel(title = "Data",
           
  #Checkbox inputs to filter/sort data
  radioButtons(inputId = "Order", label = "Order Data",
                     choices = c("Newest First" = "new",
                                 "Oldest First" = "old"),
                     selected = "new"),
  
  dataTableOutput(outputId = "DataTable")
         
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
     &nbsp &nbsp Version 0.91 Consent Waived
     </div>
     
     ')

)

####  server  ####
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
  
  output$WinPlotHoverInfo <- renderUI({
    
    hoverWin <- input$WinPlotHover
    pointWin <- nearPoints(df = dfWinPlot, coordinfo = hoverWin, threshold = 500, maxpoints = 1)
    
    if(nrow(pointWin) == 0) {return(NULL)}
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pctWin <- (hoverWin$x - hoverWin$domain$left) / (hoverWin$domain$right - hoverWin$domain$left)
    top_pctWin <- (hoverWin$domain$top - hoverWin$y) / (hoverWin$domain$top - hoverWin$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_pxWin <- hoverWin$range$left + left_pctWin * (hoverWin$range$right - hoverWin$range$left)
    top_pxWin <- hoverWin$range$top + top_pctWin * (hoverWin$range$bottom - hoverWin$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    styleWin <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_pxWin + 2, "px; top:", top_pxWin + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      
      style = styleWin,
      p(HTML(paste0("<b> Match: </b>", pointWin$Match, "<br/>",
                    "<b> Isaac: </b>", pointWin$IsaacFinalScore, "<br/>",
                    "<b> Timi: </b>", pointWin$TimiFinalScore, "<br/>")))
    
      )
    
  })
  
  output$ScorePlot <- renderPlot({
    
    ScorePlot <- ggplot(data = dfScorePlot) +
      xlim(input$ScorePloxXAxis[1], input$ScorePloxXAxis[2]) +
      ylim(.4,.6) +
      geom_abline(slope = 0, intercept = .5, alpha = .35) +
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
  
  output$ScorePlotHoverInfo <- renderUI({
    
    hoverScore <- input$ScorePlotHover
    pointScore <- nearPoints(df = dfScorePlot, coordinfo = hoverScore, threshold = 500, maxpoints = 1)
    
    if(nrow(pointScore) == 0) {return(NULL)}
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pctScore <- (hoverScore$x - hoverScore$domain$left) / (hoverScore$domain$right - hoverScore$domain$left)
    top_pctScore <- (hoverScore$domain$top - hoverScore$y) / (hoverScore$domain$top - hoverScore$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_pxScore <- hoverScore$range$left + left_pctScore * (hoverScore$range$right - hoverScore$range$left)
    top_pxScore <- hoverScore$range$top + top_pctScore * (hoverScore$range$bottom - hoverScore$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    styleScore <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                       "left:", left_pxScore + 2, "px; top:", top_pxScore + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      
      style = styleScore,
      p(HTML(paste0("<b> Match: </b>", pointScore$Match, "<br/>",
                    "<b> Isaac: </b>", pointScore$IsaacFinalScore, "<br/>",
                    "<b> Timi: </b>", pointScore$TimiFinalScore, "<br/>")))
      
    )
    
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
  
  output$DIPlotHoverInfo <- renderUI({
    
    hoverDI <- input$DIPlotHover
    pointDI <- nearPoints(df = dfDIPlot, coordinfo = hoverDI, threshold = 500, maxpoints = 1)
    
    if(nrow(pointDI) == 0) {return(NULL)}
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pctDI <- (hoverDI$x - hoverDI$domain$left) / (hoverDI$domain$right - hoverDI$domain$left)
    top_pctDI <- (hoverDI$domain$top - hoverDI$y) / (hoverDI$domain$top - hoverDI$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_pxDI <- hoverDI$range$left + left_pctDI * (hoverDI$range$right - hoverDI$range$left)
    top_pxDI <- hoverDI$range$top + top_pctDI * (hoverDI$range$bottom - hoverDI$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    styleDI <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                         "left:", left_pxDI + 2, "px; top:", top_pxDI + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      
      style = styleDI,
      p(HTML(paste0("<b> Match: </b>", pointDI$Match, "<br/>",
                    "<b> Isaac: </b>", pointDI$IsaacFinalScore, "<br/>",
                    "<b> Timi: </b>", pointDI$TimiFinalScore, "<br/>")))
      
    )
    
  })
  
  output$DataTable <- renderDataTable({
    
    dfOutput <- dplyr::select(dfMatch,
                              Match, Timestamp, WonRally, NorthEndzone, Winner, WinningScore, LosingScore, IsaacCumulativeWins, TimiCumulativeWins, IsaacLead, TimiLead, IsaacStreak, TimiStreak)
    
    if(input$Order == "new") {
      
      dfOutput <- dplyr::arrange(dfOutput, desc(Match))
      
    }
    
    dfOutput
    
  })
  
}

####  App  ####
shinyApp(ui = ui, server = server)
