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

source("dfPoint.R")

source("Create Raw Score Variables.R")

source("Point Data Quality Control.R")

## Plot Dataprep
#These files use the most up-to-date dfMatch and dfPoint datasets; they are run wihtout saving the 
#new files, or clearing the working directory, unlike the data prep files
source("WinPlot Data Prep.R")

source("ScorePlot Data Prep.R")

source("Point Plot Data Prep.R")


####  ui  ####
ui <- tagList(tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
 #tagList used to combine navbarPage (the core app) and HTML at bottom (signature)

navbarPage(theme = "bootstrap.css",
  
  title = "Office Ping Pong",
  
  navbarMenu("Plots & Charts",
             
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
    
    tabPanel(title = "Margins of Victory",
           
      fluidRow(
      
        column(width = 3,
        
          #ScorePlot x-axis limit slider
          sliderInput(inputId = "ScorePloxXAxis", label = "Score Plot X Axis",
          round = TRUE, min = min(dfScorePlot$Match[!is.na(dfScorePlot$IsaacFinalScore)]), max = nrow(dfMatch), value = c(min(dfScorePlot$Match[!is.na(dfScorePlot$IsaacFinalScore)]), nrow(dfMatch)))
        
        ),
      
        column(width = 2,
        
          #ScorePlot lagged score inclusion checkboxes
          checkboxGroupInput(inputId = "ScorePlotCheckbox", label = "Metrics",
                             choices = c("5-Game Lag" = "_5",
                                         "10-Game Lag" = "_10",
                                         "Overall" = "_All",
                                         "Dominance Index" = "DI"),
                             selected = c("_5", "_10", "_All"))
        
        )
      
      ),
    
      #ScorePlot
      plotOutput(outputId = "ScorePlot",
                 hover = hoverOpts(id = "ScorePlotHover", delay = 100, delayType = "debounce")),

      #ScorePlot slider
      uiOutput(outputId = "ScorePlotHoverInfo")

    ),
  
    tabPanel(title = "Serve by Serve",
             
      fluidPage(
        
        sliderInput(inputId = "Match", label = "Choose Match",
                    value = 204, min = 204, max = max(dfPointSpaghetti$Match), step = 1, round = TRUE),
        plotOutput("HighlightMatch")
      )
      
    ),
  
  
    tabPanel(title = "Highlights",
             
      fluidRow(
        
        print("Big chart here; left column Timi (w emoji) middle column rowname right column Isaac (w emoji); emoji just for those with higher #?")
        
      )         
             
    )
  
  ),
  
  tabPanel(title = "See Our Data",
           
  fluidRow(
    
    HTML('
         
blurb on google sheets data collection/storage process <br>
         
         ')
    
  ),
  
  fluidRow(
    
    column(width = 2,
           
           #Checkbox inputs to filter/sort data
           radioButtons(inputId = "Order", label = "Sort Data",
                        choices = c("Newest First" = "new",
                                    "Oldest First" = "old"),
                        selected = "new")
           
           ),
    
    column(width = 2,
           
           #Checkbox inputs to select only most important variables
           radioButtons(inputId = "Select", label = "Select Variables",
                        choices = c("Key Variables" = "some",
                                    "All Variables" = "all"),
                        selected = "some")
           
           )
    
  ),
           
  dataTableOutput(outputId = "DataTable")
         
  ),
  
  tabPanel(title = "About Us",
           
    fluidRow(
      
      column(width = 12,
        
        HTML('
             
<div>
  <h2> Isaac Ahuvia </h2>
  <h4> Co-Founder, Chief Technical Director, and Director of Match-Based Programming </h4>
</div>    
             
             ')
        
      )
      
    ),
    
    fluidRow(
      
      column(width = 1,
             
             HTML('
                  
<div>
  <img id="testgifI" src="IsaacImage.jpg" alt = "Spotlight Hogging Coworker" height=120 width=120>
  <script>
    $(function() {
      $("#testgifI").hover(
        function() {
          $(this).attr("src", "test_gif.gif");
        },
        function() {
          $(this).attr("src", "IsaacImage.jpg");
        }
      );
    });
  </script>
</div>
                  
                  ')
             
        ),
      
      column(width = 8, 
             
             HTML("
                  
<div style='margin-left: 20px'>
  <p>(Isaac gif: juggling off of side of paddle - either find a way to stop gif after one rep or make gif with a super long final frame)</p>
  <p>
  Isaac Ahuvia is a __________________. He enjoys playing as hard as he works, etc. An <strike>expert</strike> <strike>accomplished</strike> capable programmer, he once commented out 200 lines of code in 30 minutes flat. When not playing ping pong, Isaac can be found at his desk pulling policy levers and moving needles. In his free time Isaac collects <a href='https://www.linkedin.com/in/isaac-ahuvia-2b677694/'>rare and unique LinkedIn endoresements</a>.
  </p>
  <p>
  Ping pong skill level: Beat Brice that one time.
  </p>
</div>                  

                  ")
             
             )
      
    ),
    
    fluidRow(
      
      column(width = 12,
             
             HTML('
                  
<div>
  <h2> Timi Koyejo </h2>
  <h4> Co-Founder, Chief Thought Leader, and Director of Points-Based Programming </h4>
</div>    
                  
                  ')
             
             )
      
      ),
    
    fluidRow(
      
      column(width = 1,
             
             HTML('
                  
<div>
  <img id="testgifT" src="TimiImage.jpg" alt = "Spotlight Hogging Coworker" height=120 width=120>
  <script>
    $(function() {
      $("#testgifT").hover(
        function() {
          $(this).attr("src", "test_gif.gif");
        },
        function() {
          $(this).attr("src", "TimiImage.jpg");
        }
      );
    });
  </script>
</div>
                  
                  ')
             
             ),
      
      column(width = 8, 
             
             HTML('
                  
<div style="margin-left: 20px">
  <p>
  bio (Timi gif: smash (?))  
  </p>
  <p>
  Lorem ipsum dolor sit amet, consectetur adipiscing elit. Proin varius dictum nibh, sit amet ornare dolor ullamcorper eu. Aliquam imperdiet purus at turpis tristique consequat. Maecenas sagittis, sem et eleifend sodales, orci sapien rhoncus dui, eget hendrerit odio velit eget massa. Nullam efficitur sem ut commodo dictum. Nulla ut cursus justo. Quisque et lacus eu tellus finibus sollicitudin vel sed nisl. Cras laoreet ante sit amet tellus iaculis convallis. 
  <br>
  Pellentesque non hendrerit sem. Proin suscipit magna vitae enim gravida congue. Nam molestie ullamcorper elit vitae pharetra. Nam ut auctor purus. Pellentesque consectetur lorem et urna auctor volutpat. Sed quis odio in turpis consectetur sollicitudin. Nulla vestibulum mauris orci, in eleifend urna gravida id. Etiam placerat orci sit amet justo egestas consectetur. Pellentesque ullamcorper rhoncus libero non varius.
  </p>
</div>                  
                  
                  ')
             
             )
      
      ),
    
    fluidRow(
      
      column(width=12,
        
        HTML('

<div>
<br>
Curious about our code? <a href="https://github.com/tkoyejo/Really-Important-Stuff">Check out our github!</a>
</div>

        ')
      
      )
           
    )
  
  )
  
),

#Footer - make sticky to bottom?

HTML('

     <div>
     <hr></hr>
     &nbsp &nbsp Version 0.9.1 Consent Waived
     </div>
     
     '),

tags$head(tags$link(rel="shortcut icon", href="URL-to-favicon"))

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
                    "<b>Isaac: </b>", pointWin$IsaacFinalScore, "<br/>",
                    "<b>Timi: </b>", pointWin$TimiFinalScore, "<br/>")))
      
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
    
    if("DI" %in% input$ScorePlotCheckbox) {
      
      ScorePlot <- ScorePlot + geom_line(aes(Match, DIIsaac, color = "Dominance Index"), size = 1)
      
    }
    
    ScorePlot <- ScorePlot + labs(color = "Color")
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
      p(HTML(paste0("<b>Match: </b>", pointScore$Match, "<br/>",
                    "<b>Isaac: </b>", pointScore$IsaacFinalScore, "<br/>",
                    "<b>Timi: </b>", pointScore$TimiFinalScore, "<br/>")))
      
    )
    
  })
  
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
  
  output$DataTable <- renderDataTable({
    
    dfOutput <- dfMatch
    
    if(input$Select == "some") {
      
      dfOutput$Leader[dfOutput$IsaacLead > 0] <- "Isaac"
      dfOutput$Leader[dfOutput$TimiLead > 0] <- "Timi"
      dfOutput$Lead <- paste0(abs(dfOutput$IsaacLead), " (", dfOutput$Leader, ")")
      
      dfOutput$Streak[dfOutput$Winner == "Isaac"] <- paste0(dfOutput$IsaacStreak[dfOutput$Winner == "Isaac"], " (Isaac)")
      dfOutput$Streak[dfOutput$Winner == "Timi"] <- paste0(dfOutput$TimiStreak[dfOutput$Winner == "Timi"], " (Timi)")
      
      dfOutput <- dplyr::select(dfOutput,
                                Match, Timestamp, WonRally, NorthEndzone, Winner, WinningScore, LosingScore, IsaacCumulativeWins, TimiCumulativeWins, Lead, Streak)
      names(dfOutput) <- c("Match", "Timestamp", "Won Rally", "North Endzone", "Winner", "Winning Score", "Losing Score", "Isaac Total Wins", "Timi Total Wins", "Lead", "Streak")

    }
    
    if(input$Order == "new") {
      
      dfOutput <- dplyr::arrange(dfOutput, desc(Match))
      
    }
    
    dfOutput
    
  })
  
}

####  App  ####
shinyApp(ui = ui, server = server)