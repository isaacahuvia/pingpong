##########################
## Ping Pong Shiny App  ##
##########################

####  Startup  ####
rm(list = ls())
library(shiny)
library(ggplot2)
library(shinyBS)
library(shinythemes)
library(dygraphs)
library(plotly)
library(DT)
library(shinydashboard)

## Prepare dfMatch and dfPoint
#These files load and save the datasets in order
source("1.1 Scrape Data.R")

source("1.2 Clean Scraped Data.R")

source("1.3 Create dfMatch.R")

source("1.4 Create dfPoint.R")

source("1.5 Update dfMatch.R")


## Plot Dataprep
#These files use the most up-to-date dfMatch and dfPoint datasets; they are run wihtout saving the 
#new files, or clearing the working directory, unlike the data prep files
source("2.1 WinPlot Data Prep.R")

source("2.2 ScorePlot Data Prep.R")

source("2.3 PointPlot Data Prep.R")

source("2.4 Highlights Table Data Prep.R")


## QC
source("3.1 QC.R")

####  ui  ####
ui <- tagList(tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
              #tagList used to combine navbarPage (the core app) and HTML at bottom (signature)
              
              navbarPage(theme = shinytheme("flatly"),
                         
                         title = "Office Ping Pong",
                         
                         navbarMenu("Plots & Charts",
                                    
                                    #### ui.Win Plot ####            
                                    tabPanel(title = "Wins Over Time",
                                             fluidRow(
                                               column(width = 6, offset = 2,
                                                      
                                                      HTML('
                                                           
                                                           Navigate between plots using the menu above &#10548;<br><br>
                                                           
                                                           ')
                                                      
                                                      )
                                             ),
                                             fluidRow(
                                               sidebarLayout(
                                                 sidebarPanel(
                                                   #WinPlot x-axis limit slider
                                                   sliderInput(inputId = "WinPlotXAxis", label = "X Axis Range",
                                                               round = TRUE, min = 1, max = nrow(dfMatch), value = c(1, nrow(dfMatch)))
                                                 ),
                                                 
                                                 mainPanel(
                                                   #WinPlot itself         
                                                   plotlyOutput(outputId = "WinPlot")
                                                 )        
                                               )                   
                                             )
                                    ),
                                    
                                    #### ui.Score Plot ####    
                                    tabPanel(title = "Margins of Victory",
                                             
                                             fluidRow(
                                               sidebarLayout( 
                                                 sidebarPanel(
                                                   
                                                   
                                                   #ScorePlot x-axis limit slider
                                                   sliderInput(inputId = "ScorePloxXAxis", label = "X Axis Range",
                                                               round = TRUE, min = min(dfScorePlot$Match[!is.na(dfScorePlot$IsaacFinalScore)]), max = nrow(dfMatch), value = c(min(dfScorePlot$Match[!is.na(dfScorePlot$IsaacFinalScore)]), nrow(dfMatch)))
                                                   
                                                   ,
                                                   
                                                   
                                                   #ScorePlot lagged score inclusion checkboxes
                                                   checkboxGroupInput(inputId = "ScorePlotCheckbox", label = "Metrics",
                                                                      choices = c("5-Game Lag" = "_5",
                                                                                  "10-Game Lag" = "_10",
                                                                                  "Overall" = "_All",
                                                                                  "Dominance Index" = "DI"),
                                                                      selected = c("_5", "_10", "_All"))
                                                   
                                                 ),
                                                 mainPanel(
                                                   #ScorePlot
                                                   plotlyOutput(outputId = "ScorePlot") 
                                                 )
                                               )
                                               
                                             ) 
                                             
                                             
                                             
                                    ),
                                    
                                    #### ui.In-game Plot ####  
                                    tabPanel(title = "Serve by Serve",
                                             
                                             fluidPage(
                                               
                                               sidebarLayout(
                                                 
                                                 sidebarPanel(
                                                   #Chose Match to highlight input
                                                   sliderInput(inputId = "Match", label = "Select Match",
                                                               value = max(dfPointSpaghetti$Match), min = 204, max = max(dfPointSpaghetti$Match), step = 1, round = TRUE),
                                                   
                                                   #Radio Button inputs to select which game to highlight
                                                   selectInput(inputId = "PullUp", label = "Select Highlight Match",
                                                                choices = c("-Select Match-" = "select", 
                                                                            "Longest Game" = "long",
                                                                            "Biggest Comeback" = "comeback",
                                                                            "Biggest Blowout" = "blowout"),
                                                                selected = NULL)
                                                   
                                                 ),
                                                 
                                                 mainPanel(
                                                              fluidRow(plotlyOutput("PlotlyServe"),
                                                                       wellPanel(
                                                                         tableOutput(outputId = "MatchSummary")
                                                                       )
     
                                                     
                                                   )
                                                 )
                                               )
                                             )
                                             
                                    ),
                                    
                                    #### ui.Highlights ####  
                                    tabPanel(title = "Highlights",
                                             
                                             fluidRow(
                                               
                                               column(width = 6, offset = 3,
                                                      
                                                      HTML('
                                                           
                                                           <h3><center>Highlights</h3></center>
                                                           
                                                           ')
                                                      
                                                      )
                                               
                                               ),
                                             
                                             fluidRow(
                                               
                                               column(width = 6, offset = 3,
                                                      
                                                      DT::dataTableOutput("HighlightsTable")
                                                      
                                               )
                                               
                                             )         
                                             
                                               ),
                                    
                                    
                                    #### ui.Experimental 3D Plot ####            
                                    tabPanel(title = "Experimental 3D Plot",
                                             
                                             fluidRow(
                                               plotlyOutput(outputId = "ExperimentalWinPlot")
                                               
                                             )
                                    )
                                             ),
                         
                         #### ui.See Data ####  
                         tabPanel(title = "See Our Data",
                                  
                                  fluidRow(
                                    
                                    column(width = 12,
                                    
                                    HTML('
                                         
                                         Data recorded and stored using Google Forms & Google Sheets<br><br>
                                         
                                         ')
                                    )
                                    
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
                         
                         #### ui.About Us ####
                         tabPanel(title = "About Us",
                                  
                                  fluidRow(
                                    
                                    column(width = 6,
                                           
                                           HTML('
                                                
                                                <div>
                                                <h2> Isaac Ahuvia </h2>
                                                <h4> Co-Founder<br>Chief Technical Director<br>Director of Match-Based Programming </h4>
                                                </div>    
                                                
                                                ')
                                           
                                           ), 
                                    
                                    column(width = 6,
                                           
                                           HTML('
                                                
                                                <div>
                                                <h2> Timi Koyejo </h2>
                                                <h4> Co-Founder<br>Chief Thought Leader<br>Director of Points-Based Programming </h4>
                                                </div>    
                                                
                                                ')
                                           
                                           )
                                    
                                    ),
                                  
                                  fluidRow(
                                    
                                    column(width = 2,
                                           
                                           HTML('
                                                
                                                <div>
                                                <img id="IsaacPortrait" src="IsaacImageBig.jpg" alt = "Spotlight Hogging Coworker" height=100% width=100%>
                                                <script>
                                                $(function() {
                                                $("#IsaacPortrait").hover(
                                                function() {
                                                $(this).attr("src", "IsaacGIF.gif");
                                                },
                                                function() {
                                                $(this).attr("src", "IsaacImageBig.jpg");
                                                }
                                                );
                                                });
                                                </script>
                                                </div>
                                                
                                                ')
                                           
                                           ),
                                    
                                    column(width = 4, 
                                           
                                           HTML("
                                                
                                                <div style='margin-left: 0px'>
                                                <p>
                                                Isaac Ahuvia is an amateur programmer and full-time ping pong athlete. When not playing ping pong, Isaac can be found at his desk pulling policy levers and moving needles. In his free time Isaac collects <a href='https://www.linkedin.com/in/isaac-ahuvia-2b677694/'>rare and unique LinkedIn endoresements</a>.
                                                </p>
                                                <p>
                                                Ping pong skill level: Beat Brice that one time.
                                                </p>
                                                </div>                  
                                                
                                                ")
                                           
                                           ),
                                    
                                    column(width = 2,
                                           
                                           HTML('
                                                
                                                <div>
                                                <img id="TimiPortrait" src="TimiImageBig.jpg" alt = "Spotlight Hogging Coworker" height=100% width=100%>
                                                <script>
                                                $(function() {
                                                $("#TimiPortrait").hover(
                                                function() {
                                                $(this).attr("src", "TimiGIF.gif");
                                                },
                                                function() {
                                                $(this).attr("src", "TimiImageBig.jpg");
                                                }
                                                );
                                                });
                                                </script>
                                                </div>
                                                
                                                ')
                                           
                                           ),
                                    
                                    column(width = 4, 
                                           
                                           HTML('
                                                
                                                <div style="margin-left: 0px">
                                                <p>
                                                Timi Koyejo is a novice data-bender but an expert at running with silly ideas and shepherding them into reality. When he is not enthusiastically promoting the merits of version control, Timi can be found at his desk looking for enlightenment in StackOverflow answers. 
                                                </p>
                                                <p>
                                                Ping pong skill level: Limit is Undefined.
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
              
              #### ui.Footer ####
              
              HTML('
                   
                   <div>
                   <hr></hr>
                   &nbsp &nbsp Version 1.0 Expedited Review
                   </div>
                   
                   '),
              HTML(
                unlist(lapply(unique(dfMatch$Timestamp[dfMatch$QCFlag == TRUE]), paste0, "<br>"))
              ),
              tags$head(tags$link(rel="shortcut icon", href="URL-to-favicon"))
              
              )

####  server  ####
server <- function(input, output) {
  
  #### s.WinPlot ####    
  output$WinPlot <- renderPlotly({
    ##Set Color text color to Black
    t = list(color = toRGB("black"))
    ##Create Blanks spaces for percent win annotation
    blanks <- c("")
    blanks <- str_pad(blanks, width=25, side="right")
    
    plot_ly() %>% 
      #Timi's win line
      add_trace(data = dfWinPlot, x = ~Match, y = ~TimiCumulativeWins,   
                name = paste0("Timi", blanks),
                type = 'scatter', mode = 'lines', hoverinfo = "x + y",
                legendgroup = "Player") %>%
      #Isaac's win line
      add_trace(data = dfWinPlot, x = ~Match, y = ~IsaacCumulativeWins, type = 'scatter', mode = 'lines', 
                name = paste0("Isaac", blanks),
                hoverinfo = "x + y", legendgroup = "Player") %>%
      #Adds all of the vertical largest/current lead lines
      add_lines(data = AllLeads_XY, x = ~Lead_X, y = ~Lead_Y, type = 'scatter', mode = 'lines', hoverinfo = "x", linetype = ~Type, color = I('black'), legendgroup = "Lines") %>%
      #Adds text labels to largest/current lead lines
      add_trace(data = dfTextLead, x = ~Match, y = ~LowValue, mode = 'text', type = 'scatter',textfont = t, hoverinfo = "x", text = ~IsaacLead, textposition = "bottom right", showlegend = F) %>%
      #Adds longest streak line
      add_trace(data = AllStreaks_XY, x = ~Streak_X, y = ~Streak_Y, type = 'scatter', mode = 'lines', linetype = ~Type, color = I('black'), hoverinfo = "x", showlegend = F) %>%
      #Adds text label to longest streak lines
      add_trace(data = dfTextStreak, x = ~Streak_X, y = ~Streak_Y,type = 'scatter', mode = 'text', text = ~Text, textfont = t, hoverinfo = "x", textposition = "top left", showlegend = F) %>%
      # ##Add legend Title
      # add_annotations(data = dfWinPlot, x = 1.2, y = .725, xref = "paper", yref = "paper", showarrow = F, text = paste0("Player (Wins; Pct of ", max(dfWinPlot$Match), ")")) %>%
      # ##Add Timi Win Percentage Annotation 
      # add_annotations(data = dfWinPlot, x = 1.205, y = .63, xref = "paper", yref = "paper", showarrow = F, text = paste0("(", sum(dfWinPlot$Winner == "Timi"), "; ", round(sum(dfWinPlot$Winner == "Timi") * 100 / length(dfWinPlot$Winner), 0), "%)") ) %>%
      # ##Add Isaac Win Percentage Annotation 
      # add_annotations(data = dfWinPlot, x = 1.207, y = .574, xref = "paper", yref = "paper", showarrow = F, text = paste0("(", sum(dfWinPlot$Winner == "Isaac"), "; ", round(sum(dfWinPlot$Winner == "Isaac") * 100 / length(dfWinPlot$Winner), 0), "%)") ) %>%
      layout(
        #Titles graph
        title = "Matches Won Over Time",
        #Label x axis
        xaxis = list(           
          title = "Match",
          range = c(if(input$WinPlotXAxis[1] == 1) {0} else {input$WinPlotXAxis[1]}, input$WinPlotXAxis[2] + 5)
        ),
        #Label y axis
        yaxis = list(
          title = "Number of Matches Won",
          ##  NOTE  making the y limit reactive costs a significant amount of load-time; may want to get rid of that if we're loading too slowly
          range = c(max(min(dfWinPlot$IsaacCumuativeWins[input$WinPlotXAxis[1]], dfWinPlot$TimiCumulativeWins[input$WinPlotXAxis[1]]) - 5, 0), 
                    max(dfWinPlot$IsaacCumulativeWins[input$WinPlotXAxis[2]], dfWinPlot$TimiCumulativeWins[input$WinPlotXAxis[2]]) + 5),
          hovermode = "closest"
        ),
        #Position Legend outside plot
        legend = list(x = 100, y = 0.5)
      ) 
    
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
  
  #### s.Score Plot ####  
  output$ScorePlot <- renderPlotly({
    
    ScorePlot <- plot_ly(dfScorePlot, type = 'scatter', mode = 'lines')  %>%
      # add_trace(x = c(min(dfScorePlot$Match), max(dfScorePlot$Match)), y = c(.50, .50)) %>%
      layout(
        title = "Points Won Over Time",
        #Label y axis
        yaxis = list(
          title = "Percentage of Points Won (Isaac)", hoverformat = '.3r', range = c(.40,.60)
        ),
        xaxis = list(
          range = c(input$ScorePloxXAxis[1], input$ScorePloxXAxis[2])
        ), 
        #Position Legend outside plot
        legend = list(x = 100, y = 0.5),
        title = "Points Won Over Time",
        shapes = list(
          type = "line",
          fillcolor = "grey",
          opacity = 0.3,
          x0 = min(dfScorePlot$Match), x1 = max(dfScorePlot$Match),
          y0 = .50, y1 = .50
        )
      )
    
    
    if("_5" %in% input$ScorePlotCheckbox) {
      
      ScorePlot <- ScorePlot %>% add_trace(ScorePlot, x = ~Match, y = ~ScoreDiff5, type = 'scatter', mode = 'lines', name = '5-Match Lag') %>%
        layout(
          #Label y axis
          yaxis = list(
            title = "Percentage of Points Won (Isaac)", hoverformat = '.3r', range = c(.40,.60)
          ),
          xaxis = list(
            range = c(input$ScorePloxXAxis[1], input$ScorePloxXAxis[2])
          )
        )
      
      
    }
    
    if("_10" %in% input$ScorePlotCheckbox) {
      
      ScorePlot <- ScorePlot %>% add_trace(ScorePlot, x = ~Match, y = ~ScoreDiff10, name = '10-Match Lag') %>%
        layout(
          #Label y axis
          yaxis = list(
            title = "Percentage of Points Won (Isaac)", hoverformat = '.3r', range = c(.40,.60)
          ),
          xaxis = list(
            range = c(input$ScorePloxXAxis[1], input$ScorePloxXAxis[2])
          )
        )
      
    }
    
    if("_All" %in% input$ScorePlotCheckbox) {
      
      ScorePlot <- add_trace(ScorePlot, x = ~Match, y = ~ScoreDiffAll, name = 'Overall') %>%
        layout(
          #Label y axis
          yaxis = list(
            title = "Percentage of Points Won (Isaac)", hoverformat = '.3r', range = c(.40,.60)
          ),
          xaxis = list(
            range = c(input$ScorePloxXAxis[1], input$ScorePloxXAxis[2])
          )
        )
      
    }
    
    if("DI" %in% input$ScorePlotCheckbox) {
      
      ScorePlot <- add_trace(ScorePlot, x = ~Match, y = ~DIIsaac, name = 'Dominance Index') %>%
        layout(
          #Label y axis
          yaxis = list(
            title = "Percentage of Points Won (Isaac)", hoverformat = '.3r', range = c(.40,.60)
          ),
          xaxis = list(
            range = c(input$ScorePloxXAxis[1], input$ScorePloxXAxis[2])
          )
        )
      
      
    }
    
    ScorePlot 
    
  })
  
  #### s.Plotly Serve By Serve ####  
  output$PlotlyServe <- renderPlotly({
    
    ShowMatch <- 0
    
    if(input$PullUp == "select") {
      
      ShowMatch <- input$Match
      
    }else if(input$PullUp == "long") {
      
      ShowMatch <- dfPointSpaghetti$Match[which.max(dfPointSpaghetti$WinningScore)]
      
    }else if(input$PullUp == "comeback") {
      
      dfPointSpaghetti = dplyr::mutate(dfPointSpaghetti, ComebackSize = pmax(IsaacComebackSize, TimiComebackSize, na.rm = T))
      ShowMatch <- dfPointSpaghetti$Match[which.max(dfPointSpaghetti$ComebackSize)]
      
    }else if(input$PullUp == "blowout") {
      
      dfPointSpaghetti = dplyr::mutate(dfPointSpaghetti, Margin = WinningScore - LosingScore)
      ShowMatch <- dfPointSpaghetti$Match[which.max(dfPointSpaghetti$Margin)]
      
    }
    
    Spag <- plot_ly(data = subset(dfPointSpaghetti, Match == ShowMatch),
                    x = ~ServeIndex, y = ~TimiInGameScore, name = 'Timi',
                    type = 'scatter', mode = 'lines') %>%
      add_trace(y = ~IsaacInGameScore, name = 'Isaac')  %>%
      layout(title = "Score by Serve",
             yaxis = list(title = "Score"),
             xaxis = list(title = "Serve Number"))
    Spag  
    
  })
  
  #### s.Serve By Serve Match Summary Table ####  
  output$MatchSummary <- renderTable({
    
    ShowMatch <- 0
    
    if(input$PullUp == "select") {
      
      ShowMatch <- input$Match
      
    }else if(input$PullUp == "long") {
      
      ShowMatch <- dfPointSpaghetti$Match[which.max(dfPointSpaghetti$WinningScore)]
      
    }else if(input$PullUp == "comeback") {
      
      dfPointSpaghetti = dplyr::mutate(dfPointSpaghetti, ComebackSize = pmax(IsaacComebackSize, TimiComebackSize, na.rm = T))
      ShowMatch <- dfPointSpaghetti$Match[which.max(dfPointSpaghetti$ComebackSize)]
      
    }else if(input$PullUp == "blowout") {
      
      dfPointSpaghetti = dplyr::mutate(dfPointSpaghetti, Margin = WinningScore - LosingScore)
      ShowMatch <- dfPointSpaghetti$Match[which.max(dfPointSpaghetti$Margin)]
      
    }
    
    dfMatchSummary <- dfPointSpaghetti %>%
      dplyr::group_by(Match) %>%
      dplyr::filter(ServeIndex == 30) %>%
      dplyr::filter(Match == ShowMatch) %>%
      dplyr::select(TimiServeUnreturned, IsaacServeUnreturned, TimiServeError, IsaacServeError)
    names(dfMatchSummary) <- c("Match", "Timi: Unreturned Serves", "Isaac: Unreturned Serves", 
                               "Timi: Service Errors", "Isaac: Service Errors")
    dfMatchSummary                      
  })
  

  #### s.See Data ####    
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
  
  #### s.Highlights ####
  output$HighlightsTable <- DT::renderDataTable({
    
    DT::datatable(dfHighlights, 
                  escape = FALSE,
                  rownames = FALSE,
                  options = list(columnDefs = list(list(className = "dt-center", targets = 0:4)),
                                 paging = FALSE,
                                 bLengthChange = FALSE,
                                 bFilter = FALSE,
                                 bInfo = FALSE))
    
  })
  
  #### s.Experimental 3D Plot ####
  output$ExperimentalWinPlot <- renderPlotly({
    
    plot_ly(data = dfMatch, x = ~Match, y = ~TimiCumulativeWins, z = ~TimiFinalScore, type = "scatter3d", name = "Timi") %>%
      add_trace(data = dfMatch, x = ~Match, y = ~IsaacCumulativeWins, z = ~IsaacFinalScore, type = "scatter3d", name = "Isaac") %>%
      layout(scene = list(xaxis = list(title = "Match"),
                          yaxis = list(title = "Cumulative Wins"),
                          zaxis = list(title = "Final Score")))
    
  })
  
}

####  App  ####
shinyApp(ui = ui, server = server)