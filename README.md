# Ping Pong Reporting

To-do's below 
✓ = Done
- = To-do
^ = Optional additions

Steps:
✓ Create Google form/sheet to hold data
✓ Automatically pull data from site and prepare for analysis
  ✓ Scraping.R
  ✓ Data Cleaning.R
  ✓ Data Manipulation.R
- Create code for analyses
  _Game-based metrics_
    ✓ WinPlot.R
      ^Make streak code flexible - perhaps analogous to largest lead, in that both players get one and it shows the most recent of each
      ^Add mouse-over text for score of game, perhaps also total wins at time of game and current lead
    ✓ ScorePlot.R
    ✓ DominanceIndexPlot.R
  _Point-based metrics_
    - TBD
- Turn code into reactive RShiny objects
  _Game-based metrics_
    ✓ WinPlot.R
    - ScorePlot.R
    - DominanceIndexPlot.R
  _Point-based metrics_
    - TBD
- Port code onto external server/site
- Combine all RShiny code into single page/domain
