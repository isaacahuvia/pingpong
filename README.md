# Ping Pong Reporting

Steps:
- [x] Create Google form/sheet to hold data - __*Isaac*__
- [ ] Automatically pull data from site and prepare for analysis - __*Isaac*__
  - [x] Scraping.R - __*Isaac*__
  - [x] Data Cleaning.R - __*Isaac*__
  - [x] Data Manipulation.R - __*Isaac*__
  - [ ] Create Point Dataset.R - __*Timi*__ (loads dfMatch, creates and saves dfPoint)
    - [ ] May want separate .R file for QC stuff - __*Timi*__
      - Use final score variables to fix errors in point-by-point data, as needed
  - [ ] Update Match Dataset.R - __*Timi*__ (with new match-level variables that you need dfPoint for, like comback)
- [ ] Create code for analyses - __*Both*__
  - [x] Game-based metrics - __*Isaac*__
    - [x] WinPlot.R - __*Isaac*__
      - [ ] Add mouse-over text for score of game, perhaps also total wins at time of game and current lead
    - [x] ScorePlot.R - __*Isaac*__
      - [ ] Add mouse-over
    - [x] DominanceIndexPlot.R - __*Isaac*__
      - [ ] Add mouse-over
  - [ ] Point-based metrics - __*Timi*__
    - [ ] TBD - __*Timi*__
- [ ] Turn code into reactive RShiny objects - __*Both*__
  - [x] Game-based metrics - __*Isaac*__
    - [x] WinPlot.R - __*Isaac*__
    - [x] ScorePlot.R - __*Isaac*__
    - [x] DominanceIndexPlot.R - __*Isaac*__
  - [ ] Point-based metrics - __*Timi*__
    - [ ] TBD - __*Timi*__
- [x] Publish app with shinyapps.io - __*Isaac*__
- [x] Combine all RShiny code into single page/domain - __*Isaac*__
  - [ ] Reformat pages

Tings to fix:
- [ ] Colors of lines in score plot change when boxes are checked/unchecked
- [ ] X-axis scale labels overlap (also, set to only show multiples of 20/40?)


A silo-less thought-partnership by:

Co-Founder, Chief Technical Director, and Director of Match-Based Programming Isaac Ahuvia

Co-Founder, Chief Thought Leader, and Director of Points-Based Programming Timi Koyejo
