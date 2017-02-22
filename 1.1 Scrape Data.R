#######################
##  1.1 Scrape Data  ##
#######################

rm(list = ls())
library(rvest); library(stringr); library(tidyr); library(selectr); library(xml2)

#repeat scraping process for pre-form url, form response url
urlPre <- "https://docs.google.com/spreadsheets/d/1CjIj8OHv7_cLRkciM24nQ94HydlW4pls0cGnxhFSN3s/gviz/tq?tqx=out:html&tq&gid=1714148721"
webpagePre <- read_html(urlPre)
tablesPre <- html_nodes(webpagePre, "table")
dfPre <- html_table(tablesPre)[[1]]

urlRes <- "https://docs.google.com/spreadsheets/d/1CjIj8OHv7_cLRkciM24nQ94HydlW4pls0cGnxhFSN3s/gviz/tq?tqx=out:html&tq&gid=1881964512"
webpageRes <- read_html(urlRes)
tablesRes <- html_nodes(webpageRes, "table")
dfRes <- html_table(tablesRes)[[1]]

#### Save ####
save(dfPre, file = "Scraped Pre-Form Data.RData")
save(dfRes, file = "Scraped Form Response Data.RData")