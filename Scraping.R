######### Scraping
rm(list = ls())
library(rvest); library(stringr); library(tidyr); library(selectr); library(xml2)

#repeat process for pre-form url, form response url

urlPre <- "https://docs.google.com/spreadsheets/d/1CjIj8OHv7_cLRkciM24nQ94HydlW4pls0cGnxhFSN3s/gviz/tq?tqx=out:html&tq&gid=1714148721"
webpagePre <- read_html(urlPre)
tablesPre <- html_nodes(webpagePre, "table")
dfPre <- html_table(tablesPre)[[1]]

urlRes <- "https://docs.google.com/spreadsheets/d/1CjIj8OHv7_cLRkciM24nQ94HydlW4pls0cGnxhFSN3s/gviz/tq?tqx=out:html&tq&gid=1881964512"
webpageRes <- read_html(urlRes)
tablesRes <- html_nodes(webpageRes, "table")
dfRes <- html_table(tablesRes)[[1]]

#TO DO:
#now save both files differently. In cleanup file, change code to also rbind these 

#Using setwd then save, instead of just save with the below directory. This way, if someone besides Isaac saves the file,
#they will simply get an error on the setwd command, but the file will still save/load - just to whatever their wd is
save(dfPre, file = "Scraped Pre-Form Data.RData")
save(dfRes, file = "Scraped Form Response Data.RData")