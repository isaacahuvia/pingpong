######### Scraping
rm(list = ls())
library(rvest); library(stringr); library(tidyr); library(selectr); library(xml2)

url <- "https://docs.google.com/spreadsheets/d/1CjIj8OHv7_cLRkciM24nQ94HydlW4pls0cGnxhFSN3s/gviz/tq?tqx=out:html&tq&gid=1"
webpage <- read_html(url)
tables <- html_nodes(webpage, "table")
dfMatch <- html_table(tables)[[1]]


#Using setwd then save, instead of just save with the below directory. This way, if someone besides Isaac saves the file,
#they will simply get an error on the setwd command, but the file will still save/load - just to whatever their wd is
save(dfMatch, file = "Scraped Data.RData")