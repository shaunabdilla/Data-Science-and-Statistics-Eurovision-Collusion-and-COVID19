#Packages
install.packages("XML")
install.packages("RSelenium")
install.packages("tidyverse")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Startup docker

library(RSelenium)
library(XML)
library(tidyverse)

# scrape data-----
base.url <- "http://eurovisionworld.com/eurovision/"  #Target URL

remDr <- remoteDriver(port = 4444L, browserName = "chrome", remoteServerAddr = '172.17.0.2') #Docker IP Settings
remDr$open() #Open connection
remDr$setTimeout(type = "page load", milliseconds = 50000) #Set Timeout


for(y in 1957:1957){
  print(y) #Print Year scraping from
  url <- paste0(base.url,y) #form url
  remDr$navigate(url) #go to url
  webElem <- remDr$findElement("css", "body") #Find bottom of body
  webElem$sendKeysToElement(list(key = "end"))
  webElem$sendKeysToElement(list(key = "home"))#scroll to bottom of body - required for table visibility
  remDr$screenshot(display = TRUE) #screenshot page
  xpathv <- "/html/body[@class='h']/div[@class='pagewrap']/div[@id='page']/div[@class='midt']/main/div[@class='voting_year']/div[@id='scoreboard']/div[@class='scoreboard_wrap']/div[@class='scoreboard_scroll mm']/table[@class='scoreboard_table']" #xpath to scoreboard
  webElem <- remDr$findElement(using = 'xpath', value = xpathv) #obtain element
  webElemtxt <- webElem$getElementAttribute("outerHTML")[[1]] #obtain the HTML
  table <- readHTMLTable(webElemtxt, header=F)$`NULL` #read into Table
  xpathvh <- "/html/body[@class='h']/div[@class='pagewrap']/div[@id='page']/div[@class='midt']/main/div[@class='voting_year']/div[@id='scoreboard']/div[@class='scoreboard_wrap']/div[@class='scoreboard_scroll mm']/table[@class='scoreboard_table']/thead/tr" #since header is on in th, we need to get separately
  webElemh <- remDr$findElement(using = 'xpath', value = xpathvh) #obtain header row element
  webElemtxth <- webElemh$getElementAttribute("outerHTML")[[1]] #obtain its HTML
  doc = htmlTreeParse(webElemtxth, useInternalNodes = T) #Parse into HTML tree
  values <- xpathSApply(doc, "//td[@data-from]", xmlGetAttr, 'data-from') #Get attribute values
  table <- table[, c(3,5:ncol(table))] #remove excess columns
  list <- (as.list(values)) #change voter countries to list
  list <- append("Country", list) #Add Country to list
  names(table) <- list #Rename table
  voting.df <- table %>% gather(Voter,Points,-Country) #Make dataframe from table
  write.table(voting.df,paste0(y,".csv"),row.names = F) #Write csv
}


# Semifinal Scraping
for(y in 2004:2007){
  print(y) #Print Year scraping fromd
  url <- paste0(base.url,y,"/semi-final") #form url
  remDr$navigate(url) #go to url
  webElem <- remDr$findElement("css", "body") #Find bottom of body
  webElem$sendKeysToElement(list(key = "end"))
  webElem$sendKeysToElement(list(key = "home"))#scroll to bottom of body - required for table visibility
  remDr$screenshot(display = TRUE) #screenshot page
  xpathv <- "/html/body[@class='h']/div[@class='pagewrap']/div[@id='page']/div[@class='midt']/main/div[@class='voting_year']/div[@id='scoreboard']/div[@class='scoreboard_wrap']/div[@class='scoreboard_scroll mm']/table[@class='scoreboard_table']" #xpath to scoreboard
  webElem <- remDr$findElement(using = 'xpath', value = xpathv) #obtain element
  webElemtxt <- webElem$getElementAttribute("outerHTML")[[1]] #obtain the HTML
  table <- readHTMLTable(webElemtxt, header=F)$`NULL` #read into Table
  xpathvh <- "/html/body[@class='h']/div[@class='pagewrap']/div[@id='page']/div[@class='midt']/main/div[@class='voting_year']/div[@id='scoreboard']/div[@class='scoreboard_wrap']/div[@class='scoreboard_scroll mm']/table[@class='scoreboard_table']/thead/tr" #since header is on in th, we need to get separately
  webElemh <- remDr$findElement(using = 'xpath', value = xpathvh) #obtain header row element
  webElemtxth <- webElemh$getElementAttribute("outerHTML")[[1]] #obtain its HTML
  doc = htmlTreeParse(webElemtxth, useInternalNodes = T) #Parse into HTML tree
  values <- xpathSApply(doc, "//td[@data-from]", xmlGetAttr, 'data-from') #Get attribute values
  table <- table[, c(3,5:ncol(table))] #remove excess columns
  list <- (as.list(values)) #change voter countries to list
  list <- append("Country", list) #Add Country to list
  names(table) <- list #Rename table
  voting.df <- table %>% gather(Voter,Points,-Country) #Make dataframe from table
  write.table(voting.df,paste0(y,"semifinal",".csv"),row.names = F) #Write csv
}

#Semifinals Scraping
for(y in 2008:2019){
  print(y) #Print Year scraping fromd
  url <- paste0(base.url,y,"/semi-final-1") #form url
  remDr$navigate(url) #go to url
  webElem <- remDr$findElement("css", "body") #Find bottom of body
  webElem$sendKeysToElement(list(key = "end"))
  webElem$sendKeysToElement(list(key = "home"))#scroll to bottom of body - required for table visibility
  remDr$screenshot(display = TRUE) #screenshot page
  xpathv <- "/html/body[@class='h']/div[@class='pagewrap']/div[@id='page']/div[@class='midt']/main/div[@class='voting_year']/div[@id='scoreboard']/div[@class='scoreboard_wrap']/div[@class='scoreboard_scroll mm']/table[@class='scoreboard_table']" #xpath to scoreboard
  webElem <- remDr$findElement(using = 'xpath', value = xpathv) #obtain element
  webElemtxt <- webElem$getElementAttribute("outerHTML")[[1]] #obtain the HTML
  table <- readHTMLTable(webElemtxt, header=F)$`NULL` #read into Table
  xpathvh <- "/html/body[@class='h']/div[@class='pagewrap']/div[@id='page']/div[@class='midt']/main/div[@class='voting_year']/div[@id='scoreboard']/div[@class='scoreboard_wrap']/div[@class='scoreboard_scroll mm']/table[@class='scoreboard_table']/thead/tr" #since header is on in th, we need to get separately
  webElemh <- remDr$findElement(using = 'xpath', value = xpathvh) #obtain header row element
  webElemtxth <- webElemh$getElementAttribute("outerHTML")[[1]] #obtain its HTML
  doc = htmlTreeParse(webElemtxth, useInternalNodes = T) #Parse into HTML tree
  values <- xpathSApply(doc, "//td[@data-from]", xmlGetAttr, 'data-from') #Get attribute values
  table <- table[, c(3,5:ncol(table))] #remove excess columns
  list <- (as.list(values)) #change voter countries to list
  list <- append("Country", list) #Add Country to list
  names(table) <- list #Rename table
  voting.df <- table %>% gather(Voter,Points,-Country) #Make dataframe from table
  write.table(voting.df,paste0(y,"semifinal-1",".csv"),row.names = F) #Write csv
}

Semifinals Scraping
for(y in 2008:2019){
  print(y) #Print Year scraping fromd
  url <- paste0(base.url,y,"/semi-final-2") #form url
  remDr$navigate(url) #go to url
  webElem <- remDr$findElement("css", "body") #Find bottom of body
  webElem$sendKeysToElement(list(key = "end"))
  webElem$sendKeysToElement(list(key = "home"))#scroll to bottom of body - required for table visibility
  remDr$screenshot(display = TRUE) #screenshot page
  xpathv <- "/html/body[@class='h']/div[@class='pagewrap']/div[@id='page']/div[@class='midt']/main/div[@class='voting_year']/div[@id='scoreboard']/div[@class='scoreboard_wrap']/div[@class='scoreboard_scroll mm']/table[@class='scoreboard_table']" #xpath to scoreboard
  webElem <- remDr$findElement(using = 'xpath', value = xpathv) #obtain element
  webElemtxt <- webElem$getElementAttribute("outerHTML")[[1]] #obtain the HTML
  table <- readHTMLTable(webElemtxt, header=F)$`NULL` #read into Table
  xpathvh <- "/html/body[@class='h']/div[@class='pagewrap']/div[@id='page']/div[@class='midt']/main/div[@class='voting_year']/div[@id='scoreboard']/div[@class='scoreboard_wrap']/div[@class='scoreboard_scroll mm']/table[@class='scoreboard_table']/thead/tr" #since header is on in th, we need to get separately
  webElemh <- remDr$findElement(using = 'xpath', value = xpathvh) #obtain header row element
  webElemtxth <- webElemh$getElementAttribute("outerHTML")[[1]] #obtain its HTML
  doc = htmlTreeParse(webElemtxth, useInternalNodes = T) #Parse into HTML tree
  values <- xpathSApply(doc, "//td[@data-from]", xmlGetAttr, 'data-from') #Get attribute values
  table <- table[, c(3,5:ncol(table))] #remove excess columns
  list <- (as.list(values)) #change voter countries to list
  list <- append("Country", list) #Add Country to list
  names(table) <- list #Rename table
  voting.df <- table %>% gather(Voter,Points,-Country) #Make dataframe from table
  write.table(voting.df,paste0(y,"semifinal-2",".csv"),row.names = F) #Write csv
}
remDr$close()