# This file do the web scrapping to obtain the raw data of 2000 People's Daily 's titles
# Approximately 12 minutes to run the file
# Data Source: https://cn.govopendata.com/renminribao/2000/

# Prep work
# rm(list=ls())
# Select working directory
# setwd("~/Desktop/ECON32211 Applied Dissertation A/Stage 2-Question for trial/Data/PD news")
# Load packages
library(rvest)
library(tidyr)
library(dplyr)
library(tidyverse)
library(stringr)


# Read the main web page of 2000 news
url_year <- "https://cn.govopendata.com/renminribao/2000/"
html_year <- paste(readLines(url_year), collapse="\n")


# Extract the link for the monthly news page
matched_month <- str_match_all(html_year, "<a href=\"(.*?)\"")
url_month <- as.vector(matched_month[[1]][6:17,2])
for (i in 1:length(url_month)){
        url_month[i] <- paste("https://cn.govopendata.com", url_month[i], sep="")
}


# Extract the link for the daily news page
# According to the database, the url_day is compose of url_month plus the date
days <- as.character(rep(1:31,12))
url_day <- as.data.frame(matrix(rep(0,12*31),12*31,2))
colnames(url_day) <- c("index","links")
for (i in 1:length(url_month)){
        for (d in 1:31){
                index_day <- (i-1)*31+d
                url_day[index_day,1] <- paste("2000", i, d, sep=".")
                url_day[index_day,2] <- paste(url_month[i], days[index_day], sep="")
        }
}


# Drop the date which doesn't exist in 2000
# Drop 2.30, 4.41, 6,31, 9.31, 11.31
drop_month <- c(2,4,6,9,11)
drop_index <- rep(0,6)
for (i in 1:5){
        drop_index[i] <- (drop_month[i]-1)*31+31
}
# Drop 2.30 
drop_index[6] <-(2-1)*31+30
url_day <- url_day[-drop_index,]
# Reindex the url_day data
rownames(url_day) <- 1:366


# Web scrapping for the news title
news_2000 <- as.data.frame(matrix(rep(0,2),1,2))
colnames(news_2000) <- c("index","news_title")
for(i in 1:366){
        url_date <- url_day[i,2]
        html_date <- read_html(url_date) 
        newst_day <- html_date %>%
                     html_nodes("p") %>%
                     html_text() 
        # Drop the context with symbol and graphs
        newst_day <- str_replace_all(newst_day, "[\r\n]" , "") 
        newst_day <- str_replace_all(newst_day, "[图片]" , "")
        # Tidy the daily data
        indext_day <- as.vector(rep(url_day[i,1],length(newst_day)))
        data_daily <- cbind(indext_day,newst_day)
        colnames(data_daily) <- c("index","news_title")
        # Update the dataset
        news_2000 <- rbind(news_2000,data_daily)
}


# Drop the rows with null information and reindex the data
news_2000.1 <- news_2000[-1,]
news_2000.1 <- news_2000.1[!news_2000.1$news_title==news_2000[12,2],]


# Export data as csv file, for further analysis
write_excel_csv(news_2000.1, file='PD_2000.csv')

