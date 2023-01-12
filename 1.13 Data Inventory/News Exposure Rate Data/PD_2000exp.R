# This file compute the news exposure rate using the data of 2000 People's Daily titles
# Note: News Exposure Rate of Province s = Number of Articles about s / Number of Domestic News

# Prep work
# rm(list=ls())
# Select working directory
# setwd("~/Desktop/ECON32211 Applied Dissertation A/Stage 2-Question for trial/Data/PD news")
# Load packages
library(readr)
library(tidyr)
library(dplyr)
library(tidyverse)
library(quanteda)

# Require scrapped data of 2000 People's Daily's titles: PD_2000.csv
PD_2000 <- read_csv("PD_2000.csv")

# Create a vectoe contiaing the targeted peovinces
# 31 mainland China provincial municipalities
chn_prov <- c("河北","山西","辽宁","吉林","黑龙江","江苏","浙江","安徽","福建",
             "江西","山东","河南","湖北","湖南","广东","海南","四川","贵州",
             "云南","陕西","甘肃","青海","内蒙古","广西","西藏","宁夏","新疆",
             "北京","天津","上海","重庆")

# Translate the Chinese into English
eng_prov <- c("Hebei","Shanxi","Liaoning","Jilin","Heilongjiang","Jiangsu",
              "Zhejiang","Anhui","Fujian","Jiangxi","Shandong","Henan","Hubei",
              "Hunan", "Guangdong","Hainan","Sichuan", "Guizhou", "Yunnan", 
              "Shannxi", "Gansu","Qinghai","Inner Mongolia","Guangxi","Tibet",
              "Ningxia","Xinjiang","Beijing","Tianjing","Shanghai","Chongqing")
prov <- cbind(chn_prov, eng_prov)

# Count the apperance of each province in the 2000 People's Daily
pd2000_freq <- tokens(paste(unlist(PD_2000[,2]), collapse =" ")) %>%
        tokens_select(c(prov[,1])) %>% dfm()

# Tidy the data
pd2000_exp <- cbind(pd2000_freq@Dimnames$features, as.numeric(pd2000_freq@x))

# However, Inner Mongolia can't not be recognized, so we use different function to it
row31 <- cbind("内蒙古",sum(str_count(PD_2000, "内蒙古")))
pd2000_exp <- rbind(pd2000_exp, row31)
colnames(pd2000_exp) <- c("chn_prov", "freq")

# Add the English label to provinces
pd_2000exp <- merge(pd2000_exp,prov)[,c(3,2)]

# Compute the exposure rate for every province
n2000 <- sum(as.numeric(pd_2000exp$freq))
pd_2000exp$exp_rate2000 <- as.numeric(pd_2000exp$freq)/n2000 

# Export data as csv file, for further analysis
write_excel_csv(pd_2000exp, file='PD_2000exp.csv')
