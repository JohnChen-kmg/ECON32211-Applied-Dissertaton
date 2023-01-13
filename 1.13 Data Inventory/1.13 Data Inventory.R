# This file make the relevant tables for data inventory essay
# Use 2000 People's daily data and 2000/2006 migration data

# Prep work
# rm(list=ls())
# Select working directory
# setwd("~/Desktop/ECON32211 Applied Dissertation A/1.13 Data Inventory")
# Load packages
library(readr)
library(haven)
library(tidyr)
library(dplyr)
library(tidyverse)
library(stringr)
library(reporttools)

# Load the data
# News Exposure Rate data     
PD_2000exp <- read_csv("PD_2000exp.csv")
PD_2000exp <- as.data.frame(PD_2000exp)
# Migration data
migration_data <- read_dta("migration_data.dta")


# Part 1: News Exposure Rate data
# Summary Statistics
options(digits=5)
tableContinuous(PD_2000exp[,c(2,3)], stats=c("n","mean","median","max","min"))
# Exposure rate digits not showing, do it manually
mean(PD_2000exp$exp_rate2000)
median(PD_2000exp$exp_rate2000)
max(PD_2000exp$exp_rate2000)
min(PD_2000exp$exp_rate2000)

# Part 2: Migration data
# Summary Statistics
length(migration_data$mij_mii2000)
mean(migration_data$mij_mii2000)
median(migration_data$mij_mii2000)
max(migration_data$mij_mii2000)
min(migration_data$mij_mii2000)

length(migration_data$Vj)
mean(migration_data$Vj)
median(migration_data$Vj)
max(migration_data$Vj)
min(migration_data$Vj)

length(migration_data$Vi)
mean(migration_data$Vi)
median(migration_data$Vi)
max(migration_data$Vi)
min(migration_data$Vi)

dis <- migration_data[!is.na(migration_data$distance),]$distance
length(dis)
mean(dis)
median(dis)
max(dis)
min(dis)
