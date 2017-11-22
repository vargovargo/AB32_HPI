rm(list=ls())

library(tidyverse)

#############################

setwd("~/CapAndTradeProceeds/CalfireUrbanForestry/")
      

projects <- read.table("CalfireUrbanForestAuctionProceeds_wTracts.txt", sep=",", header=T)
