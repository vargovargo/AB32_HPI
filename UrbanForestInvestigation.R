rm(list=ls())

library(tidyverse)

#############################

setwd("~/CapAndTradeProceeds/CalfireUrbanForestry/")
      

projects <- read.table("CalfireUrbanForestAuctionProceeds_wTracts.txt", sep=",", header=T) %>%
  mutate(tractNumber = ct10)
  

tracts <- unique(projects$tractNumber)

CES <- read.csv("CES3_tracts.csv", header=T) %>%
  filter(Census.Tract %in% tracts) %>%
  mutate(tractNumber = Census.Tract) %>%
  select(tractNumber, Total.Population, CES.3.0.Percentile)

HPI <- read.csv("HDI1_tracts.csv", header=T) %>%
  filter(CensusTract %in% tracts) %>%
  mutate(tractNumber = CensusTract) %>%
  select(tractNumber, pop2010, hdi_pctile)


full <- full_join(CES, HPI) %>% full_join(projects) %>%
  gather(CES.3.0.Percentile, hdi_pctile, key = indicator, value = percentile)


ggplot(full, aes(x=PROJ_NAME, y=percentile, fill=indicator)) + geom_bar(stat="identity", position="dodge") +coord_flip()
