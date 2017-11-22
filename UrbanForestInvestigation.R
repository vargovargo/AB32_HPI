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

###############
# All tracts
###############

CES2 <- read.csv("CES3_tracts.csv", header=T) %>%
  mutate(tractNumber = Census.Tract) %>%
  select(tractNumber, Total.Population, CES.3.0.Percentile)

HPI2 <- read.csv("HDI1_tracts.csv", header=T) %>%
  mutate(tractNumber = CensusTract) %>%
  select(tractNumber, pop2010, hdi_pctile, ces2_pctile)


full2 <- full_join(CES2, HPI2)  %>%
  gather(CES.3.0.Percentile, ces2_pctile, key = CESind, value = percentile) %>% 
  mutate(venn = ifelse(percentile >= 75 & hdi_pctile >= 75, "Both CES and HPI DAC",
                       ifelse(percentile >= 75 & hdi_pctile < 75, "CES DAC only",
                              ifelse(percentile < 75 & hdi_pctile >= 75, "HPI DAC only","Not a DAC"))))


ggplot(full2, aes(x=hdi_pctile, y=percentile, color=CESind)) + geom_point(size=1, alpha=0.5) + 
  geom_hline(yintercept = 75, size=2, alpha=0.5) + 
  geom_vline(xintercept = 75, size=2, alpha=0.5)

ggplot(full2, aes(x=hdi_pctile, y=percentile, color=CESind)) + geom_point(size=1, alpha=0.3) + 
  geom_smooth(se = T) + facet_grid(.~ CESind)  


ggplot(full2, aes(x=hdi_pctile, y=percentile, color=venn)) + geom_point(size=1, alpha=0.5) + 
  geom_hline(yintercept = 75, size=2, alpha=0.5) + 
  geom_vline(xintercept = 75, size=2, alpha=0.5)


full2 %>% group_by(venn, CESind) %>%
  summarise(numberTracts = length(pop2010), 
            numberPeople = sum(pop2010, na.rm=T)) %>%
  ggplot(aes(x=venn, y = numberPeople, fill= CESind)) + geom_bar(stat="identity", position="dodge") +
  coord_flip()
