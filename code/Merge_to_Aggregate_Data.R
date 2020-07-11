####read in data####
setwd("C:/Users/ear51/DAAN- Graduate School/_Data Side Projects/Trees/Processed Data Files")
rm(list=ls())
library(dplyr)
library(here)

tree_density <- read.csv(here("data","Tree_density_By_NTA.csv"))
asthma <- read.csv(here("data","asthma_by_NTA.csv"))
census_agg <- read.csv(here("data","census_agg.csv"))
air_pollutants <- read.csv(here("data","NO2_Pollutants_nta.csv"))
water_pollutants <- read.csv(here("data","water_by_nta.csv"))

####merge data####
tree_density$NTA_small <- tolower(gsub("-", " ", tree_density$NTA_small))
census_agg$Group.1 <- tolower(gsub("-", " ", census_agg$Group.1))
asthma$Geography <- tolower(gsub("-", " ", asthma$Geography))
air_pollutants$area <- tolower(gsub("-", " ", air_pollutants$area ))
water_pollutants$area <- tolower(gsub("-", " ", water_pollutants$area ))

data_1 <- left_join(tree_density, census_agg, by = c("NTA_small"= "Group.1"))
data_1 <- left_join(data_1, asthma, by = c("NTA_small"= "Geography")) 

data_water <- left_join(water_pollutants, data_1, by = c("area" = "NTA_small"))
data_air <- left_join(air_pollutants, data_1, by = c("area" = "NTA_small"))

write.csv(data_1, here("data","Joined_aggregate_data.csv"))
write.csv(data_water, here("data","Joined_aggregate_water_data.csv"))
write.csv(data_air, here("data","Joined_aggregate_air_data.csv"))
