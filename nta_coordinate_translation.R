############################################################
## Combining Kaggle Census Data
############################################################
library(dplyr)
library(proj4)
library(sf)
library(sp)
library(rgeos)
library(rgdal)

setwd("D:/nyctrees")
censusblock = read.csv("census_block_loc.csv")
censustracts = read.csv("nyc_census_tracts.csv")
data = read.csv("C:/users/foers/documents/NYCTrees/data.csv")

# Merging census dataset based on converted tract / blockcode
censusblock$tract = censusblock$BlockCode %/% 10000
censusdata = merge(x=censustracts, y=censusblock, by.x="CensusTract", by.y="tract")

# Verifying boroughs are the same between the two
unique(data$boroname)
unique(censusdata$Borough)

# Dropping possibly irrelevant attributes in the census dataset
dropvar = c("County.y","BlockCode","State","County.x","CensusTract")
censusdata[,dropvar] = NULL



######## Looking up NTA Name based on Coordinates

# Creating dataset that has NTA Name as well as min/max for each coordinate
geotrans = data.frame(NTAName=character(), latmin=character(), latmax=character(), longmin=character(), longmax=character())

for (i in unique(data$NTAName)) {
  NTAName = i
  latmin = (summary(data$latitude[data$NTAName == i]))[[1]]
  latmax = (summary(data$latitude[data$NTAName == i]))[[6]]
  longmin = (summary(data$longitude[data$NTAName == i]))[[1]]
  longmax = (summary(data$longitude[data$NTAName == i]))[[6]]
  temp = data.frame(NTAName=NTAName, latmin=latmin, latmax=latmax, longmin=longmin, longmax=longmax)
  geotrans = rbind(geotrans, temp)
}


##### Using Water Data Method
nta_borders <- read_sf("geo_export_42a67cd3-33d5-483c-8cde-559f7911439c.shp")
nta_census <- st_as_sf(censusdata, coords = c('Longitude', 'Latitude'), crs = st_crs(nta_borders)) #creates geometry coordinates with lat/long

census_points <- nta_census %>% mutate(
  intersection = as.integer(st_intersects(geometry, nta_borders))
  , area = if_else(is.na(intersection), '', nta_borders$ntaname[intersection])
) 

#### Mapping NTA Neighborhoods for EPA Pollution Data
epa_dat = read.csv("NO2_Pollutants_Full.csv")
epa_dat = epa_dat[complete.cases(epa_dat),]
epa_nta = st_as_sf(epa_dat, coords = c('SITE_LONGITUDE', 'SITE_LATITUDE'), crs = st_crs(nta_borders)) #creates geometry coordinates with lat/long
epa  <- epa_nta %>% mutate(
  intersection = as.integer(st_intersects(geometry, nta_borders))
  , area = if_else(is.na(intersection), '', nta_borders$ntaname[intersection])
) 
