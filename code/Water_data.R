library(dplyr)
library(proj4)
library(here)

#read in water-related data files
water_data <- read.csv(here("data","Drinking_Water_Quality_Distribution_Monitoring_Data.csv"), header = TRUE)
site_index <- read.csv(here("data","Distribution_Water_Quality_Sampling_Sites_for_OpenData.csv"), header = TRUE)

water_data_2 <- left_join(water_data, site_index, by = c("Sample.Site"= "�..Site"))
water_data_2$ID <- seq.int(nrow(water_data_2))

#convert x/y to lat/long: https://gis.stackexchange.com/questions/325497/error-in-converting-xy-coordinates-to-lat-long-using-r-proj4-library
xy <- water_data_2 %>% select(X...Coordinate, Y...Coordinate, ID)
xy <- as.list(xy)
proj4string <- "+proj=lcc +lat_1=40.66666666666666 +lat_2=41.03333333333333 +lat_0=40.16666666666666 +lon_0=-74 +x_0=300000 +y_0=0 +datum=NAD83 +units=us-ft +no_defs"
pj <- project(xy, proj4string, inverse=TRUE)
points <- data.frame(xy, lat=pj$y, lon=pj$x)
final_water_data <-  merge(water_data_2, points, by.x = "ID", by.y = "ID")
final_water_data <- final_water_data[,c(1, 3:5,7:12, 16:19)]

#remove rows without coordinates:
colSums(is.na(final_water_data))
final_water_data<- final_water_data[complete.cases(final_water_data), ]

#map nta borders from shapefile: https://github.com/r-spatial/sf
library(sf)

nta_borders <- read_sf(here("data","geo_export_42a67cd3-33d5-483c-8cde-559f7911439c.shp"))
print(nta_borders)
plot(nta_borders)

#classify nta by lat/long coordinates
water_points_sf <- st_as_sf(final_water_data, coords = c('lon', 'lat'), crs = st_crs(nta_borders)) #creates geometry coordinates with lat/long

water_points <- water_points_sf %>% mutate(
  intersection = as.integer(st_intersects(geometry, nta_borders))
  , area = if_else(is.na(intersection), '', nta_borders$ntaname[intersection])
) 

head(water_points)
water_points[,c("X","ID","geometry")] = NULL
write.csv(water_points, here("data","water_by_nta.csv"), row.names = F)




