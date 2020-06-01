#Tree density measurements 

rm(list=ls())
library(dplyr)
library(stringr)
library(tidyr)

trees15 = read.csv("2015StreetTreesCensus_TREES.csv")
tree_density = read.csv("NTA_area.csv")

#add in sq miles and acres columns -- NOTE: 27878400 ft^2 = 1 mi^2; 43560 ft^2 = 1 acre
tree_density$area_sqmi = tree_density$Shape_Area_ftsq / 27878400
tree_density$area_acre  = tree_density$Shape_Area_ftsq / 43560

#clean up NTA names
trees15$nta_name = as.character(trees15$nta_name)

trees15$nta_name[trees15$nta_name == "Todt Hill-Emerson Hill-Heartland Village-Lighthous"]  = "Todt Hill-Emerson Hill-Heartland Village-Lighthouse Hill"
trees15$nta_name[trees15$nta_name == "Breezy Point-Belle Harbor-Rockaway Park-Broad Chan"]  = "Breezy Point-Belle Harbor-Rockaway Park-Broad Channel"

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

tree_density$NTA_small <- tolower(tree_density$NTA_small)
tree_density$NTA_small <- trim(tree_density$NTA_small)
tree_density$NTA_small <-str_replace_all(tree_density$NTA_small, "[^[:alnum:]]", " ")
tree_density$NTA_small <- gsub("\\s+", " ", str_trim(tree_density$NTA_small))

trees15$nta_name <- tolower(trees15$nta_name)
trees15$nta_name <- trim(trees15$nta_name)
trees15$nta_name <-str_replace_all(trees15$nta_name, "[^[:alnum:]]", " ")
trees15$nta_name <- gsub("\\s+", " ", str_trim(trees15$nta_name))

#merge tree totals to NTA area data
tree_total <- trees15 %>% group_by(nta_name)%>% tally()
tree_total <- as.data.frame(tree_total) #only has 188 NTA 
tree_density <- left_join(tree_density, tree_total, by = c("NTA_small"= "nta_name"))

#remove missing and NAs
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}
tree_density <- tree_density %>% dplyr::mutate_each(funs(empty_as_na)) #convert empty cells to NA
tree_density <- tree_density %>% drop_na() #193 rows -> 186 rows (should be 188!?) #breezy point...and todt hill missing...
tree_density %>% summarise_all(funs(sum(is.na(.)))) #missing values

names(tree_density)[names(tree_density) == "n"] = "Num_Trees"

#calculate densities - small NTA (n = 188)
tree_density$Trees_x_sq_mi = tree_density$Num_Trees / tree_density$area_sqmi
tree_density$Trees_x_acre = tree_density$Num_Trees / tree_density$area_acre

#calculate densities - large NTA (n = 55)
tree_density_2 <- tree_density[, -c(2, 7, 8)]
tree_density_2 <- tree_density_2 %>% group_by(NTA_large)%>%  summarise_all(funs(sum))

tree_density_2$Trees_x_sq_mi = tree_density_2$Num_Trees / tree_density_2$area_sqmi
tree_density_2$Trees_x_acre = tree_density_2$Num_Trees / tree_density_2$area_acre