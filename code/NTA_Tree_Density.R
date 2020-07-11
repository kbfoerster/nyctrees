#Tree density measurements 

rm(list=ls())
library(dplyr)
library(stringr)
library(tidyr)
library(here)

trees15 = read.csv(here("data","2015StreetTreesCensus_TREES.csv"))
tree_density = read.csv(here("data","NTA_area.csv"))

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

#Need to creates species + latin columns again
trees15$spc_latin = tolower(trees15$spc_latin)
trees15$genus = sub(" .*", "", trees15$spc_latin)
trees15$species = sub("\\S+", "", trees15$spc_latin)
trees15$species = sub("x ", "", trees15$species)
trees15$species = trimws(trees15$species, which="left")
trees15$species = sub(" .*", "", trees15$species)

trees15$species[trees15$species == "species" | trees15$species == "dead" | trees15$species == "unknown"] = ""
trees15$genus[trees15$genus == "unknown"] = ""

trees15$spc_latin = NULL
trees15$spc_common = NULL

#remove missing and NAs
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}
tree_density <- tree_density %>% dplyr::mutate_each(funs(empty_as_na)) #convert empty cells to NA
tree_density <- tree_density %>% drop_na() #193 rows -> 186 rows (should be 188!?) #breezy point...and todt hill missing...
tree_density %>% summarise_all(funs(sum(is.na(.)))) #missing values

names(tree_density)[names(tree_density) == "n"] = "Num_Trees"

trees15$species = empty_as_na(trees15$species)
trees15 = trees15 %>% drop_na(species)

trees15$health = empty_as_na(trees15$health)
trees15 = trees15 %>% drop_na(health)

#calculate densities - small NTA (n = 188)
tree_density$Trees_x_sq_mi = tree_density$Num_Trees / tree_density$area_sqmi
tree_density$Trees_x_acre = tree_density$Num_Trees / tree_density$area_acre

#calculate densities - large NTA (n = 55)
tree_density_2 <- tree_density[, -c(2, 7, 8)]
tree_density_2 <- tree_density_2 %>% group_by(NTA_large)%>%  summarise_all(funs(sum))

tree_density_2$Trees_x_sq_mi = tree_density_2$Num_Trees / tree_density_2$area_sqmi
tree_density_2$Trees_x_acre = tree_density_2$Num_Trees / tree_density_2$area_acre

#calculate different species in a space
listing_species = trees15%>% group_by(nta_name,species) %>% tally()
total_species = listing_species%>% group_by(nta_name) %>% tally()
names(total_species)[names(total_species) == "n"] = "Num_Species"

tree_density = merge(tree_density, total_species, by.y = "nta_name", by.x="NTA_small")

tree_density$Species_x_sq_mi = tree_density$Num_Species / tree_density$area_sqmi
tree_density$Species_x_acre = tree_density$Num_Species / tree_density$area_acre

# Calculate Tree Health (Avg. health in a space)
listing_health = trees15 %>% group_by(nta_name, health) %>% tally()
temp = trees15

temp$health[trees15$health == "Poor"] = 1
temp$health[trees15$health == "Fair"] = 2
temp$health[trees15$health == "Good"] = 3
temp$health = as.numeric(temp$health)

average_health = temp[,c("health","nta_name")] %>% group_by(nta_name)
average_health = average_health %>% summarise(avg_health = mean(health))

tree_density = merge(tree_density, average_health, by.x="NTA_small", by.y="nta_name")

# Calculate sidewalk variables
listing_sidewalk = trees15 %>% group_by(nta_name,sidewalk) %>% tally()
temp = trees15
temp$sidewalk = as.character(temp$sidewalk)

temp$sidewalk[trees15$sidewalk == "Damage"] = 1
temp$sidewalk[trees15$sidewalk == "NoDamage"] = 0
temp$sidewalk = as.numeric(temp$sidewalk)

average_sidewalk = temp[,c("sidewalk","nta_name")] %>% group_by(nta_name)
average_sidewalk = average_sidewalk %>% summarise(avg_sidewalk = mean(sidewalk))

tree_density = merge(tree_density, average_sidewalk, by.x="NTA_small", by.y="nta_name")
