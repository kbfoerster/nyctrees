library(dplyr)
library(NCmisc)
library(scales)
library(reshape2)
library(ggplot2)
library(ggcorrplot)
library(questionr)
library(finalfit) 
library(tidyr)
library(naniar)
library(VIM)
library(sm)
library(caret)
library(diptest)
library(e1071)
library(nnet)

trees05 = read.csv("2005_Street_Tree_Census.csv")
trees15 = read.csv("2015StreetTreesCensus_TREES.csv")
ntatranslation = read.csv("NTA_Name Translation.csv", colClasses = "character")
demo = read.csv("Combined-CoreData_NYC.csv")

names(demo)[names(demo) == "Sub.Borough.Area"] = "nta_name"
names(demo)[names(demo) == "Density_.2015"] = "Density_2015"
##############################################################################################################
##      Looking at boronames that are similar between Trees Censi (Censuses?)
##############################################################################################################
temp15 = unique(trees15[!(trees15$nta_name %in% trees05$nta_name),])
unique(temp15$nta_name)
# Only names in '15 that aren't in '05 are misspelled below:
#
## 'Convert from 15 -> '05
# Todt Hill-Emerson Hill-Heartland Village-Lighthous -> Todt Hill-Emerson Hill-Heartland Village-Lighthouse Hill
# Breezy Point-Belle Harbor-Rockaway Park-Broad Chan -> Breezy Point-Belle Harbor-Rockaway Park-Broad Channel

##################################################################################################################
## Selecting Relevant Columns, fixing column names, adding dataset year, and combining Trees
##################################################################################################################
#creating a new column in 2015 data that combines health and status to match the data in 2005 
trees15$status <- as.character(trees15$status)
trees15$health <- as.character(trees15$health)
trees15$healthstatus <- ifelse(trees15$health == "", trees15$status, trees15$health)
trees15$healthstatus <- ifelse(trees15$healthstatus == "Fair", "Good", trees15$healthstatus)

trees05$status <- as.character(trees05$status)
names(trees05)[names(trees05) == "status"] = "healthstatus" 
trees05 <- trees05 %>% mutate(healthstatus=recode(healthstatus,"Excellent" = "Good")) #excellent = good to make consistent with 2015 values
                                         
## Standardizing '15 census columns to '05 census
names(trees15)[names(trees15) == "guards"] = "inf_guard"
names(trees15)[names(trees15) == "brnch_shoe"] = "inf_shoes"
names(trees15)[names(trees15) == "sidewalk"] = "sidw_crack"
names(trees15)[names(trees15) == "Latitude"] = "latitude"

str(trees05)
trees05 = trees05[c("address","boro_ct","borocode","boroname","cncldist","inf_guard","inf_shoes","latitude","longitude","nta","nta_name","sidw_crack","spc_common","spc_latin","st_assem","st_senate","healthstatus","tree_dbh","x_sp","y_sp","zip_city","zipcode")]
trees15 = trees15[c("address","boro_ct","borocode","boroname","cncldist","inf_guard","inf_shoes","latitude","longitude","nta","nta_name","sidw_crack","spc_common","spc_latin","st_assem","st_senate","healthstatus","tree_dbh","x_sp","y_sp","zip_city","zipcode")]
trees05$year = 2005
trees15$year = 2015
trees = rbind(trees05, trees15)

##################################################################################################################
## Standardizing nta_name
##################################################################################################################

trees$nta_name = as.character(trees$nta_name)

trees$nta_name[trees$nta_name == "Todt Hill-Emerson Hill-Heartland Village-Lighthous"]  = "Todt Hill-Emerson Hill-Heartland Village-Lighthouse Hill"
trees$nta_name[trees$nta_name == "Breezy Point-Belle Harbor-Rockaway Park-Broad Chan"]  = "Breezy Point-Belle Harbor-Rockaway Park-Broad Channel"

# Changing the tree census names to match demographic names
for (i in 1:nrow(ntatranslation)){
  trees$nta_name[trees$nta_name == ntatranslation$X05.Trees[i]] = ntatranslation$New.Trees[i]
} 

data = merge(trees, demo, by="nta_name")

##################################################################################################################
## Prepping data...
##################################################################################################################

dups <- data[duplicated(data),] #NOTE: keep duplicates - no duplicates in raw datasets based on tree IDs!
nrow(dups)

#consolidate demographic data columns by half (based on census year)
data$income <- ifelse(data$year == 2005, data$Income_2005, data$Income_2015)
data$education <- ifelse(data$year == 2005, data$Ed_2005, data$Ed_2015)
data$population <- ifelse(data$year == 2005, data$Pop_2005, data$Pop_2015)
data$unemployment <- ifelse(data$year == 2005, data$Unemploy_2005, data$Unemploy_2015)
data$popDensity <- ifelse(data$year == 2005, data$Density_2006, data$Density_2015) #Note: uses 2006 data instead of 2005!
data$laborforce <- ifelse(data$year == 2005, data$Labor_2005, data$Labor_2015)
data$poverty <- ifelse(data$year == 2005, data$Poverty_2005, data$Poverty_2015)
data$diversity <- ifelse(data$year == 2005, data$Diversity_2005, data$Diversity_2015)
data <- data %>% select(-c(2, 24:39)) #also removes address

data$boroname[data$boroname == 5] = "Staten Island"

num_vars <- c('latitude', 'longitude', 'x_sp', 'y_sp', 'income', 'education', 'population', 'unemployment', 
              'popDensity', 'laborforce', 'poverty', 'diversity', 'tree_dbh')
cate_vars <- c('boro_ct', 'borocode', 'st_assem', 'st_senate', 'zip_city', 'year', 'nta_name', 'boroname', 
               'cncldist', 'inf_guard', 'inf_shoes', 'nta', 'sidw_crack', 'spc_common', 'spc_latin', 
               'healthstatus', 'zipcode') 

# Standardizing 'inf_guard'
data$inf_guard[data$inf_guard == "Harmful" | data$inf_guard == "Helpful" | data$inf_guard == "Unsure"] = "Yes"
data$inf_guard[data$inf_guard == "None"] = "No"

# Standardizing 'sidw_crack'
data$sidw_crack[data$sidw_crack == "NoDamage"] = "No"
data$sidw_crack[data$sidw_crack == "Damage"] = "Yes"

# Creating 'genus' and 'species' attributes from 'spc_latin' - not included in Table 1 - added afterwards
data$spc_latin = tolower(data$spc_latin)
data$genus = sub(" .*", "", data$spc_latin)
data$species = sub("\\S+", "", data$spc_latin)
data$species = sub("x ", "", data$species)
data$species = trimws(data$species, which="left")
data$species = sub(" .*", "", data$species)

data$species[data$species == "species" | data$species == "dead" | data$species == "unknown"] = ""
data$genus[data$genus == "unknown"] = ""

data$spc_latin = NULL
data$spc_common = NULL


##################################################################################################################
## N sizes and missing values
##################################################################################################################
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

data <- data %>% mutate_each(funs(empty_as_na)) #convert empty cells to NA
data %>% summarise_all(funs(sum(!is.na(.)))) #non-na n size of each column
data %>% summarise_all(funs(sum(is.na(.)))) #missing values

##################################################################################################################
## Numerical Variables summaries
##################################################################################################################
data$population <- as.numeric(data$population)
data$tree_dbh <- as.numeric(data$tree_dbh)
numdata <- data[,num_vars]

#descriptive statistics
for (i in 1:ncol(numdata)){
  x <- numdata[,i]
  print(colnames(numdata[i]))
  print(paste("Mean = ", mean(x, na.rm =TRUE)))
  print(paste("Median =", median(x, na.rm = TRUE)))
  print(paste("Standard Deviation = ", sd(x, na.rm = TRUE)))
  print(paste("Range =", range(x, na.rm = TRUE)))
  # y <- ks.test(x, "pnorm", mean=mean(x), sd=sd(x), na.omit()) #Kolmogorov-Smirnov test normality testing: if p-value >0.05, normal; if p < 0.05 not normal
  # print(paste("Normality = ", y$p.value)) 
  out <- which.outlier(x, thr = 3, method = "sd", high = TRUE, low = TRUE)
  print(paste("Outliers =", length(out)))
  print('')
}


#Correlation matrix (pre-data cleaning)
cor_all = cor(numdata, use = "pairwise.complete.obs", method = "pearson")
ggcorrplot(cor_all, method = "circle", type = "upper", lab = FALSE)

#min/max scale for overlapping density plot...
numdata_scaled <- numdata %>%
  mutate_at(c(1:13), funs(c(rescale(.))))
summary(numdata_scaled)
Variable <- num_vars
df.m <- melt(numdata_scaled)
p1 <- ggplot(df.m) + geom_freqpoly(aes(x = value,
                                       y = ..density.., colour = variable))
p1


# Violin Plot
dbout = which.outlier(numdata$tree_dbh, thr = 3, method = "sd", high = TRUE, low = TRUE)
numdata = numdata[-dbout,]
numscale = scale(numdata)
nummelt = melt(numscale)
ggplot(nummelt, aes(x=Var2, y=value, fill=Var2)) +
  geom_violin() + 
  labs(x="Attributes", fill="Attributes") + 
  theme(axis.text.x = element_text(angle = 90))

### Violin Plot on 0-1 scale
ggplot(df.m, aes(x=variable, y=value, color=variable)) +
  geom_violin(scale=count) +
  labs(x="Attributes", fill="Attributes") +
  theme(axis.text.x = element_text(angle = 90))

##################################################################################################################
## Categorical Variables summaries
##################################################################################################################
#recode variable types as factors
data$boro_ct <- as.factor(data$boro_ct)
data$borocode <- as.factor(data$borocode)
data$st_assem <- as.factor(data$st_assem)
data$st_senate <- as.factor(data$st_senate)
data$year <- as.factor(data$year)
data$cncldist <- as.factor(data$cncldist)
data$zipcode <- as.factor(data$zipcode)

catdata <- data[, cate_vars]

#descriptive statistics
for (i in 1:ncol(catdata)){
  x <- catdata[,i]
  print(paste(colnames(catdata[i]), ": Unique values = ", length(unique(na.omit(x)))))
}

for (i in 1:ncol(catdata)){
  x <- catdata[,i]
  print(colnames(catdata[i]))
  print(freq(x))
}

##################################################################################################################
## Missingness evaluation
##################################################################################################################
zip_t <- unique(data[c(18, 19)]) #extract just zipcde and zip_city 
data$zipcode[data$zipcode == 0] <- NA

#missing value exploration
k <- data[!complete.cases(data),]
rows_missing <- nrow(k) / nrow(data)

p <- data %>% missing_plot() #missing values map
missing_pattern(data) #matrix of missing values
gg_miss_upset(data, nsets = n_var_miss(data)) #visual that shows overlap of NA values across variables

#Type of missingness-- MAR, MCAR, MNAR?
data_missing <- data

#create binary missing vs. not missing variables
data_missing$boro_ct_missing[is.na(data_missing$boro_ct)] <- "missing"
data_missing$boro_ct_missing[data_missing$boro_ct != "missing" ] <- "not missing"
data_missing$inf_guard_missing[is.na(data_missing$inf_guard)] <- "missing"
data_missing$inf_guard_missing[data_missing$inf_guard != "missing" ] <- "not missing"
data_missing$sidw_crack_missing[is.na(data_missing$sidw_crack)] <- "missing"
data_missing$sidw_crack_missing[data_missing$sidw_crack != "missing" ] <- "not missing"
data_missing$genus_missing[is.na(data_missing$genus)] <- "missing"
data_missing$genus_missing[data_missing$genus != "missing" ] <- "not missing"
data_missing$species_missing[is.na(data_missing$species)] <- "missing"
data_missing$species_missing[data_missing$species != "missing" ] <- "not missing"
data_missing$zip_city_missing[is.na(data_missing$zip_city)] <- "missing"
data_missing$zip_city_missing[data_missing$zip_city != "missing" ] <- "not missing"
data_missing$unemployment_missing[is.na(data_missing$unemployment)] <- "missing"
data_missing$unemployment_missing[data_missing$unemployment != "missing" ] <- "not missing"
data_missing$laborforce_missing[is.na(data_missing$laborforce)] <- "missing"
data_missing$laborforce_missing[data_missing$laborforce != "missing" ] <- "not missing"
data_missing[c(31:38)] <- lapply(data_missing[c(31:38)], factor)

#numerical differences (Kruskal Wallis and chi-square).. just some of many combinations of tests
kruskal.test(data_missing$income~data_missing$unemployment_missing)
kruskal.test(data_missing$income~data_missing$laborforce_missing) 

table <- table(data_missing$boro_ct_missing, data_missing$nta_name) 
chisq.test(table)

table <- table(data_missing$boroname, data_missing$inf_guard_missing)
chisq.test(table)

table <- table(data_missing$nta_name, data_missing$sidw_crack_missing)
chisq.test(table)

table <- table(data_missing$nta_name, data_missing$genus_missing)
chisq.test(table)

table <- table(data_missing$nta_name, data_missing$species_missing)
chisq.test(table)

table <- table(data_missing$boroname, data_missing$zip_city_missing)
chisq.test(table)


##################################################################################################################
## Outlier evaluation
##################################################################################################################

# Boxplot for tree_dbh attribute
ggplot(data, aes(x= "",y=tree_dbh)) + geom_boxplot() + labs(x="tree_dbh", y="Value") + ggtitle("Boxplot for tree_dbh Attribute")

# Removing outliers based on 'tree_dbh'
data = data[!data$tree_dbh == -1,]
data = data[data$tree_dbh < (3*sd(data$tree_dbh)),] #removes 69683 outliers above 3 sd
##################################################################################################################
## Modality Analysis
##################################################################################################################
num_data = data[,num_vars]

dip(num_data$latitude)
dip(num_data$longitude)
dip(num_data$x_sp)
dip(num_data$y_sp)
dip(num_data$income)
dip(num_data$education)
dip(num_data$population)
dip(num_data$latitude)
dip(num_data$popDensity)
dip(num_data$poverty)
dip(num_data$diversity)
dip(num_data$tree_dbh)

unemployment = sort(num_data$unemployment)
laborforce = sort(num_data$laborforce)
dip(unemployment)
dip(laborforce)

# density plots by groups
sm.density.compare(data$education, data$boroname, xlab="education")
title(main="Education by Borough")

sm.density.compare(data$tree_dbh, data$boroname, xlab="tree diameter")
title(main="Tree diameters by Borough")

##################################################################################################################
## Missing value imputation
##################################################################################################################

m1 <- c("zip_city", "zipcode")
d1 <- c("x_sp", "y_sp")
m2 <- c("unemployment", "laborforce")
d2<- c("poverty", "diversity", "popDensity", "income", "education", "population")
m3 <- c("boro_ct")
d3 <- c("borocode", "nta_name")
m4 <- c("inf_guard", "sidw_crack")
d4 <- c("nta_name", "income", "healthstatus", "tree_dbh", "inf_shoes")
m5 <- c("species", "genus")
d5 <- c("nta_name","healthstatus", "tree_dbh")

data_final <- kNN(data = data, variable = m1, k = 7, dist_var = d1)

#computer cannot handle the following large computations without paritioning data:
  set.seed(16827)
  partition <- createDataPartition(data$nta_name, p = .5, list = FALSE, times = 1)
  data_final_A <- data_final[ partition,]
  data_final_B  <- data_final[-partition,]
  partition2 <- createDataPartition(data_final_A$nta_name, p = .5, list = FALSE, times = 1)
  data_final_C <- data_final_A[ partition2,]
  data_final_D  <- data_final_A[-partition2,]
  partition3 <- createDataPartition(data_final_B$nta_name, list = FALSE, times = 1)
  data_final_E <- data_final_B[ partition3,] 
  data_final_F  <- data_final_B[-partition3,]
  partition4 <- createDataPartition(data_final_C$nta_name, list = FALSE, times = 1)
  data_final_1 <- data_final_C[ partition4,]
  data_final_2  <- data_final_C[-partition4,]
  partition5 <- createDataPartition(data_final_D$nta_name, list = FALSE, times = 1)
  data_final_3 <- data_final_D[ partition5,]
  data_final_4  <- data_final_D[-partition5,]
  partition6 <- createDataPartition(data_final_E$nta_name, list = FALSE, times = 1)
  data_final_5 <- data_final_E[ partition6,]
  data_final_6  <- data_final_E[-partition6,]
  partition7 <- createDataPartition(data_final_F$nta_name, list = FALSE, times = 1)
  data_final_7 <- data_final_F[ partition7,]
  data_final_8  <- data_final_F[-partition7,]
  partition9 <- createDataPartition(data_final_1$nta_name, list = FALSE, times = 1)
  data_final_1a <- data_final_1[ partition9,]
  data_final_1b  <- data_final_1[-partition9,]
  partition10 <- createDataPartition(data_final_2$nta_name, list = FALSE, times = 1)
  data_final_2a <- data_final_2[ partition10,]
  data_final_2b <- data_final_2[-partition10,]
  partition11 <- createDataPartition(data_final_3$nta_name, list = FALSE, times = 1)
  data_final_3a <- data_final_3[ partition11,]
  data_final_3b <- data_final_3[-partition11,]
  partition12 <- createDataPartition(data_final_4$nta_name, list = FALSE, times = 1)
  data_final_4a <- data_final_4[ partition12,]
  data_final_4b <- data_final_4[-partition12,]
  partition13 <- createDataPartition(data_final_5$nta_name, list = FALSE, times = 1)
  data_final_5a <- data_final_5[ partition13,]
  data_final_5b <- data_final_5[-partition13,]
  partition14 <- createDataPartition(data_final_6$nta_name, list = FALSE, times = 1)
  data_final_6a <- data_final_6[ partition14,]
  data_final_6b <- data_final_6[-partition14,]
  partition15 <- createDataPartition(data_final_7$nta_name, list = FALSE, times = 1)
  data_final_7a <- data_final_7[ partition15,]
  data_final_7b <- data_final_7[-partition15,]
  partition16 <- createDataPartition(data_final_8$nta_name, list = FALSE, times = 1)
  data_final_8a <- data_final_8[ partition16,]
  data_final_8b <- data_final_8[-partition16,]

#Note: same 4 lines of code repeated for each subsetted dataframe
data_final_1a <- kNN(data = data_final_1a, variable = m5, k = 7, dist_var = d5)
data_final_1a <- kNN(data = data_final_1a, variable = m2, k = 7, dist_var = d2) 
data_final_1a <- kNN(data = data_final_1a, variable = m3, k = 7, dist_var = d3)
data_final_1a <- kNN(data = data_final_1a, variable = m4, k = 7, dist_var = d4)
data_final_1b <- kNN(data = data_final_1b, variable = m5, k = 7, dist_var = d5)
data_final_1b <- kNN(data = data_final_1b, variable = m2, k = 7, dist_var = d2) 
data_final_1b <- kNN(data = data_final_1b, variable = m3, k = 7, dist_var = d3)
data_final_1b <- kNN(data = data_final_1b, variable = m4, k = 7, dist_var = d4)
data_final_2a <- kNN(data = data_final_2a, variable = m5, k = 7, dist_var = d5)
data_final_2a <- kNN(data = data_final_2a, variable = m2, k = 7, dist_var = d2) 
data_final_2a <- kNN(data = data_final_2a, variable = m3, k = 7, dist_var = d3)
data_final_2a <- kNN(data = data_final_2a, variable = m4, k = 7, dist_var = d4)
data_final_2b <- kNN(data = data_final_2b, variable = m5, k = 7, dist_var = d5)
data_final_2b <- kNN(data = data_final_2b, variable = m2, k = 7, dist_var = d2) 
data_final_2b <- kNN(data = data_final_2b, variable = m3, k = 7, dist_var = d3)
data_final_2b <- kNN(data = data_final_2b, variable = m4, k = 7, dist_var = d4)
data_final_3a <- kNN(data = data_final_3a, variable = m5, k = 7, dist_var = d5)
data_final_3a <- kNN(data = data_final_3a, variable = m2, k = 7, dist_var = d2) 
data_final_3a <- kNN(data = data_final_3a, variable = m3, k = 7, dist_var = d3)
data_final_3a <- kNN(data = data_final_3a, variable = m4, k = 7, dist_var = d4)
data_final_3b <- kNN(data = data_final_3b, variable = m5, k = 7, dist_var = d5)
data_final_3b <- kNN(data = data_final_3b, variable = m2, k = 7, dist_var = d2) 
data_final_3b <- kNN(data = data_final_3b, variable = m3, k = 7, dist_var = d3)
data_final_3b <- kNN(data = data_final_3b, variable = m4, k = 7, dist_var = d4)
data_final_4a <- kNN(data = data_final_4a, variable = m5, k = 7, dist_var = d5)
data_final_4a <- kNN(data = data_final_4a, variable = m2, k = 7, dist_var = d2) 
data_final_4a <- kNN(data = data_final_4a, variable = m3, k = 7, dist_var = d3)
data_final_4a <- kNN(data = data_final_4a, variable = m4, k = 7, dist_var = d4)
data_final_4b <- kNN(data = data_final_4b, variable = m5, k = 7, dist_var = d5)
data_final_4b <- kNN(data = data_final_4b, variable = m2, k = 7, dist_var = d2) 
data_final_4b <- kNN(data = data_final_4b, variable = m3, k = 7, dist_var = d3)
data_final_4b <- kNN(data = data_final_4b, variable = m4, k = 7, dist_var = d4)
data_final_5a <- kNN(data = data_final_5a, variable = m5, k = 7, dist_var = d5)
data_final_5a <- kNN(data = data_final_5a, variable = m2, k = 7, dist_var = d2) 
data_final_5a <- kNN(data = data_final_5a, variable = m3, k = 7, dist_var = d3)
data_final_5a <- kNN(data = data_final_5a, variable = m4, k = 7, dist_var = d4)
data_final_5b <- kNN(data = data_final_5b, variable = m5, k = 7, dist_var = d5)
data_final_5b <- kNN(data = data_final_5b, variable = m2, k = 7, dist_var = d2) 
data_final_5b <- kNN(data = data_final_5b, variable = m3, k = 7, dist_var = d3)
data_final_5b <- kNN(data = data_final_5b, variable = m4, k = 7, dist_var = d4)
data_final_6a <- kNN(data = data_final_6a, variable = m5, k = 7, dist_var = d5)
data_final_6a <- kNN(data = data_final_6a, variable = m2, k = 7, dist_var = d2) 
data_final_6a <- kNN(data = data_final_6a, variable = m3, k = 7, dist_var = d3)
data_final_6a <- kNN(data = data_final_6a, variable = m4, k = 7, dist_var = d4)
data_final_6b <- kNN(data = data_final_6b, variable = m5, k = 7, dist_var = d5)
data_final_6b <- kNN(data = data_final_6b, variable = m2, k = 7, dist_var = d2) 
data_final_6b <- kNN(data = data_final_6b, variable = m3, k = 7, dist_var = d3)
data_final_6b <- kNN(data = data_final_6b, variable = m4, k = 7, dist_var = d4)
data_final_7a <- kNN(data = data_final_7a, variable = m5, k = 7, dist_var = d5)
data_final_7a <- kNN(data = data_final_7a, variable = m2, k = 7, dist_var = d2) 
data_final_7a <- kNN(data = data_final_7a, variable = m3, k = 7, dist_var = d3)
data_final_7a <- kNN(data = data_final_7a, variable = m4, k = 7, dist_var = d4)
data_final_7b <- kNN(data = data_final_7b, variable = m5, k = 7, dist_var = d5)
data_final_7b <- kNN(data = data_final_7b, variable = m2, k = 7, dist_var = d2) 
data_final_7b <- kNN(data = data_final_7b, variable = m3, k = 7, dist_var = d3)
data_final_7b <- kNN(data = data_final_7b, variable = m4, k = 7, dist_var = d4)
data_final_8a <- kNN(data = data_final_8a, variable = m5, k = 7, dist_var = d5)
data_final_8a <- kNN(data = data_final_8a, variable = m2, k = 7, dist_var = d2) 
data_final_8a <- kNN(data = data_final_8a, variable = m3, k = 7, dist_var = d3)
data_final_8a <- kNN(data = data_final_8a, variable = m4, k = 7, dist_var = d4)
data_final_8b <- kNN(data = data_final_8b, variable = m5, k = 7, dist_var = d5)
data_final_8b <- kNN(data = data_final_8b, variable = m2, k = 7, dist_var = d2) 
data_final_8b <- kNN(data = data_final_8b, variable = m3, k = 7, dist_var = d3)
data_final_8b <- kNN(data = data_final_8b, variable = m4, k = 7, dist_var = d4)

#combine subsetted dataframes with imputed data back into one dataset again
d <- c("data_final_1a", "data_final_1b", "data_final_2a", "data_final_2b", "data_final_3a", "data_final_3b", "data_final_4a", "data_final_4b",
       "data_final_5a", "data_final_5b", "data_final_6a", "data_final_6b", "data_final_7a", "data_final_7b", "data_final_8a", "data_final_8b")
final_data <- data_final_1a[(1:30)] #only keep original columns
for (i in 2:length(d)){  
  tmp <- d[i]
  tmp <- tmp[,(1:30)] #removes unneeded columns added during imputation
  final_data <- merge(final_data, tmp, all = TRUE, sep= "\\")
}

nrow(final_data) == nrow(data) #should be true
final_data %>% summarise_all(funs(sum(is.na(.)))) #no missing values
mvp2 <- final_data %>% missing_plot() #missing values map  - updated
##################################################################################################################
## final data set
##################################################################################################################
#write.csv(final_data, "trees_final_data.csv")


##################################################################################################################
## Week 11 Feedback
##################################################################################################################
`%ni%` <- Negate(`%in%`)
temp.fact = c("boro_ct","borocode","cncldist","st_assem","st_senate","zipcode","year")

# Reading in imputed/final data
data = read.csv("trees_final_data.csv")
data = data[,-1]
# Breaking final data into numerical and categorical
data.num = data[,num_vars]
data.cat = subset(data,select = names(data) %ni% num_vars)

# Reading in raw data (outliers and missing data)
old.data = read.csv("data.csv")
# Breaking data into numerical and categorical
old.data.num = old.data[,num_vars]
old.data.cat = subset(old.data, select = names(old.data) %ni% num_vars)

# Creating another dataset that has the outliers removed
# Refered to as 'cut' data below
cut.data = old.data[old.data$tree_dbh < (3*sd(old.data$tree_dbh)),] 
cut.data = cut.data[!cut.data$tree_dbh == -1,]
# Breaking dataset with outliers 'cut' into numerical and categorical
cut.data.num = cut.data[,num_vars]
cut.data.cat = subset(cut.data,select = names(cut.data) %ni% num_vars)

############################################################
## Categorical Exploration
############################################################
# Looking at general structure of categorical variables after imputation - all looks good
old.data.cat[,temp.fact] = lapply(old.data.cat[,temp.fact], factor)
data.cat[,temp.fact] = lapply(data.cat[,temp.fact], factor)

str(data.cat)
str(old.data.cat)

old.data.cat$zipcode[!(old.data.cat$zipcode %in% data.cat$zipcode)]
zipcode = old.data.cat[old.data.cat$zipcode == 0,]

old.data.cat$species[old.data.cat$species %ni% data.cat$species]
############################################################
## Skewness
############################################################
# Transforming categorical variables into integers and calculating skewness
# Comparing raw and cut data
col.name = colnames(cut.data.cat)
cut.data.cat[col.name] = sapply(cut.data.cat[col.name], as.integer)

old.col.name = colnames(old.data.cat)
old.data.cat[old.col.name] = sapply(old.data.cat[old.col.name], as.integer)

skew.old.data.cat = lapply(old.data.cat, skewness)
skew.data.cat = lapply(cut.data.cat, skewness)

skew.old.data.cat$boro_ct = skewness(sort(old.data.cat$boro_ct))
skew.old.data.cat$inf_guard = skewness(sort(old.data.cat$inf_guard))
skew.old.data.cat$sidw_crack = skewness(sort(old.data.cat$sidw_crack))
skew.old.data.cat$zip_city = skewness(sort(old.data.cat$zip_city))
skew.old.data.cat$genus = skewness(sort(old.data.cat$genus))
skew.old.data.cat$species = skewness(sort(old.data.cat$species))

skew.data.cat$boro_ct = skewness(sort(cut.data.cat$boro_ct))
skew.data.cat$inf_guard = skewness(sort(cut.data.cat$inf_guard))
skew.data.cat$sidw_crack = skewness(sort(cut.data.cat$sidw_crack))
skew.data.cat$zip_city = skewness(sort(cut.data.cat$zip_city))
skew.data.cat$genus = skewness(sort(cut.data.cat$genus))
skew.data.cat$species = skewness(sort(cut.data.cat$species))

do.call(rbind, Map(data.frame, Old=skew.old.data.cat, New=skew.data.cat))

############################################################
## Multicollinearity
############################################################
# Creating a correlation plot for transformed categorical variables for possible multicollinearity
# Looking at final data
cor_all = cor(data.trans, use = "pairwise.complete.obs", method = "pearson")
ggcorrplot(cor_all, method = "circle", type = "upper", lab = FALSE)

##################################################################################################################
## Numerical Exploration
##################################################################################################################

############################################################
## Skewness
############################################################
# Looking at the skewness of numerical variables
# raw data and cut data
skew.old.data = lapply(old.data.num, skewness)
skew.data = lapply(cut.data.num, skewness)
skew.old.data$unemployment = skewness(sort(old.data.num$unemployment))
skew.old.data$laborforce = skewness(sort(old.data.num$laborforce))
skew.data$unemployment = skewness(sort(cut.data.num$unemployment))
skew.data$laborforce = skewness(sort(cut.data.num$laborforce))

do.call(rbind, Map(data.frame, Old=skew.old.data, New=skew.data))


############################################################
## Modality
############################################################
# Looking at modlity of numerical variables
# raw data and cut data
old.mod.data = lapply(subset(old.data.num, select = -c(unemployment, laborforce)), dip)
old.mod.data$unemployment = dip(sort(old.data.num$unemployment))
old.mod.data$laborforce = dip(sort(old.data.num$laborforce))

mod.data = lapply(cut.data.num, dip)
unemployment = sort(old.data.num$unemployment)
laborforce = sort(old.data.num$laborforce)
old.mod.data$unemployment = dip(unemployment)
old.mod.data$laborforce = dip(laborforce)

do.call(rbind, Map(data.frame, Old=old.mod.data, New=mod.data))

mod.test.data = lapply(data.num, dip.test)



##################################################################################################################
## After Imputation
##################################################################################################################
# Looking at skewness and modality for imputed data
# Comparing final data to cut data

skew.new = lapply(data.num, skewness)

do.call(rbind, Map(data.frame, Old=skew.data, New=skew.new))


mod.new = lapply(data.num, dip)

do.call(rbind, Map(data.frame, Old=mod.data, New=mod.new))

# Calculating skewness for categorical below
data.trans = data.cat
col.name = colnames(data.trans)
data.trans[col.name] = sapply(data.trans[col.name], as.integer)

skew.data.trans = lapply(data.trans, skewness)
skew.data.trans
############################################################
## Looking at statistics for categorical
############################################################

#recode variable types as factors

data.cat$boro_ct <- as.factor(data.cat$boro_ct)
data.cat$borocode <- as.factor(data.cat$borocode)
data.cat$st_assem <- as.factor(data.cat$st_assem)
data.cat$st_senate <- as.factor(data.cat$st_senate)
data.cat$year <- as.factor(data.cat$year)
data.cat$cncldist <- as.factor(data.cat$cncldist)
data.cat$zipcode <- as.factor(data.cat$zipcode)


old.data.cat$old_boro_ct <- as.factor(old.data.cat$boro_ct)
old.data.cat$old_borocode <- as.factor(old.data.cat$borocode)
old.data.cat$old_st_assem <- as.factor(old.data.cat$st_assem)
old.data.cat$old_st_senate <- as.factor(old.data.cat$st_senate)
old.data.cat$old_year <- as.factor(old.data.cat$year)
old.data.cat$old_cncldist <- as.factor(old.data.cat$cncldist)
old.data.cat$old_zipcode <- as.factor(old.data.cat$zipcode)



#descriptive statistics
for (i in 1:ncol(data.cat)){
  x <- data.cat[,i]
  print(paste(colnames(data.cat[i]), ": Unique values = ", length(unique(na.omit(x)))))
}

temp.dat = data.cat[,c("nta_name","boro_ct")]
for (i in 1:ncol(temp.dat)){
  x <- data.cat[,i]
  print(colnames(data.cat[i]))
  print(freq(x))
}

############################################################
## Adding original NTA_Names back to dataset
############################################################
nta = read.csv("nynta.csv") # https://data.cityofnewyork.us/City-Government/NTA-map/d3qk-pfyz

# Getting relevant columns from NTA dataset
nta = nta[,c("NTACode","NTAName")]

# Merging our main dataset to the subsetted NTA dataset to get the original NTA names back
data = merge(x=data, y=nta, by.x="nta", by.y="NTACode", all.x=T)
