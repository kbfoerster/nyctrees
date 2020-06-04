############################################################
## Combining Kaggle Census Data
############################################################
library(sqldf)

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


# Creating an ID column to temporarily track columns easily from pre-merge to post-merge
tempcensus = censusdata
tempcensus$ID <- seq.int(nrow(tempcensus))

# SQLDF to do an inner join on datasets and left join to verify differences
innertest = sqldf("select * from tempcensus df1 inner join geotrans df2
             on (df1.Latitude >= df2.latmin and df1.Latitude <= df2.latmax and 
             df1.Longitude >= df2.longmin and df1.Longitude <= df2.longmax) ")

lefttest = sqldf("select * from tempcensus df1 left join geotrans df2
             on (df1.Latitude >= df2.latmin and df1.Latitude <= df2.latmax and 
             df1.Longitude >= df2.longmin and df1.Longitude <= df2.longmax) ")

# Can see the NAs are inconsistent between pre- and post- merge
sapply(tempcensus, function(x) sum(is.na(x)))
sapply(innertest, function(x) sum(is.na(x)))
sapply(lefttest, function(x) sum(is.na(x)))
# Looking at a single entry, it's matched to multiple NTA Names and gets an additional entry
innertest[innertest$ID == 8408,]
lefttest[lefttest$ID == 8408,]

# Can see that the inner join has less of the original entries than the left-join
# Missing out on possible data by doing inner join
summary(tempcensus$ID)
summary(innertest$ID)
summary(lefttest$ID)

####### To-Do
#### Find reliable way to remove duplicates

##### Failed Partition and Merge

# partition <- createDataPartition(data$boroname, p = .5, list = FALSE, times = 1)
# dataA = data[partition,]
# dataB = data[-partition,]
# 
# partitionA = createDataPartition(dataA$boroname, p = .5, list = FALSE, times = 1)
# dataA_1 = dataA[partitionA,]
# dataA_2 = dataA[-partitionA,]
# partitionB = createDataPartition(dataB$boroname, p = .5, list = FALSE, times = 1)
# dataB_1 = dataB[partitionB,]
# dataB_2 = dataB[-partitionB,]
# 
# partitionA_1 = createDataPartition(dataA_1$boroname, p = .5, list = FALSE, times = 1)
# partitionA_2 = createDataPartition(dataA_2$boroname, p = .5, list = FALSE, times = 1)
# partitionB_1 = createDataPartition(dataB_1$boroname, p = .5, list = FALSE, times = 1)
# partitionB_2 = createDataPartition(dataB_2$boroname, p = .5, list = FALSE, times = 1)
# dataA_1_1 = dataA_1[partitionA_1,]
# dataA_1_2 = dataA_1[-partitionA_1,]
# dataA_2_1 = dataA_2[partitionA_2,]
# dataA_2_2 = dataA_2[-partitionA_2,]
# dataB_1_1 = dataB_1[partitionB_1,]
# dataB_1_2 = dataB_1[-partitionB_1,]
# dataB_2_1 = dataB_2[partitionB_2,]
# dataB_2_2 = dataB_2[-partitionB_2,]
# 
# 
# 
# partitionA_1_1 = createDataPartition(dataA_1_1$boroname, p = .5, list = FALSE, times = 1)
# partitionA_1_2 = createDataPartition(dataA_1_2$boroname, p = .5, list = FALSE, times = 1)
# partitionA_2_1 = createDataPartition(dataA_2_1$boroname, p = .5, list = FALSE, times = 1)
# partitionA_2_2 = createDataPartition(dataA_2_2$boroname, p = .5, list = FALSE, times = 1)
# partitionB_1_1 = createDataPartition(dataB_1_1$boroname, p = .5, list = FALSE, times = 1)
# partitionB_1_2 = createDataPartition(dataB_1_2$boroname, p = .5, list = FALSE, times = 1)
# partitionB_2_1 = createDataPartition(dataB_2_1$boroname, p = .5, list = FALSE, times = 1)
# partitionB_2_2 = createDataPartition(dataB_2_2$boroname, p = .5, list = FALSE, times = 1)
# dataA_1_1_1 = dataA_1_1[partitionA_1_1,]
# dataA_1_1_2 = dataA_1_1[-partitionA_1_1,]
# dataA_1_2_1 = dataA_1_2[partitionA_1_2,]
# dataA_1_2_2 = dataA_1_2[-partitionA_1_2,]
# dataA_2_1_1 = dataA_2_1[partitionA_1_1,]
# dataA_2_1_2 = dataA_2_1[-partitionA_1_1,]
# dataA_2_2_1 = dataA_2_2[partitionA_1_1,]
# dataA_2_2_2 = dataA_2_2[-partitionA_1_1,]
# dataB_1_1_1 = dataB_1_1[partitionB_1_1,]
# dataB_1_1_2 = dataB_1_1[-partitionB_1_1,]
# dataB_1_2_1 = dataB_1_2[partitionB_1_2,]
# dataB_1_2_2 = dataB_1_2[-partitionB_1_2,]
# dataB_2_1_1 = dataB_2_1[partitionB_1_1,]
# dataB_2_1_2 = dataB_2_1[-partitionB_1_1,]
# dataB_2_2_1 = dataB_2_2[partitionB_1_1,]
# dataB_2_2_2 = dataB_2_2[-partitionB_1_1,]
# 
# partitionA_1_1_1 = createDataPartition(dataA_1_1_1$boroname, p = .5, list = FALSE, times = 1)
# partitionA_1_1_2 = createDataPartition(dataA_1_1_2$boroname, p = .5, list = FALSE, times = 1)
# partitionA_1_2_1 = createDataPartition(dataA_1_2_1$boroname, p = .5, list = FALSE, times = 1)
# partitionA_1_2_2 = createDataPartition(dataA_1_2_2$boroname, p = .5, list = FALSE, times = 1)
# partitionA_2_1_1 = createDataPartition(dataA_2_1_1$boroname, p = .5, list = FALSE, times = 1)
# partitionA_2_1_2 = createDataPartition(dataA_2_1_2$boroname, p = .5, list = FALSE, times = 1)
# partitionA_2_2_1 = createDataPartition(dataA_2_2_1$boroname, p = .5, list = FALSE, times = 1)
# partitionA_2_2_2 = createDataPartition(dataA_2_2_2$boroname, p = .5, list = FALSE, times = 1)
# partitionB_1_1_1 = createDataPartition(dataB_1_1_1$boroname, p = .5, list = FALSE, times = 1)
# partitionB_1_1_2 = createDataPartition(dataB_1_1_2$boroname, p = .5, list = FALSE, times = 1)
# partitionB_1_2_1 = createDataPartition(dataB_1_2_1$boroname, p = .5, list = FALSE, times = 1)
# partitionB_1_2_2 = createDataPartition(dataB_1_2_2$boroname, p = .5, list = FALSE, times = 1)
# partitionB_2_1_1 = createDataPartition(dataB_2_1_1$boroname, p = .5, list = FALSE, times = 1)
# partitionB_2_1_2 = createDataPartition(dataB_2_1_2$boroname, p = .5, list = FALSE, times = 1)
# partitionB_2_2_1 = createDataPartition(dataB_2_2_1$boroname, p = .5, list = FALSE, times = 1)
# partitionB_2_2_2 = createDataPartition(dataB_2_2_2$boroname, p = .5, list = FALSE, times = 1)
# dataA_1_1_1_1 = dataA_1_1_1[partitionA_1_1_1,]
# dataA_1_1_1_2 = dataA_1_1_1[-partitionA_1_1_1,]
# dataA_1_1_2_1 = dataA_1_1_2[partitionA_1_1_2,]
# dataA_1_1_2_2 = dataA_1_1_2[-partitionA_1_1_2,]
# dataA_1_2_1_1 = dataA_1_2_1[partitionA_1_2_1,]
# dataA_1_2_1_2 = dataA_1_2_1[-partitionA_1_2_1,]
# dataA_1_2_2_1 = dataA_1_2_2[partitionA_1_2_2,]
# dataA_1_2_2_2 = dataA_1_2_2[-partitionA_1_2_2,]
# dataA_2_1_1_1 = dataA_2_1_1[partitionA_2_1_1,]
# dataA_2_1_1_2 = dataA_2_1_1[-partitionA_2_1_1,]
# dataA_2_1_2_1 = dataA_2_1_2[partitionA_2_1_2,]
# dataA_2_1_2_2 = dataA_2_1_2[-partitionA_2_1_2,]
# dataA_2_2_1_1 = dataA_2_2_1[partitionA_2_2_1,]
# dataA_2_2_1_2 = dataA_2_2_1[-partitionA_2_2_1,]
# dataA_2_2_2_1 = dataA_2_2_2[partitionA_2_2_2,]
# dataA_2_2_2_2 = dataA_2_2_2[-partitionA_2_2_2,]
# dataB_1_1_1_1 = dataB_1_1_1[partitionB_1_1_1,]
# dataB_1_1_1_2 = dataB_1_1_1[-partitionB_1_1_1,]
# dataB_1_1_2_1 = dataB_1_1_2[partitionB_1_1_2,]
# dataB_1_1_2_2 = dataB_1_1_2[-partitionB_1_1_2,]
# dataB_1_2_1_1 = dataB_1_2_1[partitionB_1_2_1,]
# dataB_1_2_1_2 = dataB_1_2_1[-partitionB_1_2_1,]
# dataB_1_2_2_1 = dataB_1_2_2[partitionB_1_2_2,]
# dataB_1_2_2_2 = dataB_1_2_2[-partitionB_1_2_2,]
# dataB_2_1_1_1 = dataB_2_1_1[partitionB_2_1_1,]
# dataB_2_1_1_2 = dataB_2_1_1[-partitionB_2_1_1,]
# dataB_2_1_2_1 = dataB_2_1_2[partitionB_2_1_2,]
# dataB_2_1_2_2 = dataB_2_1_2[-partitionB_2_1_2,]
# dataB_2_2_1_1 = dataB_2_2_1[partitionB_2_2_1,]
# dataB_2_2_1_2 = dataB_2_2_1[-partitionB_2_2_1,]
# dataB_2_2_2_1 = dataB_2_2_2[partitionB_2_2_2,]
# dataB_2_2_2_2 = dataB_2_2_2[-partitionB_2_2_2,]
# 
# 
# 
# temp = merge(x=censusdata, y=dataA_1_1_1_1, by.x="Borough", by.y="boroname", all.y=T)
