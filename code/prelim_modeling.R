library(reshape2)
library(ggplot2)
library(randomForest)
library(caret)
library(Boruta)
library(here)

data = read.csv("data.csv")
colnames(data)
data[,c("X","income","education","population","unemployment","popDensity","laborforce","poverty","diversity","x_sp","y_sp","zip_city","cncldist","borocode")] = NULL


#### Correlation ####
data_num = data[,unlist(lapply(data, is.numeric))]
data_cor = cor(data_num, use="complete.obs")
data_cor = cor(data, use="complete.obs")

# Get upper triangle of the correlation matrix
# Function found here http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
get_upper_tri = function(df){
  df[lower.tri(df)]<- NA
  return(df)
}

upper = get_upper_tri(data_cor)


melted = melt(upper, na.rm = F)
ggplot(data = melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90))+
  coord_fixed()


#### Random Forest ####
testing = data[complete.cases(data),]
testing[,c("ntaname_full","nta_name","nta","genus","species")] = NULL
testing$healthstatus = droplevels(testing$healthstatus)
#rf = randomForest(healthstatus ~., testing)

partition <- createDataPartition(testing$healthstatus, p = .5, list = FALSE, times = 1)
train = testing[partition,]
test = testing[-partition,]
rf = randomForest(healthstatus ~., train)

bor = Boruta(healthstatus ~., train, doTrace = 2)

plot(bor, xlab = "", xaxt = "n", main = "NYCTrees Data Feature Importance")

lz<-lapply(1:ncol(bor$ImpHistory),function(i)
  
  bor$ImpHistory[is.finite(bor$ImpHistory[,i]),i])

names(lz) <- colnames(bor$ImpHistory)

Labels <- sort(sapply(lz,median))

axis(side = 1,las=2,labels = names(Labels),
     
     at = 1:ncol(bor$ImpHistory), cex.axis = 0.7)


#### Agg Boruta ####
agg_data = read.csv("Joined_aggregate_data.csv")
agg_train = agg_data[complete.cases(agg_data),]
agg_bor = Boruta(Num_Trees ~., agg_train, doTrace = 2)
agg_bor = TentativeRoughFix(agg_bor)
plot(agg_bor, xlab = "", xaxt = "n", main = "Aggregated Data Feature Importance")

lz<-lapply(1:ncol(agg_bor$ImpHistory),function(i)
  
  agg_bor$ImpHistory[is.finite(agg_bor$ImpHistory[,i]),i])

names(lz) <- colnames(agg_bor$ImpHistory)

Labels <- sort(sapply(lz,median))

axis(side = 1,las=2,labels = names(Labels),
     
     at = 1:ncol(agg_bor$ImpHistory), cex.axis = 0.7)


agg_income = Boruta(Income ~., agg_train, doTrace = 2)
agg_income = TentativeRoughFix(agg_income)

plot(agg_income, xlab = "", xaxt = "n", main = "Aggregated Data Income Feature Importance")

lz<-lapply(1:ncol(agg_income$ImpHistory),function(i)
  
  agg_income$ImpHistory[is.finite(agg_income$ImpHistory[,i]),i])

names(lz) <- colnames(agg_income$ImpHistory)

Labels <- sort(sapply(lz,median))

axis(side = 1,las=2,labels = names(Labels),
     
     at = 1:ncol(agg_income$ImpHistory), cex.axis = 0.7)