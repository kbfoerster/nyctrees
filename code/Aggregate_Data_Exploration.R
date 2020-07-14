#Aggregated data set - data exploration

####Correlation####
agg_data <- read.csv(here("data","Joined_aggregate_data.csv"))
air_data <- read.csv(here("data","Joined_aggregate_air_data.csv"))
water_data <- read.csv(here("data","Joined_aggregate_water_data.csv"))

library(corrplot)
library(RColorBrewer)
#devtools::install_github("laresbernardo/lares") #https://www.rdocumentation.org/packages/lares/versions/4.7
library(lares)

nums <- unlist(lapply(agg_data, is.numeric))  
M<-cor(agg_data[ , nums], use = "pairwise.complete.obs")
corrplot(M, method="circle", tl.col = "black", tl.srt = 45) 
corrplot(M, order = "hclust", addrect = 2, col = terrain.colors(100))

#top correlations in agg data
corr_cross(agg_data, 
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 50 # display top 10 couples of variables (by correlation coefficient)
)

#top correlations with tree density
corr_var(agg_data, 
         Trees_x_sq_mi, 
         top = 25 
) 

#top correlations with water - chlorine
corr_var(water_data, 
         Residual.Free.Chlorine..mg.L., 
         top = 25 
) 

#top correlations with water - chlorine
corr_var(water_data, 
         Turbidity..NTU., 
         top = 25 
) 

#top correlations with air - NO2
corr_var(air_data, 
         Daily.Max.1.hour.NO2.Concentration, 
         top = 25 
) 

agg_data$Hosp_5.17_Avg_Yr_Rate_per_10K
####CART####
library(rpart)
library(rpart.plot)

cart_1 <- rpart(Trees_x_sq_mi ~ SelfEmployed + Hosp_5.17_Avg_Yr_Rate_per_10K + Construction + Production + Transit + IncomePerCap + Professional+ PrivateWork,
                data = agg_data, method = "anova", control = rpart.control(minsplit = 3, xval = 0, cp = 0.05))
rpart.plot(cart_1, yesno=2, under = TRUE, box.palette = "Blues", branch.lty = 3, shadow.col = "gray")
summary(cart_1)
rsq.rpart(cart_1) 

cart_1_Predictions= predict(cart_1, agg_data, type = "matrix")
confusion_matrix_CART_1 <- table(predicted = cart_1_Predictions, actual = agg_data$Trees_x_sq_mi)
confusion_matrix_CART_1

####Clustering####
library(cluster)
library(factoextra)

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

#nums <- unlist(lapply(agg_data, is.numeric))  
#data_num <- agg_data[ , nums]
data_num <- agg_data[, c("Trees_x_sq_mi", "SelfEmployed", "MeanCommute", "Drive", "Carpool", "IncomePerCap", "PrivateWork")]
data_num[complete.cases(data_num), ]
data_num <- na.omit(data_num)
data_num <- scale(data_num)
str(data_num)

seg.k <- kmeans(data_num, centers=4, iter.max = 10)
seg.summ(data_num, seg.k$cluster)
p3 <- fviz_cluster(seg.k, geom = "point",  data = data_num) + ggtitle("k = 4")
