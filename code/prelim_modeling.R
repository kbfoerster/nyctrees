library(reshape2)
library(ggplot2)
library(randomForest)
library(caret)
library(Boruta)
library(here)

data = read.csv(here("data","data.csv"))
colnames(data)
data[,c("X","income","education","population","unemployment","popDensity","laborforce","poverty","diversity","x_sp","y_sp","zip_city","cncldist","borocode")] = NULL

boruta_plot = function(model, main_title="Boruta Feature Importance Plot",save_plot=F) {
  if(save_plot==T){png(here("plots", paste(main_title,".png", sep="")), width = 465, height = 225, units='mm', res = 300)}
  plot(model, xlab = "", xaxt = "n", main = main_title)
  
  lz<-lapply(1:ncol(model$ImpHistory),function(i)
    
    model$ImpHistory[is.finite(model$ImpHistory[,i]),i])
  
  names(lz) <- colnames(model$ImpHistory)
  
  Labels <- sort(sapply(lz,median))
  
  axis(side = 1,las=2,labels = names(Labels),
       
       at = 1:ncol(model$ImpHistory), cex.axis = 0.7)
  if(save_plot==T){dev.off()}
  return()
}

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


#### Boruta ####
testing = data[complete.cases(data),]
testing[,c("ntaname_full","nta_name","nta","genus","species")] = NULL
testing$healthstatus = droplevels(testing$healthstatus)

partition <- createDataPartition(testing$healthstatus, p = .5, list = FALSE, times = 1)
train = testing[partition,]
test = testing[-partition,]
rf = randomForest(healthstatus ~., train)

bor = Boruta(healthstatus ~., train, doTrace = 2)

boruta_plot(bor, main_title = "NYCTrees Healthstatus Feature Importance", save_plot = T)


### tree_dbh Boruta
bor = Boruta(tree_dbh ~., train, doTrace = 2)
fixed_bor = TentativeRoughFix(bor)
boruta_plot(bor, main_title = "Boruta tree_dbh Feature Importance", save_plot = T)
boruta_plot(fixed_bor, main_title = "Boruta tree_dbh Feature Importance", save_plot = T)

#### Agg Boruta ####
agg_data = read.csv(here("data","Joined_aggregate_data.csv"))
agg_data[c("Borough","X.1","X.x","X.y","X")] = NULL
agg_train = agg_data[complete.cases(agg_data),]

### Looking at number of trees against aggregate variables
agg_bor = Boruta(Num_Trees ~., agg_train, doTrace = 2)
agg_bor = TentativeRoughFix(agg_bor)

boruta_plot(agg_bor, main_title = "Aggregated Data Feature Importance", save_plot = T)

### Looking at Income against aggregate variables
agg_income = Boruta(Income ~., agg_train, doTrace = 2)
agg_income = TentativeRoughFix(agg_income)
boruta_plot(agg_income, main_title = "Aggregated Data Income Feature Importance", save_plot = T)

### Looking at Professional against aggregate variables
agg_prof = Boruta(Professional ~., agg_train, doTrace=2)
agg_prof = TentativeRoughFix(agg_prof)
boruta_plot(agg_prof, main_title = "Aggregated Data Professional Feature Importance", save_plot = T)

#### Water agg_data ####
wtr_data = read.csv(here("data","Joined_aggregate_water_data.csv"))
wtr_data[,c("ID","X...Coordinate.y","Y...Coordinate.y","geometry","Borough","X.1","X","intersection")] = NULL
wtr_data = wtr_data[complete.cases(wtr_data),]

### Looking at Trees_x_sq_mi
trees_x_bor = Boruta(Trees_x_sq_mi ~., wtr_data, doTrace = 2)
trees_x_bor = TentativeRoughFix(trees_x_bor)
boruta_plot(trees_x_bor, main_title = "Water Data Trees_x_sq_mi Feature Importance", save_plot = T)
saveRDS(trees_x_bor, here("models","boruta_trees_x_sq_mi_variable_importance.rds"))

### Looking at Fluoride..mg.L.
fluor_bor = Boruta(Fluoride..mg.L. ~., wtr_data, doTrace = 2)
fluor_bor = TentativeRoughFix(fluor_bor)
boruta_plot(fluor_bor, main_title = "Aggregated Water Data Fluoride Feature Importance", save_plot = T)
saveRDS(fluor_bor, here("models","boruta_fluoride_variable_importance.rds"))

### Looking at Coliform..Quanti.Tray...MPN..100mL.
coli_bor = Boruta(Coliform..Quanti.Tray...MPN..100mL. ~., wtr_data, doTrace=2)
coli_bor = TentativeRoughFix(coli_bor)
boruta_plot(coli_bor, main_title = "Aggregated Water Data Coliform Feature Importance", save_plot = T)
saveRDS(coli_bor, here("models","boruta_coliform_variable_importance.rds"))

### Looking at Coliform..Quanti.Tray...MPN..100mL.
Ecoli_bor = Boruta(E.coli.Quanti.Tray...MPN.100mL. ~., wtr_data, doTrace=2)
Ecoli_bor = TentativeRoughFix(Ecoli_bor)
boruta_plot(Ecoli_bor, main_title = "Aggregated Water Data E.Coli Feature Importance", save_plot = T)
saveRDS(Ecoli_bor, here("models","boruta_e_coli_variable_importance.rds"))

#### PCA ####
library(factoextra)
library(FactoMineR)
### Looking at main data
pca_data = data
pca_data$inf_guard = ifelse(pca_data$inf_guard == "Yes", 1, 0)
pca_data$inf_shoes = ifelse(pca_data$inf_shoes == "Yes", 1, 0)
pca_data$sidw_crack = ifelse(pca_data$sidw_crack == "Yes", 1, 0)
pca_data$healthstatus = ifelse(pca_data$healthstatus == "Good" | pca_data$healthstatus == "Alive", 1, 0)
pca_data = pca_data[,unlist(lapply(pca_data, is.numeric))]
pca_data = pca_data[complete.cases(pca_data),]
### Running PCA on pca_data
pca_data_mod = prcomp(pca_data, scale = TRUE)

eig = (pca_data_mod$sdev)^2
variance = eig*100/sum(eig)
cumvar = cumsum(variance)

eig.dat = data.frame(eig = eig, variance = variance, cumvariance = cumvar)
eig.dat

fviz_pca_var(pca_data_mod, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE      # Avoid text overlapping
)

### Running PCA on wtr_data
pca_wtr_data = wtr_data
pca_wtr_data[,c("Location","Sample.Date","Sample.Site","area","NTA_large","Sample.Time")] = NULL
pca_wtr_data$Residual.Free.Chlorine..mg.L. = as.numeric(pca_wtr_data$Residual.Free.Chlorine..mg.L.)
pca_wtr_data$Turbidity..NTU. = as.numeric(pca_wtr_data$Turbidity..NTU.)
pca_wtr_data$Fluoride..mg.L. = as.numeric(pca_wtr_data$Fluoride..mg.L.)
pca_wtr_data$Coliform..Quanti.Tray...MPN..100mL. = as.numeric(pca_wtr_data$Coliform..Quanti.Tray...MPN..100mL.)
pca_wtr_data$E.coli.Quanti.Tray...MPN.100mL. = as.numeric(pca_wtr_data$E.coli.Quanti.Tray...MPN.100mL.)
pca_wtr_data$Hosp_5.17_Avg_yr_Num = as.numeric(pca_wtr_data$Hosp_5.17_Avg_yr_Num)
pca_wtr_data$Hosp_5.17_Avg_Yr_Rate_per_10K = as.numeric(pca_wtr_data$Hops_Adults_Avg_Yr_Rate_per_10K_adj)

pca_wtr_data_mod = prcomp(pca_wtr_data, scale = TRUE)

eig = (pca_data_mod$sdev)^2
variance = eig*100/sum(eig)
cumvar = cumsum(variance)

wtr_eig.dat = data.frame(eig = eig, variance = variance, cumvariance = cumvar)
wtr_eig.dat

fviz_pca_var(pca_wtr_data_mod, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE      # Avoid text overlapping
)

### Running PCA on general aggregate data
