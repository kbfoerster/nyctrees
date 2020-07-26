# NYC Tree Census Project
Data collection, cleaning, exploration, and analysis of NYC Tree Censuses (2015) combined with NYC census, water, air pollution, and asthma hospitalization data. 

## Outline

### Data Collection and Cleaning
- [Tree census data](Trees.R)
- [Tree Density metrics](code/Tree_Density_by_NTA.ipynb)
- [Asthma Hospitalization data](code/Asthma_data.R)
- [Water quality data](code/Water_data.R)
- [Creating aggregate data set by NTA level](code/Merge_to_Aggregate_Data.R)

### Data Exploration
- [Feature Importance via PCA and Boruta](code/PCA_Boruta_Exploration.ipynb)
- [Aggregate Data Exploration: Correlations, Clustering, and Decision Trees](code/Aggregate_Data_Exploration.R)

### Data Analyses
- Supervised methods
- Unsupervised methods

### Data Visualizations

## Data Sources
- Department of Parks and Recreation. (2016, June 2). 2005 Street Tree Census. Retrieved from https://data.cityofnewyork.us/Environment/2005-Street-Tree-Census/29bw-z7pj 
- Department of Parks and Recreation. (2018, September 25). 2015 Street Tree Census - Tree Data. Retrieved from https://data.cityofnewyork.us/Environment/2015-Street-Tree-Census-Tree-Data/pi5s-9p35 
- Department of Health. (2016) Asthma Emergency Department Visits in New York State by Region and County. Retrieved from https://www.health.ny.gov/statistics/ny_asthma/
- Department of Environmental Protection (DEP). (2020) Drinking Water Quality Distribution Monitoring Data. Retieved from https://data.cityofnewyork.us/Environment/Drinking-Water-Quality-Distribution-Monitoring-Dat/bkwf-xfky
- Department of Health and Mental Hygiene (DOHMH. (2020) NYCCAS Air Pollution Rasters. Retrieved from https://data.cityofnewyork.us/Environment/NYCCAS-Air-Pollution-Rasters/q68s-8qxv
- NYC Department of City Planning. (2010) Boundaries of Neighborhood Tabulation Area (NTA). Retrieved from https://data.cityofnewyork.us/City-Government/Neighborhood-Tabulation-Areas-NTA-/cpf4-rkhq
