#Asthma data

#Note- we could also merge just on the raw NTA neighborhood (~186 NTAs) from the tree data - seems to be the same level of granuarlity in this raw data

rm(list=ls())

library(readxl)

asthma = read_excel("Asthma_data.xlsx")
ntatranslation = read_excel("NTA_translation_2_Ashtma.xlsx")

for (i in 1:nrow(ntatranslation)){
  asthma$Geography[asthma$Geography == ntatranslation$Raw_NTA[i]] = ntatranslation$Small_NTA[i]
} 

write.csv(asthma, "asthma_by_NTA.csv")
