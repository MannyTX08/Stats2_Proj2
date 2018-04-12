# Load necessary packages and ensure they are active
load.lib = c("randomForest","ggplot2","ggthemes","mice","scales","dplyr","Amelia","ROCR")

install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib){
  install.packages(lib,dependences=TRUE)
} 

sapply(load.lib,require,character=TRUE)


###### IMPORT DATA / CLEAN DATA / IMPUTE VARIABLES / CREATE NEW VARIABLES 

#INPUT: train.csv   RAW DATA FROM KAGGLE
#INPUT: test.csv    RAW DATA FROM KAGGLE
source('~/stats2_Proj2/BaseVars.R', echo=TRUE)
#OUTPUT: train      RAW DATA LOADED IN WORKSPACE
#OUTPUT: test       RAW DATA LOADED IN WORKSPACE
#OUTPUT: train2     CELAN DATA STORED IN WORKSPACE
#OUTPUT: test2      CLEAN DATA STORED IN WORKSPACE