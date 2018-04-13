# Load necessary packages and ensure they are active
load.lib = c("randomForest","ggplot2","ggthemes","mice","scales","dplyr","Amelia","ROCR", "boot", "bestglm")

install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib){
  install.packages(lib,dependences=TRUE)
} 

sapply(load.lib,require,character=TRUE)


###### IMPORT DATA / CLEAN DATA / IMPUTE VARIABLES / CREATE NEW VARIABLES 

#INPUT: train.csv   RAW DATA FROM KAGGLE
#INPUT: test.csv    RAW DATA FROM KAGGLE
source('~/Stats2_Proj2/BaseVars.R', echo=TRUE)
#OUTPUT: train      RAW DATA LOADED IN WORKSPACE
#OUTPUT: test       RAW DATA LOADED IN WORKSPACE
#OUTPUT: train1     CELAN DATA STORED IN WORKSPACE
#OUTPUT: test1      CLEAN DATA STORED IN WORKSPACE
#OUTPUT: train2     CELAN DATA STORED IN WORKSPACE, ALL NUMERIC
#OUTPUT: test2      CLEAN DATA STORED IN WORKSPACE, ALL NUMERIC
