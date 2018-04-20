# Load necessary packages and ensure they are active
load.lib = c("randomForest","ggplot2","ggthemes","mice","scales","dplyr","Amelia","ROCR","glmnet","boot","bestglm","corrplot")

install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib){
  install.packages(lib,dependencies=TRUE)
} 

suppressMessages(sapply(load.lib,require,character=TRUE))


###### IMPORT DATA / CLEAN DATA / IMPUTE VARIABLES / CREATE NEW VARIABLES 

#INPUT: train.csv   RAW DATA FROM KAGGLE
#INPUT: test.csv    RAW DATA FROM KAGGLE
source('BaseVars.R', echo=TRUE)
#OUTPUT: train      RAW DATA LOADED IN WORKSPACE
#OUTPUT: test       RAW DATA LOADED IN WORKSPACE
#OUTPUT: train1     CLEAN DATA STORED IN WORKSPACE
#OUTPUT: test1      CLEAN DATA STORED IN WORKSPACE
#OUTPUT: train2     CLEAN DATA STORED IN WORKSPACE, ALL NUMERIC
#OUTPUT: test2      CLEAN DATA STORED IN WORKSPACE, ALL NUMERIC

#INPUT:  train1     CLEAN DATA STORED IN WORKSPACE
#INPUT:  test1      CLEAN DATA STORED IN WORKSPACE
#INPUT:  train2     CLEAN DATA STORED IN WORKSPACE, ALL NUMERIC
#INPUT:  test2      CLEAN DATA STORED IN WORKSPACE, ALL NUMERIC
#source('TitanicR.R', echo=TRUE)
source('TitanicR2.R', echo=TRUE)
#OUTPUT: train3     CLEAN DATA STORED IN WORKSPACE, 80% SAMPLE
#OUTPUT: test3      CLEAN DATA STORED IN WORKSPACE, 80% SAMPLE
#OUTPUT: MTXKaggleF.csv  FULL MODEL SUBMISSION TO KAGGLE
#OUTPUT: MTXKaggle.csv   REDUCED MODEL SUBMISSION TO KAGGLE

#INPUT:  train1     CLEAN DATA STORED IN WORKSPACE
#INPUT:  test1      CLEAN DATA STORED IN WORKSPACE
source('ACKaggle.R', echo=TRUE)
#OUTPUT: train3     CLEAN DATA STORED IN WORKSPACE, 100%
#OUTPUT: test3      CLEAN DATA STORED IN WORKSPACE, 100%
#OUTPUT: ACKaggle_class_min.csv  SUBMISSION TO KAGGLE
#OUTPUT: ACKaggle_class_1se.csv  SUBMISSION TO KAGGLE