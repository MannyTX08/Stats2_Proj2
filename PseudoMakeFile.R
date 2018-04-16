# Load necessary packages and ensure they are active
load.lib = c("randomForest","ggplot2","ggthemes","mice","scales","dplyr","Amelia","ROCR", "boot", "bestglm")

install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib){
  install.packages(lib,dependences=TRUE)
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


#INPUT:  train2     CLEAN DATA STORED IN WORKSPACE, ALL NUMERIC
#INPUT:  test2      CLEAN DATA STORED IN WORKSPACE, ALL NUMERIC
source('KaggelSubmission.R', echo=TRUE)
#OUTPUT: TitanicLogKaggel     CLEAN DATA STORED IN WORKSPACE, KAGGEL SUBMISSION
#OUTPUT: titanic_output.csv   OUTPUT FILE FOR KAGGEL SUBMISSION

#INPUT:  train2     CLEAN DATA STORED IN WORKSPACE, ALL NUMERIC
#INPUT:  test2      CLEAN DATA STORED IN WORKSPACE, ALL NUMERIC
source('TitanicR.R', echo=TRUE)
#OUTPUT: train3     CLEAN DATA STORED IN WORKSPACE, ALL NUMERIC
#OUTPUT: test3      CLEAN DATA STORED IN WORKSPACE, ALL NUMERIC
#OUTPUT: GLM models MODELS FOR FULL, REDUCED, and LASSO