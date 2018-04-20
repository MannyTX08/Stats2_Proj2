##########
# KAGGLE SUBMISSION GENERATOR
##########

set.seed(200) # set seed so that same sample can be reproduced in future

# use below in case 'glmnet'is not automatically loaded with 'bestglm'
#library(glmnet)

#  Create indicators to allow LASSO selection
#  Note, comment one of each category out so that the model is not overspecified
train3<-train1
test3<-test1

str(train3)
#train3$Male = (train3$Sex == "male")*1
train3$Female = (train3$Sex == "female") * 1
train3$Class1 = (train3$Pclass == "1")*1
#train3$Class2 = (train3$Pclass == "2")*1
train3$Class3 = (train3$Pclass == "3")*1
train3$PortC = (train3$Embarked == "C")*1
#train3$PortQ = (train3$Embarked == "Q")*1
train3$PortS = (train3$Embarked == "S")*1
train3$Child = (train3$AgeBin == "6 or less")*1
#train3$Adult= (train3$AgeBin == "(7 - 63]")*1
train3$Senior = (train3$AgeBin == "Over 63")*1
#lapply(levels(train3$Title), function(x) paste("train3", x, sep="$"))
#train3$Master = (train3$Title == "Master")*1
train3$Miss  = (train3$Title == "Miss")*1
train3$Mr    = (train3$Title == "Mr")*1
train3$Mrs   = (train3$Title == "Mrs")*1
train3$uncommon= (train3$Title == "uncommon")*1

# Now for test  

#test3$Male = (test3$Sex == "male")*1
test3$Female = (test3$Sex == "female") * 1
test3$Class1 = (test3$Pclass == "1")*1
#test3$Class2 = (test3$Pclass == "2")*1
test3$Class3 = (test3$Pclass == "3")*1
test3$PortC = (test3$Embarked == "C")*1
#test3$PortQ = (test3$Embarked == "Q")*1
test3$PortS = (test3$Embarked == "S")*1
test3$Child = (test3$AgeBin == "6 or less")*1
#test3$Adult= (test3$AgeBin == "(7 - 63]")*1
test3$Senior = (test3$AgeBin == "Over 63")*1
#lapply(levels(test3$Title), function(x) paste("test3", x, sep="$"))
#test3$Master = (test3$Title == "Master")*1
test3$Miss  = (test3$Title == "Miss")*1
test3$Mr    = (test3$Title == "Mr")*1
test3$Mrs   = (test3$Title == "Mrs")*1
test3$uncommon= (test3$Title == "uncommon")*1



###### Create Function
###### Assumes train3 and test3 above

runcvglms<-function(notvars="", Name="KaggleSubmit", nfold=10, type='mse', lambda="lambda.min"){
  
  #Some glm functions work better with matrices
  Xy<-cbind.data.frame( train3[, !(names(train3) %in% notvars)],
                        "Survived"=train3[, "Survived"]  )
  
  #  testglm<-bestglm(Xy, family = binomial(link='logit'), IC="CV")
  ## best subset above take a long time to run
  
  X<-train3[, !(names(train3) %in% notvars)]
  y<-train3[, "Survived"]
  
  yy<-as.factor(y)
  
  M<-model.matrix(Survived~., data=Xy)
  
  testglm3<-cv.glmnet(x=as.matrix(X), y, type.measure = type, family="binomial")
  par(mar=c(5, 4, 5, 2) + 0.1)
  plot(testglm3, main="Misclassification Reduction Curve")
  par(mar=c(5, 4, 4, 2) + 0.1)
  
  print(coef(testglm3, s=lambda))
  #print(min(testglm3$cvm))
  cvm<-if(lambda=="lambda.min"){testglm3$cvm[which(testglm3$lambda == testglm3$lambda.min)]}else {testglm3$cvm[which(testglm3$lambda == testglm3$lambda.1se)]}
  print(cvm)
  
  #  Exclude old columns 
  T1<-test3[, !(names(test3) %in% notvars)]
  p3<-predict(testglm3, as.matrix(T1), s=lambda, type="response")
  # str(X)
  # str(T1)
  # str(p3)
  
  ACkaggle<-cbind.data.frame("PassengerID"=test$PassengerId, "Survived"=round(p3))
  names(ACkaggle)<-c("PassengerID", "Survived")
  #write.csv(ACkaggle, file=paste("~/Stats2_Proj2/Data/", Name, ".csv", sep=''), row.names = FALSE)
  ## Kaggle score of 0.78947
  
}


runcvglms(c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "Family"), "ACKaggle_class_min", nfold=10, type='class') # Kaggle Score 0.77990
#runcvglms(c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "Family"), "ACKaggle_mae_min", nfold=10, type='mse') # Kaggle Score 0.77990
runcvglms(c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "Family"), "ACKaggle_class_1se", nfold=10, type='class', lambda="lambda.1se") # Kaggle Score 0.77990
#runcvglms(c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "Family"), "ACKaggle_mse_1se", nfold=10, type='mse', lambda="lambda.1se") # Kaggle Score 0.77990
#runcvglms(c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "Family"), "ACKaggle_auc", nfold=10, type='auc') # Kaggle Score 0.77990

### NOTE: even with a fixed seed, rerunning the same functions will give you different results.
########################## ALWAYS RUN AFTER TIANTICR2 ################################



