##########
# 
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


# matrix of the p-value of the correlation
# p.mat <- cor.mtest(train3[, !(names(train3) %in% c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Age", "Survived"))])
# corMatrix = cor(train3[, !(names(train3) %in% c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Age", "Survived", "SibSp", "Parch", "Sex", "Female", "Miss"))])
# 
# corrplot(corMatrix, method="color",
#          type="upper", order="hclust",
#          addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45, #Text label color and rotation
#          p.mat = p.mat, sig.level = 0.01, insig = "blank",
#          diag=FALSE
# )
# 

# # easier to define what may not be in the model
notvars=c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "Parch") #original
         # "Family", "Sex", "Female", "Miss")  ##These to test multicolinearity
         # Better scores from Family + SibSp than other combinations
  
#Some glm functions work better with matrices
  Xy<-cbind.data.frame( train3[, !(names(train3) %in% notvars)],
                        "Survived"=train3[, "Survived"]  )
  
#  testglm<-bestglm(Xy, family = binomial(link='logit'), IC="CV")
## best subset above take a long time to run
  
  X<-train3[, !(names(train3) %in% notvars)]
  y<-train3[, "Survived"]
  
  yy<-as.factor(y)
  
  M<-model.matrix(Survived~., data=Xy)

testglm3<-cv.glmnet(x=M, y, 
                   # type.measure = "auc", 
                    family="binomial")
plot(testglm3, main="MSE reduction curve")


coef(testglm3, s="lambda.min")
min(testglm3$cvm)

#  Exclude old columns 
T1<-test3[, !(names(test3) %in% notvars)]
p3<-inv.logit(predict(testglm3, as.matrix(T1), s="lambda.min"))
str(X)
str(T1)
str(p3)

ACkaggle<-cbind.data.frame("PassengerID"=test$PassengerId, "Survived"=round(p3))
names(ACkaggle)<-c("PassengerID", "Survived")
write.csv(ACkaggle, file="~/ACKaggle2.csv", row.names = FALSE)
## Kaggle score of 0.78947


T1<-test3[, !(names(test3) %in% notvars)]
p3<-inv.logit(predict(testglm3, as.matrix(T1), s="lambda.1se"))
str(X)
str(T1)
str(p3)

ACkaggle<-cbind.data.frame("PassengerID"=test$PassengerId, "Survived"=round(p3))
names(ACkaggle)<-c("PassengerID", "Survived")
write.csv(ACkaggle, file="~/ACKaggle3.csv", row.names = FALSE)
## Kaggle score of 0.77990


###### Create Function
###### Assumes train3

runcvglms<-function(notvars="", Name="KaggleSubmit", nfold=10){
  
#Some glm functions work better with matrices
Xy<-cbind.data.frame( train3[, !(names(train3) %in% notvars)],
                      "Survived"=train3[, "Survived"]  )

#  testglm<-bestglm(Xy, family = binomial(link='logit'), IC="CV")
## best subset above take a long time to run

X<-train3[, !(names(train3) %in% notvars)]
y<-train3[, "Survived"]

yy<-as.factor(y)

M<-model.matrix(Survived~., data=Xy)

testglm3<-cv.glmnet(x=as.matrix(X), y, type.measure = "mse", family="binomial")
plot(testglm3, main="MSE reduction curve")


print(coef(testglm3, s="lambda.min"))
print(min(testglm3$cvm))

#  Exclude old columns 
T1<-test3[, !(names(test3) %in% notvars)]
p3<-predict(testglm3, as.matrix(T1), s="lambda.min", type="response")
# str(X)
# str(T1)
# str(p3)

ACkaggle<-cbind.data.frame("PassengerID"=test$PassengerId, "Survived"=round(p3))
names(ACkaggle)<-c("PassengerID", "Survived")
write.csv(ACkaggle, file=paste("~/", Name, ".csv", sep=''), row.names = FALSE)
## Kaggle score of 0.78947

}


runcvglms(c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "Parch"), "ACKaggle_old", nfold=5)
runcvglms(c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "SibSp"), "ACKaggle_old2", nfold=5)
runcvglms(c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "Family"), "ACKaggle_old3", nfold=5)

