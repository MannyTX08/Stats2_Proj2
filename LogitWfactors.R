##########
# 
##########

# split the training data into a secondary test (not Kaggle)
set.seed(200) # set seed so that same sample can be reproduced in future

# now selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n=nrow(train1), size=floor(.75*nrow(train1)), replace=FALSE)

# subset the data using the sample integer vector created above
train3 <- train1[ !(names(train1) %in% c("Age","SibSp", "Parch", "Embarked"))]
test3  <- test1[ !(names(test1) %in% c("Age","SibSp", "Parch", "Embarked"))]
str(train3)
str(test3)

# Logistic regression model
TitanicLog2 = glm(Survived ~ ., data = train3, family = binomial(link='logit'))
summary(TitanicLog2)

c=train1$Survived
p=predict(TitanicLog2, newdata=test3, type='response')
ACkaggle<-cbind.data.frame("PassengerID"=test$PassengerId, "Survived"=round(p))
write.csv(ACkaggle, file="~/ACKaggle.csv", row.names = FALSE)
##  best so far: 0.7790


cv=cv.glm(data=train3, glmfit=TitanicLog2, K=5)
#ls(cv)
cv["delta"]


################################################################

titanic_logis<-function(incvars='.',excvars='',mnames){
  
  train3 <- train1[ !(names(train1) %in% excvars)]
  test3  <- test1[ !(names(test1) %in% exvars)]
}

################################################################

set.seed(200) # set seed so that same sample can be reproduced in future

#  Note, comment some of these out so that the model is not overspecified
str(train1)
train1$Male = (train1$Sex == "male")*1
train1$Female = (train1$Sex == "female") * 1
train1$Class1 = (train1$Pclass == "1")*1
train1$Class2 = (train1$Pclass == "2")*1
train1$Class3 = (train1$Pclass == "3")*1
train1$PortC = (train1$Embarked == "C")*1
train1$PortQ = (train1$Embarked == "Q")*1
train1$PortS = (train1$Embarked == "S")*1
train1$Child = (train1$AgeBin == "6 or less")*1
train1$Adult= (train1$AgeBin == "(7 - 63]")*1
train1$Senior = (train1$AgeBin == "Over 63")*1
#lapply(levels(train1$Title), function(x) paste("train1", x, sep="$"))
train1$Master = (train1$Title == "Master")*1
train1$Miss  = (train1$Title == "Miss")*1
train1$Mr    = (train1$Title == "Mr")*1
train1$Mrs   = (train1$Title == "Mrs")*1
train1$uncommon= (train1$Title == "uncommon")*1

table(	train1$Male, train1$Sex )
table(	train1$Female, train1$Sex )
table(	train1$Class1, train1$Pclass)
table(	train1$Class2, train1$Pclass )
table(	train1$Class3, train1$Pclass 	)
table(	train1$PortC, train1$Embarked 	)
table(	train1$PortQ, train1$Embarked 	)
table(	train1$PortS, train1$Embarked 	)
table(	train1$Child, train1$AgeBin	)
table(	train1$Senior, train1$AgeBin 	)
table(	train1$Adult, train1$AgeBin 	)
  table(	train1$Master, train1$Title )
  table(	train1$Miss, train1$Title 	)
  table(	train1$Mr, train1$Title )
  table(	train1$Mrs, train1$Title )
  table(	train1$uncommon, train1$Title	)
  
  
test1$Male = (test1$Sex == "male")*1
test1$Female = (test1$Sex == "female") * 1
test1$Class1 = (test1$Pclass == "1")*1
test1$Class2 = (test1$Pclass == "2")*1
test1$Class3 = (test1$Pclass == "3")*1
test1$PortC = (test1$Embarked == "C")*1
test1$PortQ = (test1$Embarked == "Q")*1
test1$PortS = (test1$Embarked == "S")*1
test1$Child = (test1$AgeBin == "6 or less")*1
test1$Adult= (test1$AgeBin == "(7 - 63]")*1
test1$Senior = (test1$AgeBin == "Over 63")*1
#lapply(levels(test1$Title), function(x) paste("test1", x, sep="$"))
test1$Master = (test1$Title == "Master")*1
test1$Miss  = (test1$Title == "Miss")*1
test1$Mr    = (test1$Title == "Mr")*1
test1$Mrs   = (test1$Title == "Mrs")*1
test1$uncommon= (test1$Title == "uncommon")*1
  
  
  Xy<-cbind.data.frame( train1[, !(names(train1) %in% c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived"))],
                        "Survived"=train1[, "Survived"]  )
#  testglm<-bestglm(Xy, family = binomial(link='logit'), IC="CV")
  X<-train1[, !(names(train1) %in% c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived"))]
  y<-train1[, "Survived"]
#  testglm2<-glmnet(x=as.matrix(X), y=as.factor(y), family="binomial")
# ls(testglm2)  
# summary(testglm2)
# testglm2$beta
testglm3<-cv.glmnet(x=as.matrix(X), y, type.measure = "mse")

# testglm4<-glmnet(x=as.matrix(X), y=as.factor(y), family="binomial", lambda=testglm3$lambda)
# ls(testglm4)  
# summary(testglm4)
plot(testglm3)

# testglm4$df
coef(testglm3, s="lambda.min")
# testglm4$lambda
T1<-test1[, !(names(test1) %in% c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived"))]
p3<-predict(testglm3, as.matrix(T1), s="lambda.min")
str(X)
str(T1)
str(p3)

ACkaggle<-cbind.data.frame("PassengerID"=test$PassengerId, "Survived"=round(p3))
names(ACkaggle)<-c("PassengerID", "Survived")
write.csv(ACkaggle, file="~/ACKaggle2.csv", row.names = FALSE)
## Kaggle score of 0.78947


