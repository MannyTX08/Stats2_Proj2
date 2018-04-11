### HEADING

library('ggplot2')
library('ggthemes')
library('scales')
library('dplyr')
library('tidyr')
library('boot')
library('car')
library('stringr')


getwd()
setwd("C:/Users/acasi/Documents/Project6372/")

train<-read.csv("./titanic_train.csv", header=TRUE, stringsAsFactors = TRUE)
test<-read.csv("./titanic_test.csv", header=TRUE,  stringsAsFactors = TRUE)

#### Exploratory
train<-cbind.data.frame( train[,-2], "Survived"=train$Survived)
str(train)
head(train)

str(test)
head(test)

summ.unique.col <- function(data){
#data=train
print(str(data))
N<-length(data[1,])
for(i in 1:N){
  print(paste("Var", i, sep=''))
  print(names(data)[i])
if(length(unique(data[,i])) < 51){print(unique(data[,i]))} else print(unique(data[,i])[1:50])
if(length(unique(data[,i])) < 51){print(summary(data[,i]))} else print(summary(data[,i])[1:50])
  print(cat("\n"))
                            }
                            }
summ.unique.col(train)


apply(train, 2, summary)
summary(train[,"PassengerId"])

summary(train[,"Survived"])

summary(train[,"Pclass"])

summary(train[,"Name"])

summary(train[,"Sex"])
summary(test[,"Sex"])

summary(train[,"Cabin"])

summary(train[,"Age"])
hist(train$Age)

hist(train$Age[train$Survived == 1])
hist(train$Age[train$Survived == 0])

table(train$Pclass, train$Survived)
barplot(t(table(train$Pclass, train$Survived)))

#SPlit

proportion<-1/2
rows<-length(train[,1])
splitsize<-ceiling(length(train[,1])*proportion)
train0<-sample(1:rows, rows, replace=FALSE)
index<-train0[1:splitsize]

train1<-train[index, ]
train2<-train[-index, ]

str(train1)
str(train2)

#Numeric only


numericvar<-as.vector(sapply(train[1,], is.numeric)*1)
numericvar
str(train[,numericvar == 1])

train.num<-train[,numericvar == 1]
train.var<-apply(train.num, 2, var)

head(train.num / train.var[col(train.num)])
head(t(apply(train.num, 1, "/", train.var)))

head(train.num/train.var)
head(train.num)

str(train.num[,-c(1,7)])

titanic.pcov <- prcomp(as.matrix(train.num[is.na(train.num$Age) == FALSE,c("Pclass", "Age", "SibSp", "Parch", "Fare")]), scale = TRUE)
screeplot(titanic.pcov, type="lines")

princomp(as.matrix(train.num[,c("Age", "Fare")]), scores = TRUE, cor=FALSE)

apply(train, 2, var)
var(train[,1])

#NewVar

newtrain=train
newtrain$Family = newtrain$Parch + newtrain$SibSp
newtrain$Title = substr(str_extract(newtrain$Name, ", (.*?)[:punct:]"), 3, 15)
summary(as.factor(newtrain$Title))
# substring(newtrain$Name, find())
# unique(str_extract(newtrain$Name, "[:punct:]\\s...."))
# find("[:punct:]\\s[Mirs]", newtrain$Name[10], simple.words=FALSE)
# str_extract(newtrain$Name[10:12], "Mrs|Miss|Mr|Master|Mme|Ms|Lady|Sir|Mlle|Major|Capt|Dr")
# str_extract(newtrain$Name[10:12], ", (.*?)[:punct:]")
# unique(str_extract(newtrain$Name, ", (.*?)[:punct:]"))

#Logistic

titanicglm1<-glm(Survived ~ as.factor(Pclass) + Age + SibSp + Parch + Fare + Sex, data=train, family="binomial")
summary(titanicglm1)

newtrain=train
newtrain$Family = newtrain$Parch + newtrain$SibSp

titanicglm2<-glm(Survived ~ as.factor(Pclass) + Age + Family + Fare + Sex, data=newtrain, family="binomial")
summary(titanicglm2)

titanicglm3<-glm(Survived ~ as.factor(Pclass) + Age + Family + Sex, data=newtrain, family="binomial")
summary(titanicglm3)

newtrain1=train1
newtrain1$Family = newtrain1$Parch + newtrain1$SibSp

titanicglm4<-glm(Survived ~ as.factor(Pclass) + Age + Family + Sex, data=newtrain1, family="binomial")
summary(titanicglm4)
ls(titanicglm4)

newtrain2=train2
newtrain2$Family = newtrain2$Parch + newtrain2$SibSp
a<-predict(titanicglm4, newdata=newtrain2)
#b<-predict(titanicglm4, newdata=newtrain2, type="prob")
head(a)
head(logit(a))
head(inv.logit(a))
head(newtrain2$Survived)
sum(abs(inv.logit(a) - newtrain2$Survived)[is.na(train2$Age) == FALSE] )/ length(newtrain2$Survived[is.na(train2$Age) == FALSE] )
sum(abs(round(inv.logit(a), digits = 0) - newtrain2$Survived)[is.na(train2$Age) == FALSE] )/ length(newtrain2$Survived[is.na(train2$Age) == FALSE] )
