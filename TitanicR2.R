# split the training data into a secondary test (not Kaggel)
set.seed(100) # set seed so that same sample can be reproduced in future

# now selecting 80% of data as sample from total 'n' rows of the data  
sample <- sample.int(n=nrow(train1), size=floor(.80*nrow(train1)), replace=FALSE)

# subset the data using the sample integer vector created above
# keep factors, not numeric 
train3 <- train1[sample, ]
test3  <- train1[-sample, ]

# notvars=c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "Family") #original
# 
# X<-train3[, !(names(train3) %in% notvars)]
# y<-train3[, "Survived"]
# 
# Xy<-cbind.data.frame( M, "Survived"=train3[, "Survived"]  )
# 
# M<-model.matrix(Survived~., data=train3)
# 
# Xy<-cbind.data.frame( M, "Survived"=train3[, "Survived"]  )

# Logistic regression full model
TitanicModelFull = glm(Survived ~ Pclass + Sex +  Age + SibSp + Parch + Fare + Embarked + Title + AgeBin, data = train3, family = binomial(link='logit'))
summary(TitanicModelFull)

# Test predictive capability of full model
fittedresults <- predict(TitanicModelFull, newdata=test3, type='response')

# count any NAs in the fittedresults
sum(is.na(fittedresults))

# if P(y=1|X) > 0.5 then y = 1 otherwise y=0
fittedresults <- ifelse(fittedresults > 0.5, 1, 0)

# calculate the mean of the fitted results that don't equal the observed result - IGNORE NAs
misClasificError <- mean(fittedresults != test3$Survived, na.rm=TRUE) # this adds up all the instances of misclassification then divides by total (via mean)

# print the output as 100% - error
print(paste('Accuracy',1-misClasificError)) # Accuracy = 81.56%

### confirm model correctness above
# print the output as 100% - error
# Test predictive capability of reduced model
fittedresultsF <- predict(TitanicModelFull, newdata=test1, type='response')

# count any NAs in the fittedresults
sum(is.na(fittedresultsF))

# if P(y=1|X) > 0.5 then y = 1 otherwise y=0
fittedresultsF <- ifelse(fittedresultsF > 0.5, 1, 0)

MTXKaggle<-cbind.data.frame("PassengerID"=test$PassengerId, "Survived"=round(fittedresultsF))
names(MTXKaggle)<-c("PassengerID", "Survived")
#write.csv(MTXKaggle, file=paste("~/Stats2_Proj2/Data/", 'MTXKaggleF', ".csv", sep=''), row.names = FALSE)



##########
# Fit model again with only the variables that were significant in the full model
# Pclass, Sex, SibSp, Parch, Embarked, AgeBin

# Logistic regression reduced model
# TitanicModelRed = glm(Survived ~ Pclass + Sex + SibSp + AgeBin, data = train3, family = binomial(link='logit'))
# summary(TitanicModelRed)
TitanicModelRed = glm(Survived ~ Pclass + SibSp + Parch + Title, data = train3, family = binomial(link='logit'))
summary(TitanicModelRed)

# includes all of title anyway
# TitanicModelRed = glm(Survived ~ Pclass2 + Pclass3 + SibSp + Parch + TitleMr + Titleuncommon, data = train3, family = binomial(link='logit'))
# summary(TitanicModelRed)

# Test predictive capability of reduced model
fittedresults2 <- predict(TitanicModelRed, newdata=test3, type='response')

# count any NAs in the fittedresults
sum(is.na(fittedresults2))

# if P(y=1|X) > 0.5 then y = 1 otherwise y=0
fittedresults2 <- ifelse(fittedresults2 > 0.5, 1, 0)

# calculate the mean of the fitted results that don't equal the observed result - IGNORE NAs
misClasificError2 <- mean(fittedresults2 != test3$Survived, na.rm=TRUE) # this adds up all the instances of misclassification then divides by total (via mean)

# print the output as 100% - error
print(paste('Accuracy',1-misClasificError2)) # Accuracy = 82.681%

### confirm model correctness above
# print the output as 100% - error
# Test predictive capability of reduced model
fittedresults3 <- predict(TitanicModelRed, newdata=test1, type='response')

# count any NAs in the fittedresults
sum(is.na(fittedresults3))

# if P(y=1|X) > 0.5 then y = 1 otherwise y=0
fittedresults3 <- ifelse(fittedresults3 > 0.5, 1, 0)

MTXKaggle<-cbind.data.frame("PassengerID"=test$PassengerId, "Survived"=round(fittedresults3))
names(MTXKaggle)<-c("PassengerID", "Survived")
#write.csv(MTXKaggle, file=paste("~/Stats2_Proj2/Data/", 'MTXKaggle', ".csv", sep=''), row.names = FALSE)



######################
# Using glmnet and LASSO
# Isolate the binary response "Survived" from the training data
# GLMTrain.y <- train3$Survived
# GLMTrain.y <- as.factor(as.character(GLMTrain.y))
# 
# # create train data set while removing "Survived" from the training data
# GLMTrain.x <- train3[,!(colnames(train3) == "Survived")]
# 
# # isolate categorical/factors from the continuous features, create dummy variable matrix for all factors
# GLMTrain.xfactors <- model.matrix(GLMTrain.y ~ GLMTrain.x$Pclass + GLMTrain.x$Sex + GLMTrain.x$SibSp + GLMTrain.x$Parch + GLMTrain.x$Embarked + GLMTrain.x$Title + GLMTrain.x$AgeBin)[, -1]
# 
# # remove categorical/factors from GLMTrain.x as they will be added back in the form of dummy variable matrix from above
# dropcolsGLM <- c("Pclass", "Sex", "SibSp", "Parch", "Embarked", "Title", "AgeBin") 
# GLMTrain.x <- GLMTrain.x[,!(colnames(GLMTrain.x) %in% dropcolsGLM)]
# 
# # combine GLMTrain.x continuous variables with GLMTrain.xfactors dummy variable matrix, then converting whole thing to a matrix for glmnet
# GLMTrain.x <- as.matrix(data.frame(GLMTrain.x, GLMTrain.xfactors))




# # subset the data using the sample integer vector created above
# train3 <- train1[sample, ]
# test3  <- train1[-sample, ]
# 
# # # easier to define what may not be in the model
# notvars=c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "Parch") #original
# # "Family", "Sex", "Female", "Miss")  ##These to test multicolinearity
# # Better scores from Family + SibSp than other combinations
# 
# #Some glm functions work better with matrices
# Xy<-cbind.data.frame( train3[, !(names(train3) %in% notvars)],
#                       "Survived"=train3[, "Survived"]  )
# 
# # create dummy variable matrix for all variables
# 
# GLMTrain.X<-model.matrix(Survived~., data=train3)[,-1]
# GLMTrain.y<-train3[, "Survived"]
# 
# GLMTest.X<-model.matrix(Survived~., data=test3)[,-1]
# 
# 
# #GLMTrain<-cbind.data.frame( GLMTrain.X, "Survived"=GLMTrain.y  )
# GLMTrain<-data.matrix( GLMTrain.X, GLMTrain.y  )
# 
# 
# # use glmnet to fit a binomial logistic regression
# # select vars through CV
# glmnetfit<-cv.glmnet(x=GLMTrain.X, y=GLMTrain.y,
#                     type.measure = "class", 
#                     family="binomial", alpha=1)
# plot(glmnetfit, main="MSE reduction curve")
# 
# 
# # The above plot shows us that the optimal value of lambda in the LASSO 
# # model (the value that minimizes the mean square error) is approximately -6.25. 
# # We want to provide the smallest number of coeffecients, but also give good accuracy. 
# # For this, we will use the value of lambda that lies within one standard error of the 
# # optimal value of lamda to display those coeffecients that are significant.
# 
# lambda_lse <- glmnetfit$lambda.1se
# coef(glmnetfit, s=lambda_lse)
# 1-glmnetfit$cvm[which(glmnetfit$lambda == glmnetfit$lambda.1se)]
# # Percent misclassification error
# paste(round(100*(1-glmnetfit$cvm[which(glmnetfit$lambda == glmnetfit$lambda.1se)]), 2), "%", sep="")
# 
# 
# # Significant coefficients are:
# # Family, Pclass, Sex, SibSp, Embarked, Title, and AgeBin
# 
# # # prepare the test data in a similar manner for GLMNET usage (create dummy variables, matrix, etc.)
# # # create test data set while removing "Survived" from the test data
# # GLMTest.x <- test3[,!(colnames(test3) == "Survived")]
# # 
# # # isolate the binary response "Attrition" from the test data
# # GLMTest.y <- test3$Survived
# # GLMTest.y <- as.factor(as.character(GLMTest.y))
# # 
# # # isolate categorical/factors from the continuous features, create dummy variable matrix for all factors
# # GLMTest.xfactors <- model.matrix(GLMTest.y ~ GLMTest.x$Pclass + GLMTest.x$Sex + GLMTest.x$SibSp + GLMTest.x$Parch + GLMTest.x$Embarked + GLMTest.x$Title + GLMTest.x$AgeBin)[, -1]
# # # remove categorical/factors from GLMTest.x as they will be added back in the form of dummy variable matrix from above
# # #dropcolsGLM <- c("Pclass", "Sex", "SibSp", "Parch", "Embarked", "Title", "AgeBin") # From Above
# # GLMTest.x <- GLMTest.x[,!(colnames(GLMTest.x) %in% dropcolsGLM)]
# # 
# # # combine GLMTest.x continuous variables with GLMTest.xfactors dummy variable matrix, then converting whole thing to a matrix for glmnet
# # GLMTest.x <- as.matrix(data.frame(GLMTest.x, GLMTest.xfactors))
# 
# # predict based on the test data, type='response' output probabilities in the form of P(y=1|X)
# GLMfittedresults <- predict(glmnetfit, newx=GLMTest.X, type='response')
# 
# # if P(y=1|X) > 0.5 then y = 1 otherwise y=0
# GLMfittedresults <- ifelse(GLMfittedresults > 0.5, 1, 0)
# 
# # calculate the mean of the fitted results that don't equal the observed result - IGNORE NAs
# misClasificError <- mean(GLMfittedresults != GLMTest.y, na.rm=TRUE) # this adds up all the instances of misclassification then divides by total (via mean)
# 
# # print the output as 100% - error
# print(paste('Accuracy',1-misClasificError)) # 83.80 %
# 
# ######################
# 
# # Create ROC curves
# pr <- prediction(fittedresults, test3$Survived)
# prf <- performance(pr, measure = "tpr", x.measure = "fpr")
# plot(prf, lwd=2, colorize=TRUE)
# 
# # Ref line indicating poor performance, 50/50
# segments(0, 0,1,1)
# 
# # calculate area under curve (AUC)
# auc <- performance(pr, measure = "auc")
# auc <- auc@y.values[[1]]
# 
# # print AUC onto plot
# text(x = .40, y = .6,paste("AUC = ", round(auc,3), sep = ""))
# 
# ####################################################
# ###### ALTERNATE GLM LASSO -- MSE
# ####################################################
# 
# # ######################
# # # Using glmnet and LASSO
# # # Isolate the binary response "Survived" from the training data
# # GLMTrain.y <- train3$Survived
# # GLMTrain.y <- as.factor(as.character(GLMTrain.y))
# # 
# # # create train data set while removing "Survived" from the training data
# # GLMTrain.x <- train3[,!(colnames(train3) == "Survived")]
# # 
# # # isolate categorical/factors from the continuous features, create dummy variable matrix for all factors
# # GLMTrain.xfactors <- model.matrix(GLMTrain.y ~ GLMTrain.x$Pclass + GLMTrain.x$Sex + GLMTrain.x$SibSp + GLMTrain.x$Parch + GLMTrain.x$Embarked + GLMTrain.x$Title + GLMTrain.x$AgeBin)[, -1]
# # 
# # # remove categorical/factors from GLMTrain.x as they will be added back in the form of dummy variable matrix from above
# # dropcolsGLM <- c("Pclass", "Sex", "SibSp", "Parch", "Embarked", "Title", "AgeBin") 
# # GLMTrain.x <- GLMTrain.x[,!(colnames(GLMTrain.x) %in% dropcolsGLM)]
# # 
# # # combine GLMTrain.x continuous variables with GLMTrain.xfactors dummy variable matrix, then converting whole thing to a matrix for glmnet
# # GLMTrain.x <- as.matrix(data.frame(GLMTrain.x, GLMTrain.xfactors))
# 
# 
# 
# 
# # subset the data using the sample integer vector created above
# train3 <- train1[sample, ]
# test3  <- train1[-sample, ]
# 
# # # easier to define what may not be in the model
# notvars=c("Pclass", "Sex", "Embarked", "Title", "AgeBin", "Survived", "Parch") #original
# # "Family", "Sex", "Female", "Miss")  ##These to test multicolinearity
# # Better scores from Family + SibSp than other combinations
# 
# #Some glm functions work better with matrices
# Xy<-cbind.data.frame( train3[, !(names(train3) %in% notvars)],
#                       "Survived"=train3[, "Survived"]  )
# 
# # create dummy variable matrix for all variables
# 
# GLMTrain.X<-model.matrix(Survived~., data=train3)[,-1]
# GLMTrain.y<-train3[, "Survived"]
# 
# GLMTest.X<-model.matrix(Survived~., data=test3)[,-1]
# 
# 
# #GLMTrain<-cbind.data.frame( GLMTrain.X, "Survived"=GLMTrain.y  )
# GLMTrain<-data.matrix( GLMTrain.X, GLMTrain.y  )
# 
# 
# # use glmnet to fit a binomial logistic regression
# # select vars through CV
# glmnetfit<-cv.glmnet(x=GLMTrain.X, y=GLMTrain.y,
#                      type.measure = "mse", 
#                      family="binomial", alpha=1)
# plot(glmnetfit, main="MSE reduction curve")
# 
# 
# # The above plot shows us that the optimal value of lambda in the LASSO 
# # model (the value that minimizes the mean square error) is approximately -6.25. 
# # We want to provide the smallest number of coeffecients, but also give good accuracy. 
# # For this, we will use the value of lambda that lies within one standard error of the 
# # optimal value of lamda to display those coeffecients that are significant.
# 
# lambda_lse <- glmnetfit$lambda.1se
# coef(glmnetfit, s=lambda_lse)
# 1-glmnetfit$cvm[which(glmnetfit$lambda == glmnetfit$lambda.1se)]
# # Percent misclassification error
# # paste(round(100*(1-glmnetfit$cvm[which(glmnetfit$lambda == glmnetfit$lambda.1se)]), 2), "%", sep="")
# 
# 
# # Significant coefficients are:
# # Family, Pclass, Sex, SibSp, Embarked, Title, and AgeBin
# 
# # # prepare the test data in a similar manner for GLMNET usage (create dummy variables, matrix, etc.)
# # # create test data set while removing "Survived" from the test data
# # GLMTest.x <- test3[,!(colnames(test3) == "Survived")]
# # 
# # # isolate the binary response "Attrition" from the test data
# # GLMTest.y <- test3$Survived
# # GLMTest.y <- as.factor(as.character(GLMTest.y))
# # 
# # # isolate categorical/factors from the continuous features, create dummy variable matrix for all factors
# # GLMTest.xfactors <- model.matrix(GLMTest.y ~ GLMTest.x$Pclass + GLMTest.x$Sex + GLMTest.x$SibSp + GLMTest.x$Parch + GLMTest.x$Embarked + GLMTest.x$Title + GLMTest.x$AgeBin)[, -1]
# # # remove categorical/factors from GLMTest.x as they will be added back in the form of dummy variable matrix from above
# # #dropcolsGLM <- c("Pclass", "Sex", "SibSp", "Parch", "Embarked", "Title", "AgeBin") # From Above
# # GLMTest.x <- GLMTest.x[,!(colnames(GLMTest.x) %in% dropcolsGLM)]
# # 
# # # combine GLMTest.x continuous variables with GLMTest.xfactors dummy variable matrix, then converting whole thing to a matrix for glmnet
# # GLMTest.x <- as.matrix(data.frame(GLMTest.x, GLMTest.xfactors))
# 
# # predict based on the test data, type='response' output probabilities in the form of P(y=1|X)
# GLMfittedresults <- predict(glmnetfit, newx=GLMTest.X, type='response')
# 
# # if P(y=1|X) > 0.5 then y = 1 otherwise y=0
# GLMfittedresults <- ifelse(GLMfittedresults > 0.5, 1, 0)
# 
# # calculate the mean of the fitted results that don't equal the observed result - IGNORE NAs
# misClasificError <- mean(GLMfittedresults != GLMTest.y, na.rm=TRUE) # this adds up all the instances of misclassification then divides by total (via mean)
# 
# # print the output as 100% - error
# print(paste('Accuracy',1-misClasificError)) # 83.80 %
# 
# ######################
# 
# # Create ROC curves
# pr <- prediction(fittedresults, test3$Survived)
# prf <- performance(pr, measure = "tpr", x.measure = "fpr")
# plot(prf, lwd=2, colorize=TRUE)
# 
# # Ref line indicating poor performance, 50/50
# segments(0, 0,1,1)
# 
# # calculate area under curve (AUC)
# auc <- performance(pr, measure = "auc")
# auc <- auc@y.values[[1]]
# 
# # print AUC onto plot
# text(x = .40, y = .6,paste("AUC = ", round(auc,3), sep = ""))

