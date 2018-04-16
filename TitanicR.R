##########
# Here need to do better job of cross validation for project
##########

# split the training data into a secondary test (not Kaggle)
set.seed(100) # set seed so that same sample can be reproduced in future

# now selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n=nrow(train2), size=floor(.75*nrow(train2)), replace=FALSE)

# subset the data using the sample integer vector created above
train3 <- train2[sample, ]
test3  <- train2[-sample, ]

# Logistic regression model
TitanicLog1 = glm(Survived ~ ., data = train3, family = binomial(link='logit'))
summary(TitanicLog1)

fittedresults <- predict(TitanicLog1, newdata=test3, type='response')

# predict based on the test data, type='response' output probabilities in the form of P(y=1|X)
fittedresults <- predict(TitanicLog1, newdata=test3, type='response')

# count any NAs in the fittedresults
sum(is.na(fittedresults))

# if P(y=1|X) > 0.5 then y = 1 otherwise y=0
fittedresults <- ifelse(fittedresults > 0.5, 1, 0)

# Need to adjust here..
# calculate the mean of the fitted results that don't equal the observed result - IGNORE NAs
misClasificError <- mean(fittedresults != test3$Survived, na.rm=TRUE) # this adds up all the instances of misclassification then divides by total (via mean)

# print the output as 100% - error
print(paste('Accuracy',1-misClasificError)) # 78.92 % Accurate

# ROC Curves
pr <- ROCR::prediction(fittedresults, test3$Survived)
prf <- ROCR::performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# More Work Here
auc <- ROCR::performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
