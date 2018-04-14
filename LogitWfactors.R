##########
# 
##########

# split the training data into a secondary test (not Kaggle)
set.seed(100) # set seed so that same sample can be reproduced in future

# now selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n=nrow(train1), size=floor(.75*nrow(train1)), replace=FALSE)

# subset the data using the sample integer vector created above
train3 <- train1[sample, !(names(train1) %in% c("Age","Sibch", "Parch", "Embarked"))]
test3  <- train1[-sample, !(names(test1) %in% c("Age","Sibch", "Parch", "Embarked"))]
str(train3)
str(test3)

# Logistic regression model
TitanicLog2 = glm(Survived ~ ., data = train3, family = binomial(link='logit'))
summary(TitanicLog2)

c=train1$Survived
p=predict(TitanicLog2, newdata=test3, type='response')
cv=cv.glm(data=train3, glmfit=TitanicLog2, K=5)
#ls(cv)
cv["delta"]
