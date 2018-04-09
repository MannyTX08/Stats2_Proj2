# Load necessary packages and ensure they are active
load.lib = c("randomForest","ggplot2","ggthemes","mice","scales","dplyr","Amelia","ROCR")

install.lib = load.lib[!load.lib %in% installed.packages()]
for(lib in install.lib){
  install.packages(lib,dependences=TRUE)
} 

sapply(load.lib,require,character=TRUE)

# Load train and test csv files from working directory
# Using Amelia package visualize where we need imputation

train <- read.csv('./Data/train.csv')
Amelia::missmap(train, main="Missing Values in Raw Data", col = c("black","light blue"))

test <- read.csv('./Data/test.csv')
Amelia::missmap(test, main="Missing Values in Raw Data", col = c("black","light blue"))

# Append test to train for data review and cleaning (result column only valid in train)
full <- bind_rows(train, test)

# Review if components of name, specifically title add to prediction
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Currently 18 levels for Factor title
table(full$Sex, full$Title)

uncommon <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
              'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Combine different titles into similar category
# Mlle is french for 'Mademoiselle'
# Mme is French for 'Madame'
# https://en.wikipedia.org/wiki/Mademoiselle_(title)
# https://en.wikipedia.org/wiki/French_honorifics

full$Title[full$Title == 'Mlle']  <- 'Miss'
full$Title[full$Title == 'Ms']  <- 'Miss'
full$Title[full$Title == 'Mme']  <- 'Mrs'
full$Title[full$Title %in% uncommon]  <- 'uncommon'

# Reduced to 5 levels for Factor Title
table(full$Sex, full$Title)

# Using mice package impute values for Age that are missing
sum(is.na(train$Age)) # 177 missing values
sum(is.na(full$Age))  # 263 missing values in both train and test

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Title')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Survived')], method='rf') 

# Save the complete output 
mice_output <- complete(mice_mod)

# Plot age distributions of raw data against imputed from mice package
par(mfrow=c(1,2))
hist(full$Age, freq=F, main='Age: Original Data', col='lightblue', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Imputation Output', col='lightblue', ylim=c(0,0.04))

# Replace Age variable from the mice model
full$Age <- mice_output$Age

# Show new number of missing Age values is now 0
sum(is.na(full$Age))

# Create DF of independent/dependent variables
nonvars = c("PassengerId","Name","Ticket","Cabin")
full2 = full[,!(names(full) %in% nonvars)]
str(full2)

convert.vars <- c('Pclass','Sex','Embarked','Title')

full2[convert.vars] <- lapply(full2[convert.vars], function(x) as.numeric(x))

# Get back to train data set
train2 <- full2[!is.na(full2$Survived),]
test2 <- full2[is.na(full2$Survived),]

# Correlation matrix
cor(train2)

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
