# Load train and test csv files from working directory
# Using Amelia package visualize where we need imputation

train <- read.csv('~/Stats2_Proj2/Data/train.csv')
Amelia::missmap(train, main="Missing Values in Raw Data", col = c("black","light blue"))

test <- read.csv('~/Stats2_Proj2/Data/test.csv')
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

# Create Age as a categorical variable
#   Be sure to run this BEFORE imputing with mice and rf
full$AgeBin<-addNA(cut(full$Age, seq(0, 90, by=10)))
#full$AgeBin[10:35]
l<-levels(full$AgeBin)[-(length(levels(full$AgeBin)))]
#l
#    replace <NA> with 'unknown'
levels(full$AgeBin)<-c(l, 'unknown')
full$AgeBin[10:35]

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