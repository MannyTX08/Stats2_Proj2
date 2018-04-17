# Load train and test csv files from working directory
# Using Amelia package visualize where we need imputation

#setwd("~/Stats2_Proj2/")

par(mfrow=c(1,2))

train <- read.csv('Data/train.csv') # Reading from location after clone
Amelia::missmap(train, main="Missing Values in Train Data", col = c("black","light blue"))

test <- read.csv('Data/test.csv') # Reading from location after clone
Amelia::missmap(test, main="Missing Values in Test Data", col = c("black","light blue"))

par(mfrow=c(1,1))

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

# Handle the null Fare in Test
# The passenger departed from S and had Pclass = 3, lets find an appropriate value
# Replace missing fare value with median fare for class/embarkment
nullFare = full[is.na(full$Fare),] #1044

ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) + 
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),colour='red', linetype='dashed', lwd=1) +
  scale_x_continuous(breaks = seq(0,70,10), labels=dollar_format()) +
  ggtitle("Density of Ticket Fare for Embarked = S and Pclass = 3",
          subtitle = paste0("Median Fare: $", 
          median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm=T))) +
  theme_few()

full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE) # $8.05

# Using mice package impute values for Age that are missing
sum(is.na(train$Age)) # 177 missing values
sum(is.na(full$Age))  # 263 missing values in both train and test

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

# Create Age as a categorical variable
full$AgeBin <- cut(full$Age, seq(0, 90, by=3))
levels(full$AgeBin) <- c(rep("6 or less", 2), rep("(7 - 63]", 19), rep("Over 63", 9))

#full$AgeBin <- cut(full$Age, seq(0, 80, by=10))
#full$AgeBin[10:35]
# l <- levels(full$AgeBin)[-(length(levels(full$AgeBin)))]
# #l
# #    replace <NA> with 'unknown'
#levels(full$AgeBin)<-c(l, 'unknown')
summary(full$AgeBin)

# Create a family = siblings + parents/children
# -Possibly for dimension reducing
full$Family = full$Parch + full$SibSp

# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked','Title', 'AgeBin')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Create DF of independent/dependent variables
nonvars = c("PassengerId","Name","Ticket","Cabin")
full2 = full[,!(names(full) %in% nonvars)]
str(full2)

convert.vars <- c('Pclass','Sex','Embarked','Title', 'AgeBin')

full2[convert.vars] <- lapply(full2[convert.vars], function(x) as.numeric(x))

# Get back to train data set
train1 <- full[!is.na(full$Survived),!(names(full) %in% nonvars)]
test1 <- full[is.na(full$Survived),!(names(full) %in% nonvars)]
train2 <- full2[!is.na(full2$Survived),]
test2 <- full2[is.na(full2$Survived),]

# Structure & Correlation matrix
str(train2)
corMatrix = cor(train2)
corMatrix

write.csv(corMatrix, "CorrelationMatrix.csv")

par(mfrow=c(1,1))

# Correlogram
# corrplot(corMatrix, type = "upper")
# corrplot(corMatrix, type = "upper", method = "number", order = "original",
#         diag = FALSE, tl.pos = "td") #, cl.pos = "n")

# http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(train2)

corrplot(corMatrix, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         diag=FALSE 
)

