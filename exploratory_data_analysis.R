par(mfrow=c(1,1))
par(xpd=NA)
#par(oma=c(3.7,0,0,0))
#par(mar=c(5, 4, 4, 2) + 0.1)

############################
## FUNCTIONS
############################

# Barplot Percentages

summ.percent.by<-function(vec1, vec2, main="Percents", xname='vec1', yname='vec2'){
  par(xpd=NA)
  par(mar=c(4.5, 4, 4, 2))
  #par(oma=c(4.6, 0, 0, 0))
   # vec1=train$Pclass
   # vec2=train$Survived
   # main="Percents"
t<-table(vec1, vec2, dnn=c(xname, yname))
print(t)
t2<-t/apply(t, 1, sum)
barplot(t(t2), col=c('grey', 'lightblue'), legend.text=c('0=Did not Survive','1=Survived'), 
        main=main, args.legend = list(x="bottom", horiz=TRUE, inset=c(0, -0.3)))
print(t2)
par(mar=c(5, 4, 4, 2) + 0.1)
#par(mar=c(1, 3, 3, 2) + 0.1)
                                      }
summ.percent.by(train$Pclass, train$Survived, main="Percent Survived by Class", 
                xname='Passenger Class', yname="Survived")



# Dual Histograms

hist.by<-function(vec1, vec2, main="Percents", ymax=25, breaks=breaks, xname='vec1', yname='vec2'){
  par(mfrow=c(2,1))
  
  # vec1=train$Fare
  # vec2=train$Survived
  # main="Percents"
  # xname='vec1'
  # yname='vec2'
  # ymax=25
  
  vec1_1=vec1[vec2==1]
  vec1_2=vec1[vec2==0]
  
  t<-table(vec1, vec2, dnn=c(xname, yname))
  xmin=min(vec1)
  xmax=max(vec1)
  ymax=ymax
  
  breaks2=seq(xmin, xmax, length.out = breaks)
  
  par(mar=c(0.2,5,3,3))
  hist(vec1_1 , main=main , xlim=c(xmin,xmax), ylab="Freq. Survived", xlab="", ylim=c(0,ymax) , xaxt="n", 
       las=1 , col="lightblue", breaks=breaks2)
  
  legend("topright", c('Survived', 'Did Not Survive'), col=c('lightblue','tomato3') , pch = 15)
  
  par(mar=c(5,5,0.2,3))
  hist(vec1_2 , main= '', xlim=c(xmin,xmax), ylab="Freq. not Survived", xlab=xname, ylim=c(ymax,0) , 
       las=1 , col="tomato3"  , breaks=breaks2)
  
 par(mfrow=c(1,1))
  par(mar=c(5, 4, 4, 2) + 0.1)
                                                                                  }
# test
hist.by(train$Fare[train$Fare < 150], train$Survived[train$Fare < 150], 
        main="Histograms of Fare by Survival", ymax=300, breaks=20, 
        xname='Fare', yname="Survived")

############################
## PASSENGERID
############################

summary(train[,"PassengerId"])
summary(test[,"PassengerId"])
length(unique(train[,"PassengerId"]))
length(unique(test[,"PassengerId"]))

############################
## SURVIVED
############################

summary(train[,"Survived"])


############################
## PCLASS
############################

summary(train[,"Pclass"])
summary(test[,"Pclass"])

summ.percent.by(train$Pclass, train$Survived, main="Percent Survived by Class", 
                xname='Passenger Class', yname="Survived")

############################
## Name
############################

summary(train[,"Name"])[1:30]
summary(test[,"Name"])[1:30]
length(unique(train[,"Name"]))
length(unique(test[,"Name"]))


############################
## TITLE
############################

summary(train1[,"Title"])
summary(test1[,"Title"])

summ.percent.by(train1$Title, train1$Survived, main="Proportion Survived by Title", 
                xname='Passenger Title', yname="Survived")

table(train1$Title[!(train1$AgeBin %in% c("(0,10]", "(10,20]"))], train1$AgeBin[!(train1$AgeBin %in% c("(0,10]", "(10,20]"))])

summ.percent.by(train1$Title[!(train1$AgeBin %in% c("(0,10]", "(10,20]"))], 
                train1$Survived[!(train1$AgeBin %in% c("(0,10]", "(10,20]"))],
                main="Proportion Survived by Title, Age > 20", 
                xname='Passenger Title', yname="Survived")

summ.percent.by(train1$Title[!(train1$AgeBin %in% c("(0,10]", "(10,20]","unknown"))], 
                train1$Survived[!(train1$AgeBin %in% c("(0,10]", "(10,20]", "unknown"))],
                main="Proportion Survived by Title, Age > 20 and not Unknown", 
                xname='Passenger Title', yname="Survived")

############################
## SEX
############################

summary(train[,"Sex"])
summary(test[,"Sex"])

summ.percent.by(train$Sex, train$Survived, main="Percent Survived by Sex", 
                xname='Passenger Sex', yname="Survived")

############################
## AGEBIN
############################

summary(train1[,"AgeBin"])
summary(test1[,"AgeBin"])

# FareBin<-cut(train$Fare, seq(0, 60, by=2))
# l<-levels(FareBin)[-(length(levels(FareBin)))]
# levels(FareBin)<-c(l, '> $58')

summ.percent.by(train1$AgeBin, train1$Survived, main="Proportion Survived by AgeBin", 
                xname='Age of Passenger', yname="Survived")


AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=5)))
l<-levels(AgeBin2)[-(length(levels(AgeBin2)))]
levels(AgeBin2)<-c(l, 'unknown')

summ.percent.by(AgeBin2, train1$Survived, main="Proportion Survived by AgeBin", 
                xname='Age of Passenger', yname="Survived")


AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=6)))
l<-levels(AgeBin2)[-(length(levels(AgeBin2)))]
levels(AgeBin2)<-c(l, 'unknown')

summ.percent.by(AgeBin2, train1$Survived, main="Proportion Survived by AgeBin", 
                xname='Age of Passenger', yname="Survived")


AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=9)))
l<-levels(AgeBin2)[-(length(levels(AgeBin2)))]
levels(AgeBin2)<-c(l, 'unknown')

summ.percent.by(AgeBin2, train1$Survived, main="Proportion Survived by AgeBin", 
                xname='Age of Passenger', yname="Survived")


AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=10)))
l<-levels(AgeBin2)[-(length(levels(AgeBin2)))]
levels(AgeBin2)<-c(l, 'unknown')

summ.percent.by(AgeBin2, train1$Survived, main="Proportion Survived by AgeBin", 
                xname='Age of Passenger', yname="Survived")


AgeBin3<-addNA(cut(train$Age, seq(0, 90, by=15)))
l<-levels(AgeBin3)[-(length(levels(AgeBin2)))]
levels(AgeBin3)<-c(l, 'unknown')

summ.percent.by(AgeBin3, train1$Survived, main="Proportion Survived by AgeBin", 
                xname='Age of Passenger', yname="Survived")


AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=6)))
levels(AgeBin2)<-c("6 or less", rep("7 - 60", 9), rep("Over 60", 5), "unknown")

summ.percent.by(AgeBin2, train1$Survived, main="Proportion Survived by AgeBin", 
                xname='Age of Passenger', yname="Survived")


AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=6)))
levels(AgeBin2)<-c("6 or less", rep("7 - 65", 10), rep("66+", 4), "unknown")

summ.percent.by(AgeBin2, train1$Survived, main="Proportion Survived by AgeBin", 
                xname='Age of Passenger', yname="Survived")


AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=3)))
levels(AgeBin2)<-c(rep("6 or less", 2), rep("(7 - 63]", 19), rep("Over 63", 9), "unknown")

summ.percent.by(AgeBin2, train1$Survived, main="Proportion Survived by AgeBin", 
                xname='Age of Passenger', yname="Survived")

# After imputation
AgeBin3<-cut(train1$Age, seq(0, 90, by=3))
levels(AgeBin3)<-c(rep("6 or less", 2), rep("(7 - 63]", 19), rep("Over 63", 9))

summ.percent.by(AgeBin3, 
                train1$Survived, 
                main="Proportion Survived by AgeBin", 
                xname='Age of Passenger (after imputation)', yname="Survived")

AgeBin3<-cut(train1$Age, seq(0, 90, by=3))
levels(AgeBin3)<-c(rep("6 or less", 2), rep("(7 - 60]", 18), rep("Over 60", 10))

summ.percent.by(AgeBin3, 
                train1$Survived, 
                main="Proportion Survived by AgeBin", 
                xname='Age of Passenger (after imputation)', yname="Survived")

AgeBin3<-cut(train1$Age, seq(0, 90, by=6))
#levels(AgeBin3)<-c(rep("6 or less", 2), rep("(7 - 63]", 19), rep("Over 63", 9))

summ.percent.by(AgeBin3, 
                train1$Survived, 
                main="Proportion Survived by AgeBin", 
                xname='Age of Passenger (after imputation)', yname="Survived")

AgeBin3<-cut(train1$Age, seq(0, 80, by=10))
#levels(AgeBin3)<-c(rep("6 or less", 2), rep("(7 - 63]", 19), rep("Over 63", 9))

summ.percent.by(AgeBin3, 
                train1$Survived, 
                main="Proportion Survived by Age (10 Year Increments)", 
                xname='Age of Passenger (after imputation)', yname="Survived")




hist(train$Age[train$Survived == 1], breaks=seq(0,100, by=5))
hist(train$Age[train$Survived == 0], breaks=seq(0,100, by=5))

t<-table(train$Pclass, train$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

############################
## AGE BEFORE IMPUTATION
############################

summary(train[,"Age"])
summary(test[,"Age"])

hist.by(train$Age[!is.na(train$Age)], train$Survived[!is.na(train$Age)], 
        main="Histograms of Age by Survival", ymax=100, breaks=16, 
        xname='Age (after imputation)', yname="Survived")

hist.by(train$Age[train1$Age >= 45 & !is.na(train$Age)], 
        train$Survived[train1$Age >= 45 & !is.na(train$Age)], 
        main="Histograms of Age by Survival", ymax=25, breaks=12, 
        xname='Age (after imputation)', yname="Survived")

hist.by(train$Age[train1$Age >= 45 & !is.na(train$Age)], 
        train$Survived[train1$Age >= 45 & !is.na(train$Age)],
        main="Histograms of Age by Survival", ymax=15, breaks=20, 
        xname='Age (after imputation)', yname="Survived")

############################
## AGE AFTE IMPUTATION
############################
#After imputation
summary(train1[,"Age"])
summary(test1[,"Age"])

hist.by(train1$Age, train$Survived, 
        main="Histograms of Age by Survival", ymax=150, breaks=15, 
        xname='Age (after imputation)', yname="Survived")

hist.by(train1$Age[train1$Age >= 45], train$Survived[train1$Age >= 45], 
        main="Histograms of Age by Survival", ymax=30, breaks=15, 
        xname='Age (after imputation)', yname="Survived")

hist.by(train1$Age[train1$Age >= 45], train$Survived[train1$Age >= 45], 
        main="Histograms of Age by Survival", ymax=20, breaks=16, 
        xname='Age (after imputation)', yname="Survived")

hist.by(train1$Age[train1$Age >= 45], train$Survived[train1$Age >= 45], 
        main="Histograms of Age by Survival", ymax=15, breaks=20, 
        xname='Age (after imputation)', yname="Survived")

############################
## FARE
############################

summary(train[,"Fare"])
summary(test[,"Fare"])

#   Dual Histograms

hist.by(train$Fare[train$Fare < 150], train$Survived[train$Fare < 150], 
        main="Histograms of Fare by Survival", ymax=400, breaks=10, 
        xname='Fare', yname="Survived")

hist.by(train$Fare[train$Fare < 150], train$Survived[train$Fare < 150], 
        main="Histograms of Fare by Survival", ymax=300, breaks=30, 
        xname='Fare', yname="Survived")

#   Percentages by Fare

FareBin<-cut(train$Fare, seq(0, 70, by=2))
l<-levels(FareBin)[-(length(levels(FareBin)))]
levels(FareBin)<-c(l, '> $68')

summ.percent.by(FareBin, train1$Survived, main="Proportion Survived by Fare", 
                xname='Ticket Price', yname="Survived")


FareBin<-cut(train$Fare, seq(0, 80, by=3))
l<-levels(FareBin)[-(length(levels(FareBin)))]
levels(FareBin)<-c(l, '> $77')

summ.percent.by(FareBin, train1$Survived, main="Proportion Survived by Fare", 
                xname='Ticket Price', yname="Survived")


FareBin<-cut(train$Fare, seq(0, 90, by=4))
l<-levels(FareBin)[-(length(levels(FareBin)))]
levels(FareBin)<-c(l, '> $84')

summ.percent.by(FareBin, train1$Survived, main="Proportion Survived by Fare", 
                xname='Ticket Price', yname="Survived")

FareBin<-cut(train$Fare, seq(0, 100, by=5))
l<-levels(FareBin)[-(length(levels(FareBin)))]
levels(FareBin)<-c(l, '> $95')

summ.percent.by(FareBin, train1$Survived, main="Proportion Survived by Fare", 
                xname='Ticket Price', yname="Survived")


FareBin<-cut(train$Fare, seq(0, 60, by=6))
l<-levels(FareBin)[-(length(levels(FareBin)))]
levels(FareBin)<-c(l, '> $54')

summ.percent.by(FareBin, train1$Survived, main="Proportion Survived by Fare", 
                xname='Ticket Price', yname="Survived")


FareBin<-cut(train$Fare, seq(0, 90, by=7))
l<-levels(FareBin)[-(length(levels(FareBin)))]
levels(FareBin)<-c(l, '> $84')

summ.percent.by(FareBin, train1$Survived, main="Proportion Survived by Fare", 
                xname='Ticket Price', yname="Survived")


FareBin<-cut(train$Fare, seq(0, 88, by=8))
l<-levels(FareBin)[-(length(levels(FareBin)))]
levels(FareBin)<-c(l, '> $80')

summ.percent.by(FareBin, train1$Survived, main="Proportion Survived by Fare", 
                xname='Ticket Price', yname="Survived")


FareBin<-cut(train$Fare, seq(0, 90, by=9))
l<-levels(FareBin)[-(length(levels(FareBin)))]
levels(FareBin)<-c(l, '> $81')

summ.percent.by(FareBin, train1$Survived, main="Proportion Survived by Fare", 
                xname='Ticket Price', yname="Survived")


FareBin<-cut(train$Fare, seq(0, 120, by=10))
l<-levels(FareBin)[-(length(levels(FareBin)))]
levels(FareBin)<-c(l, '> $110')

summ.percent.by(FareBin, train1$Survived, main="Proportion Survived by Fare", 
                xname='Ticket Price', yname="Survived")


FareBin<-cut(train$Fare, seq(0, 240, by=12))
l<-levels(FareBin)[-(length(levels(FareBin)))]
levels(FareBin)<-c(l, '> $238')

summ.percent.by(FareBin, train1$Survived, main="Proportion Survived by Fare", 
                xname='Ticket Price', yname="Survived")

############################
## CABIN
############################

summary(train[,"Cabin"])[1:30]
summary(test[,"Cabin"])[1:30]

section_train=substr(train[,"Cabin"], 1, 1)
section_test=substr(test[,"Cabin"], 1, 1)

table(section_train)
table(section_test)

summ.percent.by(section_train, train1$Survived, main="Proportion Survived by Section", 
                xname='Cabin Section', yname="Survived")

library('stringr')

sections_train=sapply(train[,"Cabin"], function(x) str_extract_all(x, "[A-z]"))
unique(sections_train)

############################
## EMBARKED
############################

summary(train[,"Embarked"])
summary(test[,"Embarked"])

summ.percent.by(train1$Embarked, train1$Survived, main="Proportion Survived by Port", 
                xname='Port of Embarkation', yname="Survived")

############################
## SIBSP
############################

summary(train[,"SibSp"])
summary(test[,"SibSp"])

#   Dual Histograms

hist.by(train$SibSp, train$Survived, 
        main="Histograms of Siblings/Spouses by Survival", ymax=400, breaks=10, 
        xname='SibSp', yname="Survived")


summ.percent.by(train$SibSp, train1$Survived, main="Proportion Survived by SibSp", 
                xname='SibSp', yname="Survived")

############################
## PARCH
############################

summary(train[,"Parch"])
summary(test[,"Parch"])

#   Dual Histograms

hist.by(train$Parch, train$Survived, 
        main="Histograms of Parents/Children by Survival", ymax=450, breaks=10, 
        xname='Parch', yname="Survived")


summ.percent.by(train$Parch, train1$Survived, main="Proportion Survived by Parch", 
                xname='Parch', yname="Survived")

############################
## FAMILY
############################

summary(train1[,"Family"])
summary(test1[,"Family"])

#   Dual Histograms

hist.by(train1$Family, train$Survived, 
        main="Histograms of Family Size by Survival", ymax=500, breaks=11, 
        xname='Family', yname="Survived")


summ.percent.by(train1$Family, train1$Survived, main="Proportion Survived by Family", 
                xname='Family', yname="Survived")