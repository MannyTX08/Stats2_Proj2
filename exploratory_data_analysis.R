par(mfrow=c(1,1))

summary(train[,"PassengerId"])
summary(test[,"PassengerId"])
length(unique(train[,"PassengerId"]))
length(unique(test[,"PassengerId"]))

summary(train[,"Survived"])

summary(train[,"Pclass"])
summary(test[,"Pclass"])
t<-table(train$Pclass, train$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

summary(train[,"Name"])
summary(test[,"Name"])
length(unique(train[,"Name"]))
length(unique(test[,"Name"]))


summary(train[,"Sex"])
summary(test[,"Sex"])

summary(train[,"Age"])
summary(test[,"Age"])
hist(train$Age, breaks=seq(0,100, by=5))
sort(train$Age)[1:30]

summary(train1[,"AgeBin"])
summary(test1[,"AgeBin"])
t<-table(train1$AgeBin, train1$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=5)))
#full$AgeBin[10:35]
l<-levels(AgeBin2)[-(length(levels(AgeBin2)))]
#l
#    replace <NA> with 'unknown'
levels(AgeBin2)<-c(l, 'unknown')
t<-table(AgeBin2, train1$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=6)))
#full$AgeBin[10:35]
l<-levels(AgeBin2)[-(length(levels(AgeBin2)))]
#l
#    replace <NA> with 'unknown'
levels(AgeBin2)<-c(l, 'unknown')
t<-table(AgeBin2, train1$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=9)))
#full$AgeBin[10:35]
l<-levels(AgeBin2)[-(length(levels(AgeBin2)))]
#l
#    replace <NA> with 'unknown'
levels(AgeBin2)<-c(l, 'unknown')
t<-table(AgeBin2, train1$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

AgeBin3<-addNA(cut(train$Age, seq(0, 90, by=15)))
#full$AgeBin[10:35]
l<-levels(AgeBin3)[-(length(levels(AgeBin3)))]
#l
#    replace <NA> with 'unknown'
levels(AgeBin3)<-c(l, 'unknown')
t<-table(AgeBin3, train1$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=6)))
#full$AgeBin[10:35]
#l<-levels(AgeBin2)[-(length(levels(AgeBin2)))]
#l
#    replace <NA> with 'unknown'
levels(AgeBin2)<-c("6 or less", rep("7 - 60", 9), rep("60+", 5), "unknown")
t<-table(AgeBin2, train1$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

AgeBin2<-addNA(cut(train$Age, seq(0, 90, by=6)))
#full$AgeBin[10:35]
#l<-levels(AgeBin2)[-(length(levels(AgeBin2)))]
#l
#    replace <NA> with 'unknown'
levels(AgeBin2)<-c("6 or less", rep("7 - 65", 10), rep("66+", 4), "unknown")
t<-table(AgeBin2, train1$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

hist(train$Age[train$Survived == 1], breaks=seq(0,100, by=5))
hist(train$Age[train$Survived == 0], breaks=seq(0,100, by=5))

t<-table(train$Pclass, train$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

#After imputation
summary(train1[,"Age"])
summary(test1[,"Age"])

summary(train[,"Fare"])
summary(test[,"Fare"])

FareBin<-cut(train$Age, seq(0, 60, by=6))
#FareBin[10:35]
l<-levels(FareBin)[-(length(levels(FareBin)))]
#l
#    replace <NA> with 'unknown'
levels(FareBin)<-c(l, '> $55')
t<-table(FareBin, train$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

FareBin<-cut(train$Age, seq(0, 60, by=5))
#FareBin[10:35]
l<-levels(FareBin)[-(length(levels(FareBin)))]
#l
#    replace <NA> with 'unknown'
levels(FareBin)<-c(l, '> $55')
t<-table(FareBin, train$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

FareBin<-cut(train$Age, seq(0, 60, by=4))
#FareBin[10:35]
l<-levels(FareBin)[-(length(levels(FareBin)))]
#l
#    replace <NA> with 'unknown'
levels(FareBin)<-c(l, '> $56')
t<-table(FareBin, train$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

FareBin<-cut(train$Age, seq(0, 60, by=3))
#FareBin[10:35]
l<-levels(FareBin)[-(length(levels(FareBin)))]
#l
#    replace <NA> with 'unknown'
levels(FareBin)<-c(l, '> $57')
t<-table(FareBin, train$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

FareBin<-cut(train$Age, seq(0, 63, by=7))
#FareBin[10:35]
l<-levels(FareBin)[-(length(levels(FareBin)))]
#l
#    replace <NA> with 'unknown'
levels(FareBin)<-c(l, '> $57')
t<-table(FareBin, train$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))

summary(train[,"Cabin"])[1:30]
summary(test[,"Cabin"])[1:30]

summary(train[,"Embarked"])
summary(test[,"Embarked"])


summary(train1[,"Title"])
summary(test1[,"Title"])
t<-table(train1$Title, train1$Survived)
t
t2<-t/apply(t, 1, sum)
t2
barplot(t(t2))