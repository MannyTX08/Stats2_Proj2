# Make use of BaseVars.R and generate a Kaggel submission from logistic regression

TitanicLogKaggel = glm(Survived ~ ., data = train2, family = binomial(link='logit'))
summary(TitanicLogKaggel)

fittedresults <- predict(TitanicLogKaggel, newdata=test2, type='response')

Kaggel = data.frame(PassengerID = rownames(test2), Survived = fittedresults)

# count any NAs in the fittedresults
sum(is.na(Kaggel$Survived))

# if P(y=1|X) > 0.5 then y = 1 otherwise y=0
Kaggel$Survived <- ifelse(fittedresults > 0.5, 1, 0)

write.csv(Kaggel, file = 'titanic_output.csv', row.names = F)


