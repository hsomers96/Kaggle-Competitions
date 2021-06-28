train <- read.csv('train.csv')
test <- read.csv('test (1).csv')

table(train$Survived)

prop.table(table(train$Survived))

#Assume everyone dies
test$Survived <- rep(0,418)

submit <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = 'theyallperish.csv', row.names = FALSE)


#Part 2 - Gender-class Model
summary(train$Sex)

prop.table(table(train$Sex, train$Survived), 1)   # See that most women survived

test$Survived <- 0       #Automatically default to people died
test$Survived[test$Sex == 'female'] <- 1     #Then change it so all women survive


train$Child <- 0         #indication of whether a child or not
train$Child[train$Age < 18] <- 1   #If younger than 18, then assign a 1 for "is child"


aggregate(Survived ~ Child + Sex, data=train, FUN=sum)

aggregate(Survived ~ Child + Sex, data=train, FUN=length)

aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)})


train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'


aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0


submit <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = 'genderclassmodel.csv', row.names = FALSE)

