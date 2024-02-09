setwd("c:/carretta/Machine Learning/Titanic Survival")

library(rfPermute)

rm(list=ls())

train <- read.csv("train.csv")
test <- read.csv("test.csv")

# Parse last names by Splitting the string at comma
Name <- strsplit(train$Name, ",")

Last_Name <- function(df) {  Name <- strsplit(df$Name, ",")

  df$Last_Name <- NA
  
  for(i in 1:length(Name)) { df$Last_Name[[i]] <- Name[[i]][1] }
    
    return(df$Last_Name) } 

train$Last_Name <- Last_Name(train)
test$Last_Name <- Last_Name(test)

# length of last name

train$Len_Surname <- nchar(train$Last_Name)
test$Len_Surname <- nchar(test$Last_Name)

# first letter of last name

train$Last_Name_Begins_With <- substr(train$Last_Name, 1, 1)
test$Last_Name_Begins_With <- substr(test$Last_Name, 1, 1)

# combine parents-children and sibling-spouse fields to single family size variable

train$FamSize <- train$Parch + train$SibSp
test$FamSize <- test$Parch + test$SibSp

# identify cases in train and test with missing ages

Age.missing.train <- which(is.na(train$Age==TRUE))
Age.missing.test <- which(is.na(test$Age==TRUE))

# assign missing fares from test as mean

test$Fare[which(is.na(test$Fare==TRUE))] <- mean(na.omit(test$Fare))


# Identify cabin area from 1st character

train$Deck <- substr(train$Cabin, 1, 1)
test$Deck <- substr(test$Cabin, 1, 1)

train$Deck[which(train$Deck=="")] <- "X"
test$Deck[which(test$Deck=="")] <- "X"

source("PredictAge.R")

train.w.age <- train[-Age.missing.train,]
Age.predicted.test <- PredictAge(train.w.age, test)
test$Age[Age.missing.test] <- Age.predicted.test[Age.missing.test]

test.w.age <- test[-Age.missing.test,]
Age.predicted.train <- PredictAge(test.w.age, train)
train$Age[Age.missing.train] <- Age.predicted.train[Age.missing.train]


Survived <- as.factor(train$Survived)

complete.vars <- c("Age", "Deck", "Embarked", "FamSize", "Fare", "Last_Name_Begins_With", "Len_Surname", "Pclass", "Sex")
model.data <- train[,which(names(train)%in%complete.vars)]

# assign data priors

rf.model <- rfPermute(Survived ~ ., model.data, ntree=500, nodesize=25, replace=FALSE)
rf.model

plotTrace(rf.model)
plotImportance(rf.model)

predictions <- predict(rf.model, test)

output.file <- cbind.data.frame(test$PassengerId, predictions)
names(output.file) <- c("PassengerId", "Survived")
write.csv(output.file, "Titanic.Predictions.csv", row.names=FALSE)

save.image("Titanic.Output.RData")


