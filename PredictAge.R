# predict  ages from other variables

PredictAge <- function(df1, df2) {
  
  use.vars <- c("Deck", "Embarked", "FamSize", "Fare", "Last_Name", "Parch", "Pclass", "Sex", "SibSp")
  
  rf_Age <- rfPermute(df1$Age ~ ., df1[,use.vars], ntree=500, replace=FALSE)
  
  PredictAge <- predict(rf_Age, df2)
  
  return(PredictAge)
}