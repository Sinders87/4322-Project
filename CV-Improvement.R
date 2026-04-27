
#read data
data <- read.csv("data/processed/cleaned_data.csv")

# IN CASE PACKAGES NOT INSTALLED: install.packages(c("caret", "MASS", "randomForest", "dplyr"))

#load libraries
library(caret)
library(MASS)
library(randomForest)
library(dplyr)

#ensure that response variable classifies correctly
data$Successful_App <- factor(data$Successful_App, levels = c(0, 1), labels = c("No", "Yes"))


#set correct seed
set.seed(42)

#k-fold setup for logistic regresssion
train_control <- trainControl(
  method = "cv",
  number = 5,
  savePredictions = "final",
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

#perform cv on logistic
model_cv <- train(
  Successful_App ~ Category + Rating + Reviews + Size_MB + Type + Price_. + 
    Content_Rating + Genres + Android_Version,
  data = data,
  method = "glm",
  family = "binomial",
  trControl = train_control,
  metric = "Accuracy"
)

#get confusion matrix
lr_preds <- model_cv$pred
cm_log <- confusionMatrix(lr_preds$pred, lr_preds$obs)
cm_log

#perform cv on random forest
rf_model <- train(
  Successful_App ~ Category + Rating + Reviews + Size_MB + Type + Price_. + 
    Content_Rating + Genres + Android_Version,
  data = data,
  method = "rf",
  trControl = train_control,
  tuneLength = 3
)

#get accuracy
rf_preds <- rf_model$pred
confusionMatrix(rf_preds$pred, rf_preds$obs)

#visuals from here onward

#for RF
importance <- varImp(rf_model)
print(importance)
plot(importance, main = "RF Variable Importance")

log_table <- cm_log$table
print(log_table)