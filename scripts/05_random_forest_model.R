library(readr)
library(dplyr)
library(randomForest)

# load cleaned data
df <- read_csv("data/processed/cleaned_data.csv")

# rename price column so the formula is easier
df <- df %>%
  rename(Price = `Price_$`)

# convert needed variables
df$Category <- as.factor(df$Category)
df$Type <- as.factor(df$Type)
df$Content_Rating <- as.factor(df$Content_Rating)

# create 3 class outcome variable
df <- df %>%
  mutate(
    Outcome_Class = case_when(
      Installs >= 1000000 & Rating >= 4.2 ~ 3,
      Installs >= 100000 & Rating >= 3.5 ~ 2,
      TRUE ~ 1
    )
  )

# convert outcome to factor
df$Outcome_Class <- as.factor(df$Outcome_Class)

# check class counts
table(df$Outcome_Class)

# split data into train and test
set.seed(123)

n <- nrow(df)
train_rows <- sample(1:n, size = 0.8 * n)

train <- df[train_rows, ]
test <- df[-train_rows, ]

# train random forest model
rf_model <- randomForest(
  Outcome_Class ~ Reviews + Size_MB + Price + Category + Type + Content_Rating,
  data = train
)

# make predictions
pred <- predict(rf_model, newdata = test)

# confusion matrix
table(Predicted = pred, Actual = test$Outcome_Class)

# accuracy
mean(pred == test$Outcome_Class)

# variable importance
importance(rf_model)
