library(readr)
library(dplyr)
library(randomForest)
library(ggplot2)

# load data
df <- read_csv("data/processed/cleaned_data.csv")

# rename price column
df <- df %>%
  rename(Price = `Price_$`)

# convert categorical variables
df$Category <- as.factor(df$Category)
df$Type <- as.factor(df$Type)
df$Content_Rating <- as.factor(df$Content_Rating)

# create 3-class outcome
df <- df %>%
  mutate(
    Outcome_Class = case_when(
      Installs >= 1000000 & Rating >= 4.2 ~ 3,
      Installs >= 100000 & Rating >= 3.5 ~ 2,
      TRUE ~ 1
    )
  )

df$Outcome_Class <- as.factor(df$Outcome_Class)

# check class counts
table(df$Outcome_Class)

# split data
set.seed(123)
n <- nrow(df)
train_rows <- sample(1:n, size = 0.8 * n)

train <- df[train_rows, ]
test <- df[-train_rows, ]

# random forest model
rf_model <- randomForest(
  Outcome_Class ~ Reviews + Size_MB + Price + Category + Type + Content_Rating,
  data = train,
  ntree = 1000
)

# predictions
pred <- predict(rf_model, newdata = test)

# confusion matrix
table(Predicted = pred, Actual = test$Outcome_Class)

# accuracy
mean(pred == test$Outcome_Class)

# variable importance
importance(rf_model)

# top 3 most successful categories
category_success <- df %>%
  group_by(Category) %>%
  summarise(
    total_apps = n(),
    successful_apps = sum(Outcome_Class == 3),
    success_rate = successful_apps / total_apps
  ) %>%
  filter(total_apps >= 20) %>%
  arrange(desc(success_rate))

head(category_success, 3)

# plot category success rates
ggplot(category_success, aes(x = reorder(Category, success_rate), y = success_rate)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Google Play Store Success Rate by Category",
    x = "Category",
    y = "Success Rate"
  )
