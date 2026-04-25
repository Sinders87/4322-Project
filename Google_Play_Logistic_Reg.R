# Set to your Downloads folder
setwd("/Users/sukalyandas/Downloads")

# Verify CSV exists
list.files(pattern = "csv")

df <- read.csv("google_play_store_apps_famous.csv")
head(df)
nrow(df)
#1200 apps spread across variety of categories
summary(df$Rating)
#Minimum rating of 2.5 with max being 5

df %>%
  count(Category, sort = TRUE) %>%
  slice_head(n = 10) %>%
  ggplot(aes(x = reorder(Category, n), y = n, fill = Category)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 10 App Categories by Count",
       x = "Category", y = "Number of Apps") +
  theme_minimal() +
  theme(legend.position = "none")

#Most number of apps are games.


df$Installs_num = as.numeric(gsub("[+,]", "", df$Installs))
df$successful = ifelse(df$Installs_num >= 100000, 1, 0)
table(df$successful)
#Defined as successful for now is apps > 100k downloads, (752 apps are said to be successful)

# Train-test split (80/20)
set.seed(42)
n = nrow(df)
train_idx = sample(1:n, size = 0.8*n)
train = df[train_idx, ]
test = df[-train_idx, ]

cat("Training set:", nrow(train), "rows\n")
cat("Test set:", nrow(test), "rows\n")

# Logistic regression
model = glm(successful ~ Category + Rating + Reviews + Size_MB + Type + Price_. + 
              Content_Rating + Genres + Android_Version, data = train, family = binomial)
summary(model)

test$pred_prob = predict(model, newdata = test, type = "response")
test$pred_class = ifelse(test$pred_prob >= 0.5, 1, 0)
accuracy = mean(test$pred_class == test$successful)
cat("Test accuracy:", round(accuracy, 3), "\n")

table(Predicted = test$pred_class, Actual = test$successful)

# Save results
write.csv(test, "model_predictions.csv", row.names = FALSE)
