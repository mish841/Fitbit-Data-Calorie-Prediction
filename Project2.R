library(klaR)
library(naivebayes)
library(rsample)
library(rpart)
library(rpart.plot)
library(caret)
library(ggplot2)
library(e1071)

# Read the data
data <- read.csv("activity_data_heartrate.csv")

# Check structure and first few rows of the data
str(data)
head(data)

# Create HighCalories variable
set.seed(123)
ids <- c("Group 1", "Group 2", "Group 3")
data$HighCalories <- ifelse(data$Calories > 2500, "High", "Low")

# Create a data frame for modeling
data_frame <- data.frame(
  Id = rep(ids, each = 100),
  Calories = c(rnorm(100, mean = 2000, sd = 200),
               rnorm(100, mean = 2500, sd = 200),
               rnorm(100, mean = 3000, sd = 200)),
  Heart_rate = c(rnorm(100, mean = 60, sd = 5),
                 rnorm(100, mean = 70, sd = 5),
                 rnorm(100, mean = 80, sd = 5))
)

# Check the data frame
head(data_frame)

# Plot Calories vs. Heart_rate
ggplot(data_frame, aes(x = Calories, y = Heart_rate, color = Id)) +
  geom_point() +
  labs(title = "Calories vs. Heart_rate", x = "Calories", y = "Heart_rate (bpm)") +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(color = guide_legend(title = "Group ID"))

# Split data for modeling
data_tree <- rpart(Heart_rate ~ Calories, data = data)
train_indices <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Build rpart model
rpart_model <- rpart(HighCalories ~ Calories + Heart_rate, data = train_data)
rpart.plot(rpart_model, main = "Decision Tree: Calories and Heart_rate")

# Make predictions with rpart model
rpart_predictions <- predict(rpart_model, test_data, type = "class")
rpart_confusion <- table(Predicted = rpart_predictions, Actual = test_data$HighCalories)
rpart_accuracy <- sum(diag(rpart_confusion)) / sum(rpart_confusion)

# Build Naive Bayes model
naive_bayes_model <- naiveBayes(HighCalories ~ Calories + Heart_rate, data = train_data)
naive_bayes_predictions <- predict(naive_bayes_model, test_data)
naive_bayes_confusion <- table(Predicted = naive_bayes_predictions, Actual = test_data$HighCalories)
naive_bayes_accuracy <- sum(diag(naive_bayes_confusion)) / sum(naive_bayes_confusion)

# Print results
cat("Confusion Matrix for rpart:\n")
print(rpart_confusion)
cat("Accuracy for rpart:", rpart_accuracy * 100, "%\n")

cat("Confusion Matrix for Naive Bayes:\n")
print(naive_bayes_confusion)
cat("Accuracy for Naive Bayes:", naive_bayes_accuracy * 100, "%\n")

