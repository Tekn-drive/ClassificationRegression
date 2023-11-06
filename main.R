data = read.csv("phones_data.csv")
data = na.omit(data)

data2 = read.csv("water_potability.csv")
names(data2)
data2 = na.omit(data2)
OS <- table(data$os)

barplot(OS)

brand <- table(data$brand_name)
sum <- brand


brandName <- names(brand)

sum(brand)

piepercent <- round(100*brand / sum(brand), 2)
piepercent
pie(piepercent, label = brandName, cex = 0.6, col = rainbow(length(piepercent)))

#change currency to USD from UAH (avg of 2020, 1 USD = 26.9735 UAH)
data$best_price = data$best_price / 26.9735
data$lowest_price = data$lowest_price / 26.9735
data$highest_price = data$highest_price / 26.9735

data = na.omit(data)
data$brand_name <- factor(data$brand_name)
data$os <- factor(data$os)
data$model_name <- factor(data$model_name)
data$release_date <- factor(data$release_date)

install.packages("dplyr") # Install the package if you haven't already
library(dplyr) # Load the package into R

data <- data %>%
  mutate(os = if_else(os == "", "unknown", os))

# Alternatively, using `replace`
data <- data %>%
  mutate(os = replace(os, os == "", "unknown"))
data$brand_name <- factor(data$brand_name)
data$os <- factor(data$os)
data$model_name <- factor(data$model_name)
data$release_date <- factor(data$release_date)
levels(data$os)

install.packages("randomForest")
library(randomForest)
names(data$brand_name)

#remove column
data$X <- NULL
data$model_name <- NULL
data$release_date <- NULL

levels(data$release_date)

# Load necessary libraries
install.packages("caret")
install.packages("randomForest")
library(randomForest)
library(caret)

# 
# data$brand_name <- factor(data$brand_name)
# 
# # Stratified sampling to ensure all classes are represented in both training and test sets
# set.seed(42)  # for reproducibility
# trainIndex <- createDataPartition(data$brand_name, p = 0.7, list = FALSE)
# train_data <- data[trainIndex, ]
# test_data <- data[-trainIndex, ]
# 
# # Ensure that factor levels with no instances are dropped
# train_data$brand_name <- droplevels(train_data$brand_name)
# test_data$brand_name <- droplevels(test_data$brand_name)
# 
# # Fit the random forest model
# # Notice that you might need to adjust ntree and other parameters according to your specific needs
# set.seed(42)  # for reproducibility of random forest model
# rf_model <- randomForest(brand_name ~ ., data=train_data, ntree=500, importance=TRUE)
# 
# # Make predictions on the test data
# predictions <- predict(rf_model, newdata=test_data)
# 
# # Evaluate the model's performance
# confusion_matrix <- table(test_data$brand_name, predictions)
# print(confusion_matrix)
# accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
# print(paste("Accuracy:", accuracy))

#Boah O Woah
data2$Potability <- as.factor(data2$Potability)

# Load necessary library
if (!require("caret")) install.packages("caret")
library(caret)

# Set seed for reproducibility
set.seed(42)

# Create a train/test split
trainIndex <- createDataPartition(data2$Potability, p = 0.7, list = FALSE)
train_data <- data2[trainIndex, ]
test_data <- data2[-trainIndex, ]

# Load the randomForest package
library(randomForest)

# Train the model
rf_model <- randomForest(Potability ~ ., data=train_data, ntree=500, importance=TRUE)

# Make predictions
predictions <- predict(rf_model, newdata=test_data)

# Evaluate model performance
confusionMatrix(predictions, test_data$Potability)
print(paste("Accuracy:", confusionMatrix(predictions, test_data$Potability)$overall['Accuracy']))

