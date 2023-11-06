data = read.csv("water_potability.csv")
names(data)
data = na.omit(data)

install.packages("caret")
install.packages("randomForest")
library(randomForest)
library(caret)

#Boah O Woah
data$Potability <- as.factor(data$Potability)

# Set seed for reproducibility
set.seed(42)

# Create a train/test split
trainIndex <- createDataPartition(data$Potability, p = 0.7, list = FALSE)
train_data <- data[trainIndex, ]
test_data <- data[-trainIndex, ]

# Train the model
rf_model <- randomForest(Potability ~ ., data=train_data, ntree=500, importance=TRUE)

# Make predictions
predictions <- predict(rf_model, newdata=test_data)

# Evaluate model performance
confusionMatrix(predictions, test_data$Potability)
print(paste("Accuracy:", confusionMatrix(predictions, test_data$Potability)$overall['Accuracy']))


#Regression
data$Potability <- as.numeric(data2$Potability)  # converting to numeric if it's not already
if (!require("caret")) install.packages("caret")
library(caret)

# Set seed for reproducibility
set.seed(42)

# Create a train/test split
trainIndex <- createDataPartition(data2$Potability, p = 0.7, list = FALSE)
train_data <- data2[trainIndex, ]
test_data <- data2[-trainIndex, ]

# Train the regression model
rf_regr_model <- randomForest(Potability ~ ., data=train_data, ntree=500)
# Make predictions
predictions <- predict(rf_regr_model, newdata=test_data)
# Calculate RMSE
rmse <- RMSE(predictions, test_data$Potability)
print(paste("RMSE:", rmse))

# Calculate R-squared
r_squared <- cor(predictions, test_data$Potability)^2
print(paste("R-squared:", r_squared))
