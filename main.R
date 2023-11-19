data = read.csv("water_potability.csv")
names(data)

#Remove null values
data = na.omit(data)

# Initialize the filtered data as the original data
filtered_data <- data

# Loop through each column
for(column_name in names(filtered_data)) {
  # Skip if the column is not numeric
  if(!is.numeric(filtered_data[[column_name]])) next
  
  # Calculate IQR and quartiles
  IQR_value <- IQR(filtered_data[[column_name]], na.rm = TRUE)
  Q1 <- quantile(filtered_data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(filtered_data[[column_name]], 0.75, na.rm = TRUE)
  
  # Define outlier bounds
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Filter out the outliers
  filtered_data <- filtered_data[filtered_data[[column_name]] >= lower_bound & filtered_data[[column_name]] <= upper_bound, ]
}

# The result is in 'filtered_data'


install.packages("caret")
install.packages("randomForest")
library(randomForest)
library(caret)

install.packages("randomForestExplainer")
library(randomForestExplainer)


#Classification
filtered_data$Potability <- as.factor(filtered_data$Potability)

# Set seed for reproducibility
set.seed(42)

# Create a train/test split
trainIndex <- createDataPartition(filtered_data$Potability, p = 0.7, list = FALSE)
train_data <- filtered_data[trainIndex, ]
test_data <- filtered_data[-trainIndex, ]

# Train the model
rf_model <- randomForest(Potability ~ ., filtered_data=train_data, ntree=500, importance=TRUE)

# Make predictions
predictions <- predict(rf_model, newdata=test_data)

# Evaluate model performance
confusionMatrix(predictions, test_data$Potability)
print(paste("Accuracy:", confusionMatrix(predictions, test_data$Potability)$overall['Accuracy']))


# Extract feature importance
importance_scores <- importance(rf_model)

# Print feature importance scores
print(importance_scores)

# Plot feature importance
varImpPlot(rf_model, type = 1, pch = 19, col = "blue", main = "Random Forest - Feature Importance")


#Regression
set.seed(123)  
training_indices <- sample(1:nrow(filtered_data), 0.7 * nrow(filtered_data))
train_data <- filtered_data[training_indices, ]
test_data <- filtered_data[-training_indices, ]

rf_regressor <- randomForest(ph ~ ., data=train_data, ntree=500)


predicted_ph <- predict(rf_regressor, newdata=test_data)


rmse <- sqrt(mean((predicted_ph - test_data$ph)^2))
print(paste("RMSE:", rmse))

r_squared <- cor(predicted_ph, test_data$ph)^2
print(paste("R-squared:", r_squared))

# Load necessary library
if (!require(stats)) install.packages("stats")
library(stats)

# Split data into training and testing sets
set.seed(123)  # for reproducibility
train_indices <- sample(1:nrow(filtered_data), 0.7 * nrow(filtered_data))
train_data <- filtered_data[train_indices, ]
test_data <- filtered_data[-train_indices, ]

# Train Linear Regression Model
lm_model <- lm(ph ~ ., data=train_data)

# Summarize the model
summary(lm_model)

# Make predictions on the test set
lm_predictions <- predict(lm_model, newdata=test_data)

# Evaluate the model
lm_rmse <- sqrt(mean((lm_predictions - test_data$ph)^2))
print(paste("Linear Regression RMSE:", lm_rmse))

# Load necessary library
if (!require(gbm)) install.packages("gbm")
library(gbm)

# Train GBM Model
set.seed(123)  # for reproducibility
gbm_model <- gbm(ph ~ ., 
                 data=train_data, 
                 distribution="gaussian", 
                 n.trees=500, 
                 interaction.depth=3, 
                 shrinkage=0.01,
                 cv.folds=5)

# Summary of the model
summary(gbm_model)

# Make predictions on the test set
gbm_predictions <- predict(gbm_model, newdata=test_data, n.trees=500)

# Evaluate the model
gbm_rmse <- sqrt(mean((gbm_predictions - test_data$ph)^2))
print(paste("GBM RMSE:", gbm_rmse))
