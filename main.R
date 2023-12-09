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
rf_model <- randomForest(Potability ~ ., data=train_data, ntree=500, importance=TRUE)

# Make predictions
predictions <- predict(rf_model, newdata=test_data)

# Evaluate model performance
confusionMatrix(predictions, test_data$Potability)
print(paste("Accuracy:", confusionMatrix(predictions, test_data$Potability)$overall['Accuracy']))


# Extract feature importance
importance_scores <- importance(rf_model)

# Print feature importance scores
print(importance_scores)
varImpPlot(rf_model, type = 1, pch = 19, col = "blue", main = "Random Forest - Feature Importance")
