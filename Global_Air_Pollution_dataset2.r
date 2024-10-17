
library(ggplot2)
library(dplyr)
library(corrplot)
library(tidyverse)
library(dplyr)
library(gridExtra)
library(extremevalues)
library(caret)
library(rpart)
library(kknn)
library(e1071)

data <- read.csv("C:/Users/Naveen/Documents/Projects/DMML/Datasets/global air pollution dataset.csv")

# Print the data frame
print(data)

# Print information about the data frame
str(data)

summary(data)

dim(data)

# Check for missing values and print the sum
colSums(is.na(data))


ggplot(data, aes(x = `AQI.Value`, y = `PM2.5.AQI.Value`, color = `AQI.Category`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Scatter Plot with Linear Regression Line",
       x = "AQI Value",
       y = "PM2.5 AQI Value",
       color = "AQI Category") +
  theme_minimal()

# Print the modified data frame
#print(data)

data <- data[, c(3,4,5,6,7,8,9,10,11,12)]

# Assuming 'data' is your data frame

# Set up the plot with one column and as many rows as there are selected columns
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

# Loop through columns and create histograms for numeric columns
for (i in 5:(ncol(data) - 1)) {
  if (is.numeric(data[, i])) {
    # Create a new plot for each histogram
    hist(data[, i], main = colnames(data)[i], xlab = "", col = "skyblue", border = "black")
  }
}


# Assuming 'data' is your data frame

# Set up the plot
par(mfrow = c(1, 5), mar = c(4, 4, 2, 1) + 0.1, oma = c(0, 0, 0, 0))

# Loop through columns and create boxplots for numeric columns
for (i in 5:(ncol(data) - 1)) {
  if (is.numeric(data[, i])) {
    # Create a boxplot for each column
    boxplot(data[, i], main = colnames(data)[i], col = "skyblue", border = "black", notch = FALSE)
  }
}

# Adjust layout
mtext("Boxplots for Selected Columns", side = 3, line = 0.5, outer = TRUE, cex = 1.5)

# Reset par settings to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1, oma = c(0, 0, 0, 0))

library(corrplot)

# Subset the data to include only numeric columns
numeric_data <- data[, sapply(data, is.numeric)]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_data)

# Create a heatmap with correlation values annotated
corrplot(cor_matrix, method = "color", type = "upper", tl.col = "black", tl.srt = 45, addCoef.col = "black")



library(dplyr)

# Function to label encode a column if it's a string
label_encode_if_string <- function(column) {
  if (is.character(column)) {
    return(as.factor(column))
  } else {
    return(column)
  }
}

# Apply label encoding to string columns
data <- data %>%
  mutate(across(everything(), label_encode_if_string))

# Check the data types again to verify label encoding
str(data)


data$AQI.Category <- as.numeric(as.factor(data$AQI.Category))
data$CO.AQI.Category <- as.numeric(as.factor(data$CO.AQI.Category))
data$Ozone.AQI.Category <- as.numeric(as.factor(data$Ozone.AQI.Category))
data$NO2.AQI.Category <- as.numeric(as.factor(data$NO2.AQI.Category))
data$PM2.5.AQI.Category <- as.numeric(as.factor(data$PM2.5.AQI.Category))


col <- names(data)
str(data)

# Extract predictor variables (all columns except the last one)
predictor <- col[1:(length(col) - 1)]
print(predictor)

# Extract the target variable (last column)
target <- col[length(col)]
print(target)


library(caret)

# Set seed for reproducibility
set.seed(22245421)

# Create an index for splitting the data
index <- createDataPartition(data[[target]], p = 0.8, list = FALSE)

# Split the data into training and testing sets
train <- data[index, ]
test <- data[-index, ]

# Print the training and testing sets
print(train)
print(test)


library(rpart)

# Set seed for reproducibility
set.seed(22245421)

# Create a decision tree model
model <- rpart(as.formula(paste(target, "~", paste(predictor, collapse = " + "))), data = train, method = "class")

# Print the decision tree
print(model)

# Predictions on Test Data
print("For Test Data")
test_pred <- predict(model, newdata = test, type = "class")

# Confusion Matrix and Accuracy for Test Data
conf_matrix_test <- table(test[[target]], test_pred)
print(conf_matrix_test)
accuracy_test <- sum(diag(conf_matrix_test)) / sum(conf_matrix_test)
print(accuracy_test)

cat("\n")

# Predictions on Train Data
print("For Train Data")
train_pred <- predict(model, newdata = train, type = "class")

# Confusion Matrix and Accuracy for Train Data
conf_matrix_train <- table(train[[target]], train_pred)
print(conf_matrix_train)
accuracy_train <- sum(diag(conf_matrix_train)) / sum(conf_matrix_train)
print(accuracy_train)

# Install and load the required packages
library(rpart.plot)

# Plot the decision tree
prp(model, type = 4, extra = 2, main = "Decision Tree Diagram")

# Load the required packages
library(glmnet)

# Convert AQI Category to binary format in the original dataset
data$AQI_Category_Binary <- ifelse(data$AQI.Category == "Good", 1, 0)

# Check the unique values of the new binary target variable
table(data$AQI_Category_Binary)

# Split the dataset into training and testing sets
index <- createDataPartition(data$AQI_Category_Binary, p = 0.7, list = FALSE)

str(data$AQI_Category_Binary)

table(data$AQI.Category)

# Convert AQI Category to binary format
data$AQI_Category_Binary <- ifelse(data$AQI.Category == 1, 1, 0)

# Check the unique values of the new binary target variable
table(data$AQI_Category_Binary)


# Split the dataset into training and testing sets
index <- createDataPartition(data$AQI_Category_Binary, p = 0.7, list = FALSE)

# Check the length of the index
length(index)

# Split the data into training and testing sets
train <- data[index, ]
test <- data[-index, ]


# Install and load the gbm package

library(gbm)

# Set up the GBM model
gbm_model <- gbm(AQI.Category ~ ., data = train, distribution = "multinomial", n.trees = 1000, interaction.depth = 4, shrinkage = 0.01)

# Print the GBM model summary
print(gbm_model)

# Predictions on Test Data
print("For Test Data")
test_pred_gbm <- predict(gbm_model, newdata = test, type = "response")

# Convert predicted probabilities to class labels
test_pred_class <- apply(test_pred_gbm, 1, which.max)

# Confusion Matrix and Accuracy for Test Data
conf_matrix_test_gbm <- table(test[[target]], test_pred_class)
print(conf_matrix_test_gbm)
accuracy_test_gbm <- sum(diag(conf_matrix_test_gbm)) / sum(conf_matrix_test_gbm)
print(accuracy_test_gbm)

cat("\n")

# Predictions on Train Data
print("For Train Data")
train_pred_gbm <- predict(gbm_model, newdata = train, type = "response")

# Convert predicted probabilities to class labels
train_pred_class <- apply(train_pred_gbm, 1, which.max)

# Confusion Matrix and Accuracy for Train Data
conf_matrix_train_gbm <- table(train[[target]], train_pred_class)
print(conf_matrix_train_gbm)
accuracy_train_gbm <- sum(diag(conf_matrix_train_gbm)) / sum(conf_matrix_train_gbm)
print(accuracy_train_gbm)


# Load the gbm package
library(gbm)

# Set up the GBM model
gbm_model <- gbm(AQI.Category ~ ., data = train, distribution = "multinomial", n.trees = 1000, interaction.depth = 4, shrinkage = 0.01)

# Print the GBM model summary
print(gbm_model)

# Predictions on Test Data
test_pred_gbm <- predict(gbm_model, newdata = test, type = "response")

# Convert predicted probabilities to class labels
test_pred_class <- apply(test_pred_gbm, 1, which.max)

# Confusion Matrix and Accuracy for Test Data
conf_matrix_test_gbm <- table(test[[target]], test_pred_class)
print(conf_matrix_test_gbm)
accuracy_test_gbm <- sum(diag(conf_matrix_test_gbm)) / sum(conf_matrix_test_gbm)
print(accuracy_test_gbm)

# Predictions on Train Data
train_pred_gbm <- predict(gbm_model, newdata = train, type = "response")

# Convert predicted probabilities to class labels
train_pred_class <- apply(train_pred_gbm, 1, which.max)

# Confusion Matrix and Accuracy for Train Data
conf_matrix_train_gbm <- table(train[[target]], train_pred_class)
print(conf_matrix_train_gbm)
accuracy_train_gbm <- sum(diag(conf_matrix_train_gbm)) / sum(conf_matrix_train_gbm)
print(accuracy_train_gbm)

# Predictions on Test Data
test_pred_gbm <- predict(gbm_model, newdata = test, type = "response")

# Convert predicted probabilities to class labels
test_pred_class <- apply(test_pred_gbm, 1, which.max)

# Compute RMSE
rmse <- sqrt(mean((test$AQI.Category - test_pred_class)^2))
print(paste("RMSE:", rmse))


