# Reading the data from a CSV file
df <- read.csv("C:/Users/Naveen/Documents/Projects/DMML/car_price_prediction.csv")

#Displaying the First 10 Rows
head(df, 10)

# Display the structure of the data frame
str(df)

# Displaying the shape of the dataframe
shape <- dim(df)
# Total number of rows in the Dataframe is 19237 rows and 18 columns 
shape

# Checking if there are duplicate in datagframe, there are 313 number of duplicated rows in the dataset 
num_duplicates <- sum(duplicated(df))
cat("Number of duplicated rows:", num_duplicates, "\n")

# Using unique function getting the unique rows in the dataframe 
df <- unique(df)

# Verifying if there are any duplicates, output below confirm that the duplicate are removed 
missing_values <- colSums(is.na(df))
print(missing_values)

#Getting the Summary of the Dataset
summary(df)

head(df, 10)

#Looks like Door does not add value information to dataset
unique(df$Doors)

#ID is not related to Car Price 
unique(df$ID)

#Droping ID and Doors in ID column
df <- df[, !(colnames(df) %in% c("ID", "Doors"))]

# Convert Levy column to character
df$Levy <- as.character(df$Levy)

# Replace '-' with '0'
df$Levy[df$Levy == '-'] <- '0'

# Convert Levy column to numeric
df$Levy <- as.numeric(df$Levy)

# Print the updated dataframe
print(df)
str(df$`Prod..year`)

# Replace missing values in 'Prod. year' with a default value 0
df$`Prod..year`[is.na(df$`Prod..year`)] <- 0

current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Calculate the age and create a new column 'Age'
df$Age <- current_year - df$`Prod..year`

#Drop the 'Prod..year' column is not 
df <- df[, !colnames(df) %in% c("Prod..year")]

#Displaying the First 10 Rows
head(df, 10)

#Converting km to integer 
unique_values <- unique(df$Mileage)
print(unique_values)

df$Mileage <- gsub('km', '', df$Mileage)

df$Mileage <- as.integer(df$Mileage)

df

# Replace 'Turbo' with ''
df$`Engine.volume` <- gsub('Turbo', '', df$`Engine.volume`)

# Convert 'Engine volume' column to numeric
df$`Engine.volume` <- as.numeric(df$`Engine.volume`)


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


#Getting the top Preferred category
top_categories <- df %>%
  group_by(Category) %>%
  summarise(count = n()) %>%
  top_n(5, count) %>%
  pull(Category)

print(top_categories)
# Create the plot with the top 5 categories
df %>%
  filter(Category %in% top_categories) %>%
  ggplot(aes(x = Category, fill = Category)) +
  geom_bar() +
  scale_fill_manual(values = c("#FF9999", "#66B2FF", "#99FF99", "#FFCC99", "#c2c2f0")) +  # Custom colors
  theme_minimal() +
  labs(title = "Top 5 Categories", fontsize = 20) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = FALSE)  

#Most type of Car Preferred by people is Sedan, Hatchback, and Jeep.

#Getting the top Colour car
top_colors <- df %>%
  group_by(Color) %>%
  summarise(count = n()) %>%
  top_n(10, count) %>%
  pull(Color)

df %>%
  filter(Color %in% top_colors) %>%
  ggplot(aes(x = Color, fill = Color)) +
  geom_bar() +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6")) +  # Custom dark colors
  theme_minimal() +
  labs(title = "Top Car Colors", fontsize = 20) +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = FALSE)  

#Most sold car colour are black, silver, white and grey


get_dark_colors <- function(n) {
  dark_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6")
  return(dark_palette[1:n])
}

df %>%
  ggplot(aes(x = Fuel.type, fill = Fuel.type)) +
  geom_bar() +
  scale_fill_manual(values = get_dark_colors(length(unique(df$Fuel.type)))) +  # Custom dark colors
  theme_minimal() +
  labs(title = "Count of Fuel Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = FALSE)

# Function to generate dark colors
get_dark_colors <- function(n) {
  dark_palette <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f", "#cab2d6")
  return(dark_palette[1:n])
}

# Create the pie chart for the count of Gear Box Type with dark colors
df %>%
  group_by(Gear.box.type) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = "", y = count, fill = Gear.box.type)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y") +
  scale_fill_manual(values = get_dark_colors(n_distinct(df$Gear.box.type))) +  # Custom dark colors
  theme_void() +
  labs(title = "Count of Gear Box Type") +
  theme(plot.title = element_text(hjust = 0.5))

#People are preferring more automatic car compared to use of normal car.

# Function to generate different colors
get_different_colors <- function(n) {
  # You can modify this color palette if needed
  different_palette <- rainbow(n)
  return(different_palette)
}

# Count occurrences of each manufacturer and select the top 10
top_manufacturers <- df %>%
  group_by(Manufacturer) %>%
  summarise(count = n()) %>%
  top_n(10, count)

print(top_manufacturers)
#Hyundai is the top brand


# Select numeric columns only
numeric_data <- df[, sapply(df, is.numeric)]

# Compute the correlation matrix
cor_matrix <- cor(numeric_data)

print(cor_matrix)

# Plot the correlation matrix as a heatmap with shades of blue
corrplot(cor_matrix, method = "color", col = colorRampPalette(c("#08306b", "#2171b5", "#6baed6", "#bdd7e7", "#eff3ff"))(50))

numeric_columns <- sapply(df, is.numeric)
numeric_data <- df[, numeric_columns]

# Removing Outliers 
remove_outliers_iqr <- function(x, k = 1.5) {
  q <- boxplot.stats(x)$stats
  iqr <- IQR(x)
  lower_bound <- q[2] - k * iqr
  upper_bound <- q[4] + k * iqr
  return(between(x, lower_bound, upper_bound))
}

# Remove Outliers from numeric columns
cleaned_data <- df %>%
  filter(across(where(is.numeric), ~remove_outliers_iqr(.)))

summary(cleaned_data)

#Label encoding
cleaned_data$Manufacturer <- as.numeric(factor(cleaned_data$Manufacturer))
cleaned_data$Model <- as.numeric(factor(cleaned_data$Model))
cleaned_data$Category <- as.numeric(factor(cleaned_data$Category))
cleaned_data$Leather.interior <- as.numeric(factor(cleaned_data$Leather.interior))
cleaned_data$Fuel.type <- as.numeric(factor(cleaned_data$Fuel.type))
cleaned_data$Gear.box.type <- as.numeric(factor(cleaned_data$Gear.box.type))
cleaned_data$Drive.wheels <- as.numeric(factor(cleaned_data$Drive.wheels))
cleaned_data$Wheel <- as.numeric(factor(cleaned_data$Wheel))
cleaned_data$Color <- as.numeric(factor(cleaned_data$Color))

#Splitting X and Y
X <- cleaned_data %>% select(-Price)

# Extract the 'Price' column as the target variable (y)
y <- cleaned_data$Price

# Set seed for reproducibility
set.seed(123)

# Define the control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

# Split the data into training (80%) and testing (20%) sets
train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
x_train <- X[train_indices, ]
y_train <- y[train_indices]
x_test <- X[-train_indices, ]
y_test <- y[-train_indices]

#Model 1 Linear Regression 
lm_model <- train(x = x_train, y = y_train, method = "lm", trControl = ctrl)
lm_r_squared <- lm_model$results$Rsquared[1]

summary(lm_model)
cat("R-squared for Linear Regression:", lm_r_squared, "\n")

#Model 2 Decision Tree
dt_model <- train(x = x_train, y = y_train, method = "rpart", trControl = ctrl)
dt_r_squared <- dt_model$results$Rsquared[1]
summary(dt_model)
cat("R-squared for Decision Tree:", dt_r_squared, "\n")

#Model 3 Support Vector Machine (SVM)
svm_model <- train(x = x_train, y = y_train, method = "svmRadial", trControl = ctrl)
svm_predictions <- predict(svm_model, newdata = x_test)
summary(svm_model)

# Evaluate Support Vector Machine (SVM)
svm_r_squared <- cor(svm_predictions, y_test)
cat("Support Vector Machine (SVM) Metrics:\n")
cat("R-squared:", svm_r_squared, "\n")

# Predictions
lm_predictions <- predict(lm_model, newdata = x_test)
dt_predictions <- predict(dt_model, newdata = x_test)


# Scatter plot
plot(y_test, lm_predictions, main = "Linear Regression Predictions vs Actual",
     xlab = "Actual Values", ylab = "Predicted Values", pch = 16, col = "blue")
abline(0, 1, col = "red", lwd = 2)  # Diagonal line for reference

# Add a legend
legend("topright", legend = c("Linear Regression", "Diagonal Line"),
       col = c("blue", "red"), pch = c(16, NA), lwd = c(NA, 2))

# Decision Tree Predictions
plot(y_test, dt_predictions, main = "Decision Tree Predictions vs Actual",
     xlab = "Actual Values", ylab = "Predicted Values", pch = 16, col = "green")
abline(0, 1, col = "red", lwd = 2) 

# Diagonal line for reference

# Add a legend
legend("topright", legend = c("Decision Tree", "Diagonal Line"),
       col = c("green", "red"), pch = c(16, NA), lwd = c(NA, 2))

plot(y_test, svm_predictions, main = "SVM Predictions vs Actual",
     xlab = "Actual Values", ylab = "Predicted Values", pch = 16, col = "brown")
abline(0, 1, col = "red", lwd = 2)
legend("topright", legend = c("SVM", "Diagonal Line"),
       col = c("brown", "red"), pch = c(16, NA), lwd = c(NA, 2))

# Calculate RMSE for Linear Regression
lm_rmse <- sqrt(mean((lm_predictions - y_test)^2))
cat("RMSE for Linear Regression:", lm_rmse, "\n")

# Calculate RMSE for Decision Tree
dt_rmse <- sqrt(mean((dt_predictions - y_test)^2))
cat("RMSE for Decision Tree:", dt_rmse, "\n")

# Calculate RMSE for Support Vector Machine (SVM)
svm_rmse <- sqrt(mean((svm_predictions - y_test)^2))
cat("RMSE for Support Vector Machine (SVM):", svm_rmse, "\n")


