# Load necessary libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
library(reshape2)


# Load the data
data <- read_csv("C:/Users/Naveen/Documents/Projects/DMML/archive (14)/Electric_Vehicle_Population_Data.csv")

# Explore the data
head(data)

str(data)
nrow(data)

colnames(data)


unique_makes <- unique(data$Make)
table(data$Make)
table(data$`Electric Vehicle Type`)

# Select numeric columns
numeric_data <- data %>%
  select_if(is.numeric)

# Calculate correlation matrix
correlation_matrix <- cor(numeric_data)

# Visualize correlation matrix using heatmap
library(ggplot2)
library(reshape2)

# Melt correlation matrix for heatmap
melted_correlation <- melt(correlation_matrix)

# Plot heatmap with values
ggplot(melted_correlation, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(label = round(value, 2)), color ="black", size = 3) +  # Add text labels
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1),
                       space = "Lab", name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_fixed() +
  labs(title = "Correlation Heatmap")



# Plotting
pie_data <- table(data$`Electric Vehicle Type`)
colors <- c("pink", "salmon")
pie(pie_data, labels = names(pie_data), col = colors)

barplot(table(data$`Electric Vehicle Type`), col = colors)

top_makes <- head(table(data$Make), 5)
barplot(top_makes, col = c("salmon", "pink", "lightgreen", "gold", "darkgreen"))

top_counties <- head(table(data$County), 10)
barplot(top_counties, col = c("salmon", "pink", "lightgreen", "gold", "lightcoral", "blue", "red", "skyblue", "yellow", "darkgreen"))
title("Top Counties by Vehicle Sales")

king_data <- subset(data, County == "King")
unique_makes_king <- head(table(king_data$Make), 10)
barplot(unique_makes_king, col = c("salmon", "pink", "lightgreen", "gold", "lightcoral", "blue", "red", "skyblue", "yellow", "darkgreen"))
title("Best Selling Cars in King County")

top_models <- head(table(data$Model), 5)
barplot(top_models, col = c("gold", "darkgreen", "salmon", "pink", "lightgreen"))
title("Best-selling Electric Car Models")

cafv_eligibility <- table(data$`Clean Alternative Fuel Vehicle (CAFV) Eligibility`)
colors <- c("skyblue", "salmon", "lightgreen", "gold", "lightcoral")
pie(cafv_eligibility, labels = names(cafv_eligibility), col = colors)

ggplot(data, aes(x = `Electric Range`)) +
  geom_histogram(color = "black", fill = "skyblue", bins = 15) +
  labs(x = "Electric Range", y = "Frequency", title = "Power Reserve")

# Count the frequency of each Electric Vehicle Type
electric_vehicle_counts <- table(data$`Electric Vehicle Type`)

# Plotting the distribution
barplot(electric_vehicle_counts, 
        col = "skyblue", 
        main = "Distribution of Electric Vehicle Types",
        xlab = "Electric Vehicle Type",
        ylab = "Frequency")

# Plotting the histogram of electric range
ggplot(data, aes(x = `Electric Range`)) +
  geom_histogram(color = "black", fill = "skyblue", bins = 15) +
  labs(x = "Electric Range", y = "Frequency", title = "Electric Range Distribution")


# Calculate the top 10 models based on frequency
top_10_models <- data %>%
  count(Model) %>%
  top_n(10, wt = n) %>%
  pull(Model)

# Filter the data for only the top 10 models
data_top_10 <- data %>%
  filter(Model %in% top_10_models)

# Visualization 4: Electric Vehicle Types by Make for Top 10 Models
ggplot(data_top_10, aes(x = Make, fill = `Electric Vehicle Type`)) +
  geom_bar(position = "stack") +
  labs(x = "Make", y = "Count", title = "Electric Vehicle Types by Make (Top 10 Models)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

# Visualization 5: Electric Range Distribution by Electric Vehicle Type for Top 10 Models
ggplot(data_top_10, aes(x = `Electric Range`, fill = `Electric Vehicle Type`)) +
  geom_density(alpha = 0.7) +
  labs(x = "Electric Range", y = "Density", title = "Electric Range Distribution by EV Type (Top 10 Models)") +
  scale_fill_brewer(palette = "Set3")

# Visualization 6: Base MSRP Distribution by Electric Vehicle Type for Top 10 Models
ggplot(data_top_10, aes(x = `Base MSRP`, fill = `Electric Vehicle Type`)) +
  geom_density(alpha = 0.7) +
  labs(x = "Base MSRP", y = "Density", title = "Base MSRP Distribution by EV Type (Top 10 Models)") +
  scale_fill_brewer(palette = "Set3")

# Calculate the frequency of EVs sold in each county
top_counties <- data %>%
  count(County) %>%
  arrange(desc(n)) %>%
  slice(1:5) # Select the top 5 counties with the most sales

# Plot the bar plot
ggplot(top_counties, aes(x = reorder(County, n), y = n)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "County", y = "Number of EVs Sold", title = "Top 5 Counties for Most Selling EVs") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





top_msrp <- head(table(data$`Base MSRP`), 10)
barplot(top_msrp)

# Decision Tree Model
# Assuming you want to predict Electric Vehicle Type based on other variables

# Prepare data
data <- data %>%
  select(-c(1, 11, 12, 13, 14, 15, 16, 17)) # Remove non-numeric columns and unnecessary columns for modeling
data <- na.omit(data) # Remove rows with NA values

# Split data into training and testing sets
set.seed(123)
train_index <- sample(1:nrow(data), 0.7 * nrow(data)) # 70% for training, 30% for testing
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train decision tree model
tree_model <- rpart(`Electric Vehicle Type` ~ ., data = train_data, method = "class")

# Plot the decision tree
rpart.plot(tree_model)

# Relevel the "County" variable in the test data
test_data$County <- factor(test_data$County, levels = levels(train_data$County))
test_data$City <- factor(test_data$City, levels = levels(train_data$City))
test_data$State <- factor(test_data$State, levels = levels(train_data$State))
test_data$Model <- factor(test_data$Model, levels = levels(train_data$Model))

# Predict on test data
predictions <- predict(tree_model, test_data, type = "class")

# Evaluate the model
accuracy <- sum(predictions == test_data$`Electric Vehicle Type`) / nrow(test_data)
print(paste("Accuracy:", accuracy))

# Print the summary of the decision tree model
summary(tree_model)

# Print the importance of each predictor variable
printcp(tree_model)

library(caret)

# Plot the variable importance
# Load the necessary libraries
library(caret)

# Calculate variable importance
var_importance <- varImp(tree_model)

# Check if the tree_model is properly trained
if (is.null(tree_model$variable.importance)) {
  stop("Variable importance calculation failed. Ensure that the model is properly trained.")
}

# Extract variable importance from the model
var_importance <- as.data.frame(tree_model$variable.importance)

# Print var_importance
print(var_importance)


# Extract variable names and importance values
var_names <- names(var_importance)
importance_values <- unlist(var_importance)

# Create a data frame
var_importance_df <- data.frame(variable = var_names,
                                importance = importance_values)

# Plot the variable importance
ggplot(var_importance_df, aes(x = reorder(variable, -importance), y = importance)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Variable", y = "Importance", title = "Variable Importance")



# Visualize the decision tree with variable importance
rpart.plot(tree_model, extra = 104, under = TRUE, faclen = 0, type = 2)


