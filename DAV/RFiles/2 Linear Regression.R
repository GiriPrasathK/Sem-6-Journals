# Load necessary libraries
library(ggplot2)
library(caTools)

# Load the dataset
df <- read.csv("Salary_dataset.csv")

# Display first few rows
head(df, 5)

# Check number of unique values
sapply(df, function(x) length(unique(x)))

# Check for missing values
colSums(is.na(df))

# Print dataset shape
dim(df)

# Split data into training and testing sets
set.seed(40)
s<-sample.split(df,SplitRatio=0.4)
testData<-subset(df,s==TRUE)
trainData<-subset(df,s==FALSE)

# Print test set
testData

# Display dataset structure
str(df)

# Summary statistics
summary(df)

# Linear Regression Model
model <- lm(Salary ~ YearsExperience, data = trainData)
summary(model)  # Display model summary

# Predict on test data
testData$Predicted_Salary <- predict(model, testData)

# Calculate RMSE and R-squared
rmse <- sqrt(mean((testData$Salary - testData$Predicted_Salary)^2))
r_squared <- summary(model)$r.squared

# Print performance metrics
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Visualization
# Scatter plot of YearsExperience vs Salary
scatter_plot <- ggplot(df, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color = "black", alpha = 0.6) + 
  labs(title = "Years of Experience vs Salary", x = "Years of Experience", y = "Salary") 
ggsave("scatter_plot1.png", plot = scatter_plot, width = 8, height = 6)

# Histogram of Salary
histogram_plot <- ggplot(df, aes(x = Salary)) + 
  geom_histogram(fill = "gray", color = "black", bins = 20, alpha = 0.7) + 
  labs(title = "Salary Distribution", x = "Salary", y = "Count") 
ggsave("histogram_plot1.png", plot = histogram_plot, width = 8, height = 6)

# Plot actual vs predicted salaries
regression_plot <- ggplot(testData, aes(x = Salary, y = Predicted_Salary)) + 
  geom_point(color = "blue", alpha = 0.6) + 
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + 
  labs(title = "Actual vs Predicted Salary", x = "Actual Salary", y = "Predicted Salary")
ggsave("regression_plot.png", plot = regression_plot, width = 8, height = 6)
