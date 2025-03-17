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
s <- sample.split(df, SplitRatio = 0.6)
trainData <- subset(df,s==TRUE)
testData <- subset(df,s==FALSE)

# Print test set
testData

# Display dataset structure
str(df)

# Summary statistics
summary(df)

# Visualization
# Scatter plot of YearsExperience vs Salary
scatter_plot <- ggplot(df, aes(x = YearsExperience, y = Salary)) + 
  geom_point(color = "blue", alpha = 0.6) +  
  labs(title = "Years of Experience vs Salary", x = "Years of Experience", y = "Salary") 
ggsave("scatter_plot.png", plot = scatter_plot, width = 8, height = 6)

# Histogram of Salary
histogram_plot <- ggplot(df, aes(x = Salary)) + 
  geom_histogram(fill = "white", color = "black", bins = 20, alpha = 0.7) + 
  labs(title = "Salary Distribution", x = "Salary", y = "Count")
ggsave("histogram_plot.png", plot = histogram_plot, width = 8, height = 6)


