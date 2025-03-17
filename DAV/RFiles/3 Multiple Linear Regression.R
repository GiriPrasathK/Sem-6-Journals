# Load necessary libraries
library(ggplot2)

# Load dataset
df <- read.csv("Student_Performance.csv")
df
# Define target variable and predictors
df <- df[, !names(df) %in% "Extracurricular Activities"]
y <- df$Performance.Index
x <- df[, !names(df) %in% "Performance.Index"]

# Split data into training and testing sets
library(caTools)
set.seed(123)
train_index <- sample.split(y, SplitRatio= 0.6)
x_train <- x[train_index, ]
y_train <- y[train_index]
x_test <- x[-train_index, ]
y_test <- y[-train_index]


library(FSelector)

# Convert data to a suitable format
df_train <- cbind(y_train, x_train)
df_train$y_train <- as.factor(df_train$y_train)

# Perform chi-square feature selection
scores <- chi.squared(y_train ~ ., data = df_train)
selected_features <- cutoff.k(scores, 2)  # Select top 2 features
x_train_selected <- x_train[, selected_features]
x_test_selected <- x_test[, selected_features]

# Fit multiple linear regression model
model <- lm(y_train ~ ., data = as.data.frame(cbind(y_train, x_train_selected)))

# Make predictions
y_pred <- predict(model, as.data.frame(x_test_selected))

# Evaluate model performance
r2 <- cor(y_test, y_pred)^2
mse <- mean((y_test - y_pred)^2)

# Print results
print(paste("R-squared:", r2))
print(paste("Mean Squared Error:", mse))

# Plot actual vs predicted values and save it
g <- ggplot() +
  geom_point(aes(x = y_test, y = y_pred), color = "blue") +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(title = "Actual vs Predicted", x = "Actual Values", y = "Predicted Values")

ggsave("Actual_vs_Predicted.png", plot = g)

library(plot3D)
if (ncol(x_test_selected) >= 2) {
  png("3D_Plot.png")
  scatter3D(
    x = x_test_selected[, 1],
    y = x_test_selected[, 2],
    z = y_pred,
    colvar = y_pred,
    col = terrain.colors(100),
    pch = 19,
    xlab = "Feature 1",
    ylab = "Feature 2",
    zlab = "Predicted Values",
    main = "3D Scatter Plot"
  )
  dev.off()
}

