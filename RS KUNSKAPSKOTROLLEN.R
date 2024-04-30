# Load necessary packages
install.packages("C:/Users/mayas/Downloads/lmvar_1.0.0.tar.gz", repos = NULL, type = "source")
install.packages(c("matrixcalc", "nleqslv", "carData", "tidyverse", "devtools", "corrplot", "lmtest"))

# Load required libraries
library(openxlsx)
library(glmnet)
library(foreach)
library(readxl)
library(caret)
library(ggplot2)
library(leaps)
library(gridExtra)
library(iterators)
library(ISLR2)
library(car)
library(lmtest)
library(rpart)
library(lmvar)

# Specify the path to your Excel file
file_path <- "C:/Users/mayas/OneDrive/Desktop/kunskapskontrollen R/Volkswagen 1.xlsx"

# Read in the Excel file
car_data <- read.xlsx(file_path)

# Display the data
View(car_data)
head(car_data)
dim(car_data)
names(car_data)
str(car_data)
summary(car_data)

# Remove 'Brand' column and create dummy variables
car_data$Brand <- NULL
car_data <- model.matrix(~ . + 0, data = car_data)

# Split the data into training and testing sets
set.seed(123) # For reproducibility
index <- createDataPartition(car_data[, "Price"], p = 0.8, list = FALSE)
train_data <- car_data[index, ]
test_data <- car_data[-index, ]

# Convert train_data to a data frame if it's not already one
train_data <- as.data.frame(train_data)
test_data <- as.data.frame(test_data)


# Create a linear regression model named Linear Regression_modell
Linear_Regression_modell <- lm(Price ~ ., data = train_data)
summary(Linear_Regression_modell)

# Calculate RMSE for the linear regression model
train_predictions_lm <- predict(Linear_Regression_modell, newdata = train_data)
rmse_lm <- sqrt(mean((train_data$Price - train_predictions_lm)^2))

# Create a LASSO model named lasso_modell
set.seed(123)
lasso_modell <- glmnet(as.matrix(train_data[, -which(names(train_data) == "Price")]), train_data$Price, alpha = 1)
plot(lasso_modell)

# Calculate predictions for the test data using the LASSO model
test_predictions_lasso <- predict(lasso_modell, s = best_lambda, newx = as.matrix(test_data[, -which(names(test_data) == "Price")]))


# Calculate RMSE for the LASSO model
cv_lasso <- cv.glmnet(as.matrix(train_data[, -which(names(train_data) == "Price")]), train_data$Price, alpha = 1)
best_lambda <- cv_lasso$lambda.min
lasso_modell <- glmnet(as.matrix(train_data[, -which(names(train_data) == "Price")]), train_data$Price, alpha = 1, lambda = best_lambda)
train_predictions_lasso <- predict(lasso_modell, s = best_lambda, newx = as.matrix(train_data[, -which(names(train_data) == "Price")]))
rmse_lasso <- sqrt(mean((train_data$Price - train_predictions_lasso)^2))

# Validate the model using cross-validation named cv_modell
set.seed(123)
fitControl <- trainControl(method = "cv", number = 10)
cv_modell <- train(Price ~ ., data = train_data, method = "glmnet", trControl = fitControl)
print(cv_modell)

# Plot the relationship between 'Year' and 'Price'
ggplot(data = car_data, aes(x = Year, y = Price)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between Year and Price", x = "Year", y = "Price") +
  theme_minimal()

# Print RMSE for each model
print(paste("RMSE for Linear Regression Model:", rmse_lm))
print(paste("RMSE for LASSO Model:", rmse_lasso))

