# Load the dataset
splicing_data <- read.csv("C:/Users/amrut/OneDrive/Documents/R/project/splicing_data.csv")

# Summary statistics
summary(splicing_data)

# Check for missing values
missing_values <- colSums(is.na(splicing_data))
missing_values

# Create boxplots for numerical variables to identify outliers
bp <- boxplot(splicing_data[, -1], plot = FALSE)  # Excluding the first column if it's an identifier

# Get the outlier values
outlier_values <- bp$out

# Print the outlier values
cat("Outlier Values:\n")
print(outlier_values)



# corelational matrices between splicing factors and splicing events
cor.test(splicing_data $SplicingFactor1, splicing_data$SplicingEvent, alternative = "less", method = "pearson", conf.level = 0.95)
cor.test(splicing_data $SplicingFactor2, splicing_data$SplicingEvent, alternative = "less", method = "pearson", conf.level = 0.95)
cor.test(splicing_data $SplicingFactor3, splicing_data$SplicingEvent, alternative = "less", method = "pearson", conf.level = 0.95)

# Compute the correlation between SplicingFactor1 and SplicingEvent
correlation1 <- cor(splicing_data$SplicingFactor1, splicing_data$SplicingEvent, method = "pearson")

# Compute the correlation between SplicingFactor2 and SplicingEvent
correlation2 <- cor(splicing_data$SplicingFactor2, splicing_data$SplicingEvent, method = "pearson")

# Compute the correlation between SplicingFactor3 and SplicingEvent
correlation3 <- cor(splicing_data$SplicingFactor3, splicing_data$SplicingEvent, method = "pearson")

# Print the correlation values
cat("Correlation between SplicingFactor1 and SplicingEvent:", correlation1, "\n")
cat("Correlation between SplicingFactor2 and SplicingEvent:", correlation2, "\n")
cat("Correlation between SplicingFactor3 and SplicingEvent:", correlation3, "\n")


library(ggplot2)

# Explore the distribution of splicing factors

ggplot(splicing_data, aes(x = SplicingFactor1)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Distribution of Factor1")

ggplot(splicing_data, aes(x = SplicingFactor2)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Distribution of Factor2")

ggplot(splicing_data, aes(x = SplicingFactor3)) +
  geom_histogram(binwidth = 0.5, fill = "red", color = "black") +
  labs(title = "Distribution of Factor3")

# Explore the distribution of the splicing event
ggplot(splicing_data, aes(x = SplicingEvent)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black") +
  labs(title = "Distribution of Splicing Event")

library(ggplot2)

# Scatter Plot: SplicingFactor1 vs. Splicing Event with Regression Line and Confidence Intervals
plot_factor1 <- ggplot(splicing_data, aes(x = SplicingFactor1, y = SplicingEvent)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +
  labs(title = "Scatter Plot: SplicingFactor1 vs. Splicing Event with Regression Line")
plot_factor1

Model.1 <- lm(splicing_data$SplicingEvent ~ splicing_data$SplicingFactor1)
summary(Model.1)

# Scatter Plot: SplicingFactor2 vs. Splicing Event with Regression Line and Confidence Intervals
plot_factor2 <- ggplot(splicing_data, aes(x = SplicingFactor2, y = SplicingEvent)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "lightcoral") +
  labs(title = "Scatter Plot: SplicingFactor2 vs. Splicing Event with Regression Line")
plot_factor2
Model.2 <- lm(splicing_data$SplicingEvent ~ splicing_data$SplicingFactor2)
summary(Model.2)

# Scatter Plot: SplicingFactor3 vs. Splicing Event with Regression Line and Confidence Intervals
plot_factor3 <- ggplot(splicing_data, aes(x = SplicingFactor3, y = SplicingEvent)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red", fill = "lightcoral") +
  labs(title = "Scatter Plot: SplicingFactor3 vs. Splicing Event with Regression Line")
plot_factor3
Model.3 <- lm(splicing_data$SplicingEvent ~ splicing_data$SplicingFactor3)
summary(Model.3)



# Create custom categories based on SplicingFactor1 values

splicing_data$SplicingFactor1Category <- ifelse(splicing_data$SplicingFactor1 >= 2.691 & splicing_data$SplicingFactor1 <= 4.69, "Category 1",
                                                ifelse(splicing_data$SplicingFactor1 > 4.69 & splicing_data$SplicingFactor1 <= 6.69 , "Category 2",
                                                       ifelse(splicing_data$SplicingFactor1 > 6.69 & splicing_data$SplicingFactor1 <= 7.187 , "Category 3", "Category 4")))

# Create a box plot to compare splicing event values across different levels of SplicingFactor1
ggplot(splicing_data, aes(x = SplicingFactor1Category, y = SplicingEvent)) +
  geom_boxplot(fill = "red") +
  labs(title = "Box Plot: Splicing Event by SplicingFactor1 Categories")

# Extract and view the values of SplicingFactor1 in individual categories
category_values <- split(splicing_data$SplicingFactor1, splicing_data$SplicingFactor1Category)

# Print the summary of values in each category
lapply(category_values, summary)

# Create custom categories based on SplicingFactor2 values
splicing_data$SplicingFactor2Category <- ifelse(splicing_data$SplicingFactor2 >= 4.920 & splicing_data$SplicingFactor2 < 7.92, "Category 1",
                                                ifelse(splicing_data$SplicingFactor2 >= 7.92 & splicing_data$SplicingFactor2 < 10.92 , "Category 2",
                                                       ifelse(splicing_data$SplicingFactor2 >= 10.92 & splicing_data$SplicingFactor2 <= 12.862 , "Category 3", "Category 4")))


# Create a box plot to compare splicing event values across different levels of SplicingFactor2
ggplot(splicing_data, aes(x = SplicingFactor2Category, y = SplicingEvent)) +
  geom_boxplot(fill = "red") +
  labs(title = "Box Plot: Splicing Event by SplicingFactor2 Categories")

# Extract and view the values of SplicingFactor2 in individual categories
category_values2 <- split(splicing_data$SplicingFactor2, splicing_data$SplicingFactor2Category)


# Print the summary of values in each category
lapply(category_values2, summary)



# Create custom categories based on SplicingFactor3 values
splicing_data$SplicingFactor3Category <- ifelse(splicing_data$SplicingFactor3 >= 2.59 & splicing_data$SplicingFactor3 < 3.92, "Category 1",
                                                ifelse(splicing_data$SplicingFactor3 >= 3.92 & splicing_data$SplicingFactor3 < 4.92 , "Category 2",
                                                       ifelse(splicing_data$SplicingFactor3 >= 4.92 & splicing_data$SplicingFactor3 <= 5.83 , "Category 3", "Category 4")))

# Create a box plot to compare splicing event values across different levels of SplicingFactor3
ggplot(splicing_data, aes(x = SplicingFactor3Category, y = SplicingEvent)) +
  geom_boxplot(fill = "red") +
  labs(title = "Box Plot: Splicing Event by SplicingFactor3 Categories")

# Extract and view the values of SplicingFactor3 in individual categories
category_values3 <- split(splicing_data$SplicingFactor3, splicing_data$SplicingFactor3Category)


# Print the summary of values in each category
lapply(category_values3, summary)





# Load necessary libraries

install.packages("caret")
# Load necessary libraries
library(caret)  # For data splitting, model training, and evaluation

# Set a seed for reproducibility
set.seed(123)

# Load necessary libraries
library(caret)  # For data splitting and model evaluation

# Set a seed for reproducibility
set.seed(123)

# Split the dataset into training (70%) and testing (30%) sets
splitIndex <- createDataPartition(splicing_data$SplicingEvent, p = 0.7, list = FALSE)
train_data <- splicing_data[splitIndex, ]
test_data <- splicing_data[-splitIndex, ]

# Build a linear regression model
model <- lm(SplicingEvent ~ SplicingFactor1 + SplicingFactor2 + SplicingFactor3, data = train_data)
summary(model)

# Make predictions on the testing set
predictions <- predict(model, newdata = test_data)

# Extract actual values
actual_values <- test_data$SplicingEvent


# Combine actual and predicted values into a data frame
comparison_df <- cbind(Actual = actual_values, Predicted = predictions)

# Print the combined data frame
print(comparison_df)
library(ggplot2)

# Combine actual and predicted values into a data frame
comparison_df <- data.frame(Actual = actual_values, Predicted = predictions)

# Create a scatterplot with a fitted line
ggplot(comparison_df, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Actual vs. Predicted Values") +
  xlab("Actual Values") +
  ylab("Predicted Values")



# Calculate R-squared
rsquared <- 1 - sum((test_data$SplicingEvent - predictions)^2) / sum((test_data$SplicingEvent - mean(test_data$SplicingEvent))^2)

# Calculate Mean Squared Error (MSE)
mse <- mean((test_data$SplicingEvent - predictions)^2)

# Print the evaluation metrics
cat("R-squared:", rsquared, "\n")
cat("Mean Squared Error:", mse, "\n")
