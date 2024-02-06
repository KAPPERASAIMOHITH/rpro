matrixdata<-matrix(c(1:30),nrow=5)
row_means<-rowMeans(matrixdata)
row_means
medians<-apply(matrixdata,1,median)
print(medians)
sumofcol<-sum(matrixdata[,1:2])
cat("sum of the first 2 columns")
print(sumofcol)




data <- c(21, 62, 10, 53)
labels <- c('London', 'New York', 'Singapore', 'Mumbai')
pie(data, labels = labels, main = 'City Pie-Chart', col = rainbow(length(data)), cex = 0.8)
legend('topright', legend = labels, fill = rainbow(length(data)), cex = 0.8, title = 'Cities')





factorial_function <- function(n) {
  if (n == 0 || n == 1) {
    return(1)
  } else {
    return(n * factorial_function(n - 1))
  }
}
number <- 5
result <- factorial_function(number)
cat(paste("Factorial of", number, "is:", result))




sum_of_natural_numbers <- 0
for (i in 1:10) {
  sum_of_natural_numbers <- sum_of_natural_numbers + i
}
cat("Sum of natural numbers up to 10 is:", sum_of_natural_numbers)

dev.off()


# Load required libraries and the Titanic dataset
library(ggplot2)
data("Titanic")

# Convert the Titanic dataset to a data frame
titanic_df <- as.data.frame(Titanic)

# (a) Bar chart showing survival based on passenger class
ggplot(titanic_df, aes(x = Class, fill = Survived)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Survival on the Titanic by Passenger Class",
       x = "Passenger Class",
       y = "Count",
       fill = "Survived")

# (b) Modified bar chart based on gender of people who survived
ggplot(titanic_df, aes(x = Class, fill = Survived, color = Sex)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Survival on the Titanic by Passenger Class and Gender",
       x = "Passenger Class",
       y = "Count",
       fill = "Survived",
       color = "Gender")

# (c) Histogram plot to show the distribution of feature "Age"
ggplot(titanic_df, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Age on the Titanic",
       x = "Age",
       y = "Count")

# Function to check if a number is prime
is_prime <- function(n) {
  if (n <= 1) {
    return(FALSE)
  }
  
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}

# Example usage
number_to_check <- 17  # Replace this with the number you want to check
if (is_prime(number_to_check)) {
  cat(number_to_check, "is a prime number.\n")
} else {
  cat(number_to_check, "is not a prime number.\n")
}

H <- c(7, 12, 28, 3, 41)
M <- c("mar", "apr", "may", "jun", "jul")

# Create a box plot
boxplot(H ~ M, main = "Revenue chart", xlab = "Month", ylab = "Revenue")



library(reshape2)
data("ChickWeight")
ordered_data <- ChickWeight[order(ChickWeight$Diet, ChickWeight$weight), ]
last_6_records <- tail(ordered_data, 6)
cat("Last 6 records:\n")
print(last_6_records)
melted_data <- melt(ChickWeight, id.vars = c("Chick", "Time", "Diet"))
cast_mean <- dcast(melted_data, Diet ~ variable, mean)
cast_mode <- dcast(melted_data, Diet ~ variable, mode)
cat("\n(ii.b) Mean value of weight grouped by Diet:\n")
print(cast_mean)

cat("\n(ii.c) Mode of weight grouped by Diet:\n")
print(cast_mode)


# Create two vectors
vector1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
vector2 <- c(10, 11, 12, 13, 14, 15, 16, 17, 18)

# Combine vectors into a 3x3 matrix
matrix1 <- matrix(vector1, nrow = 3, ncol = 3, byrow = TRUE)
matrix2 <- matrix(vector2, nrow = 3, ncol = 3, byrow = TRUE)

# Create an array from the matrices
array_of_matrices <- array(c(matrix1, matrix2), dim = c(3, 3, 2))

# Print the array
cat("Array of two 3x3 matrices:\n")
print(array_of_matrices)

# Print the second row of the second matrix
cat("\nSecond row of the second matrix:\n")
print(array_of_matrices[2, , 2])

# Print the element in the 3rd row and 3rd column of the 1st matrix
cat("\nElement in the 3rd row and 3rd column of the 1st matrix:\n")
print(array_of_matrices[3, 3, 1])



age <- c(9, 13, 21, 8, 36, 22, 12, 41, 31, 33, 19)

# Create a histogram
hist(age, col = "skyblue", border = "black", main = "Age Histogram",
     xlab = "Age", ylab = "Frequency", xlim = c(0, 50), ylim = c(0, 4), breaks = 10)

# Add a title to the chart
title(main = "Age", col.main = "darkblue", font.main = 4)






# Load required library
library(reshape2)

# Load the airquality dataset
data(airquality)

# (a) Melt 'airquality' dataset and display as a long-format data
melted_data_a <- melt(airquality)
cat("(a) Melted 'airquality' dataset in long-format:\n")
print(melted_data_a)

# (b) Melt 'airquality' data and specify month and day as "ID variables"
melted_data_b <- melt(airquality, id.vars = c("Month", "Day"))
cat("\n(b) Melted 'airquality' dataset with Month and Day as ID variables:\n")
print(melted_data_b)

# (c) Cast the molten 'airquality' dataset
casted_data <- dcast(melted_data_b, Month + Day ~ variable, value.var = "value")
cat("\n(c) Casted 'airquality' dataset:\n")
print(casted_data)

# (d) Use cast function appropriately and compute the average per month
averages_per_month <- dcast(melted_data_b, Month ~ variable, fun.aggregate = mean)
cat("\n(d) Averages of Ozone, Solar, Wind, and Temperature per month:\n")
print(averages_per_month)



# Load the required library
library(ggplot2)

# Load the airquality dataset
data(airquality)

# Create a boxplot for ozone readings
ggplot(airquality, aes(x = "Ozone", y = Ozone, fill = "Ozone")) +
  geom_boxplot() +
  labs(title = "Boxplot of Ozone Readings",
       x = "Variable",
       y = "Ozone",
       fill = "Ozone") +
  theme_minimal()


# i) Sort a vector in ascending and descending order
vector_to_sort <- c(15, 8, 27, 42, 12)

# Sorting in ascending order
sorted_vector_asc <- sort(vector_to_sort)

# Sorting in descending order
sorted_vector_desc <- sort(vector_to_sort, decreasing = TRUE)

cat("(i) Sorted vector in ascending order:", sorted_vector_asc, "\n")
cat("    Sorted vector in descending order:", sorted_vector_desc, "\n\n")

# ii) Find sum, mean, and product of vectors
vector_for_operations <- c(5, 10, 15, 20, 25)

sum_vector <- sum(vector_for_operations)
mean_vector <- mean(vector_for_operations)
product_vector <- prod(vector_for_operations)

cat("(ii) Sum of the vector:", sum_vector, "\n")
cat("     Mean of the vector:", mean_vector, "\n")
cat("     Product of the vector:", product_vector, "\n\n")

# iii) Create a sequence of numbers and find the mean and sum of specified ranges
sequence_numbers <- seq(20, 60)
mean_range_20_to_60 <- mean(sequence_numbers)
sum_range_51_to_91 <- sum(seq(51, 91))

cat("(iii) Mean of numbers from 20 to 60:", mean_range_20_to_60, "\n")
cat("      Sum of numbers from 51 to 91:", sum_range_51_to_91, "\n\n")

# iv) Calculate the area of a rectangle
calculate_rectangle_area <- function(length, width) {
  area <- length * width
  return(area)
}

rectangle_length <- 8
rectangle_width <- 5
area_of_rectangle <- calculate_rectangle_area(rectangle_length, rectangle_width)

cat("(iv) Area of rectangle with length", rectangle_length, "and width", rectangle_width, "is:", area_of_rectangle, "\n")



# Load required library
library(plotly)

# Create a dataset for political knowledge
political_knowledge <- c("High", "Medium", "Low", "High", "Low")

# Create a 3D Pie Chart
pie_chart <- plot_ly(labels = ~political_knowledge, type = 'pie', marker = list(colors = c('#1f77b4', '#ff7f0e', '#2ca02c')),
                     height = 400, width = 600)

# Add suitable labels
layout(pie_chart, title = "Political Knowledge", showlegend = FALSE)

# Display the 3D Pie Chart
pie_chart



# Create a data frame with the given data
data <- data.frame(
  Month = 1:12,
  Spend = c(100, 0, 4000, 5000, 4500, 3000, 4000, 9000, 11000, 15000, 12000, 7000, 3000),
  Sales = c(991, 4, 4048, 7, 5432, 4, 5004, 4, 3471, 9, 4255, 1, 9487)
)

# Create a linear regression model
model <- lm(Sales ~ Spend, data = data)

# Summary of the regression model
summary(model)

# Predict sales for Spend=13500
new_data <- data.frame(Spend = 13500)
predicted_sales <- predict(model, newdata = new_data)

cat("\nPredicted Sales for Spend=13500:", predicted_sales, "\n")



# Given vector of numbers
vector_of_numbers <- 1:16

# Create a matrix with specified dimensions and by row
matrix_data <- matrix(vector_of_numbers, nrow = 4, byrow = TRUE)

# Define column and row names
colnames(matrix_data) <- c("Col1", "Col2", "Col3", "Col4")
rownames(matrix_data) <- c("Row1", "Row2", "Row3", "Row4")

# Display the created matrix
cat("Created Matrix:\n")
print(matrix_data)

# Access specific elements
element_3_2 <- matrix_data[2, 3]  # Element at 2nd row and 3rd column
row_3 <- matrix_data[3, ]         # Entire 3rd row
col_4 <- matrix_data[, 4]        # Entire 4th column

cat("\nAccessed Elements:\n")
cat("Element at 3rd column and 2nd row:", element_3_2, "\n")
cat("Entire 3rd row:\n")
print(row_3)
cat("Entire 4th column:\n")
print(col_4)




# Given data
height <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
weight <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Create a data frame
data <- data.frame(Height = height, Weight = weight)

# Create a linear regression model
model <- lm(Weight ~ Height, data = data)

# Display the summary of the linear regression model
summary(model)

# Predict weight for a new height value
new_height <- 160
predicted_weight <- predict(model, newdata = data.frame(Height = new_height))

cat("\nPredicted Weight for Height =", new_height, ":", predicted_weight, "\n")




# Load the USArrests dataset
data("USArrests")

# (i.a) Explore the summary of the dataset
cat("Summary of the USArrests dataset:\n")
summary(USArrests)

# (i.b) Print the state which saw the largest total number of rape
state_max_rape <- USArrests$Rape[which.max(USArrests$Rape)]
state_with_max_rape <- rownames(USArrests)[which.max(USArrests$Rape)]

cat("\nState with the largest total number of rape:\n")
cat("State:", state_with_max_rape, "\n")
cat("Number of rape:", state_max_rape, "\n")

# (i.c) Print the states with the max & min crime rates for murder
state_max_murder <- rownames(USArrests)[which.max(USArrests$Murder)]
state_min_murder <- rownames(USArrests)[which.min(USArrests$Murder)]

cat("\nStates with the max crime rate for murder:\n")
cat("State:", state_max_murder, "\n")
cat("Crime rate for murder:", USArrests$Murder[which.max(USArrests$Murder)], "\n")

cat("\nStates with the min crime rate for murder:\n")
cat("State:", state_min_murder, "\n")
cat("Crime rate for murder:", USArrests$Murder[which.min(USArrests$Murder)], "\n")




# Function to find the range of a vector
find_range <- function(vec) {
  max_val <- max(vec)
  min_val <- min(vec)
  range_val <- max_val - min_val
  return(range_val)
}

# Sample input
C <- c(9, 8, 7, 6, 5, 4, 3, 2, 1)

# Find and print the range
range_result <- find_range(C)
cat("\n(ii) Range of the given vector C:\n")
print(range_result)


# Create a factor from a random sample of LETTERS
set.seed(123)  # Set seed for reproducibility
random_letters <- sample(LETTERS, 100, replace = TRUE)
factor_letters <- as.factor(random_letters)

# Extract five levels of the factor
five_levels <- levels(sample(factor_letters, 5))
cat("(i) Five levels of the factor:\n")
print(five_levels)


# Creating a sample data frame
data <- data.frame(
  Name = c("Shan", "Ash", "Malu", "Viji"),
  Score = c(12.5, 9.0, 16.5, 12.5),
  Attempts = c(1, NA, 2, NA),
  Qualify = c("Yes", "No", "Yes", "No"),
  stringsAsFactors = FALSE  # Avoid converting strings to factors
)

# Extracting 2nd and 4th rows with 1st and 3rd columns
extracted_data <- data[c(2, 4), c(1, 3)]

# Display the extracted data
print(extracted_data)




# First Dataset
first_data <- data.frame(
  Surname = c("Smith", "Johnson", "Williams", "Jones", "Brown", "Davis", "Miller"),
  Nationality = c("USA", "Canada", "UK", "Australia", "USA", "Canada", "UK")
)

# Second Dataset
second_data <- data.frame(
  Surname = c("Smith", "Johnson", "Williams", "Jones", "Brown", "Davis", "Miller"),
  Movies = c(5, 8, 3, 6, 7, 2, 4)
)

# Merge datasets based on the common key variable (Surname)
merged_data <- merge(first_data, second_data, by = "Surname")

# Display the merged data
print("Merged Dataset:")
print(merged_data)

# Check the dimensionality
cat("\nDimensionality of the merged data:", dim(merged_data), "\n")







# Load the built-in dataset ChickWeight
data("ChickWeight")

# (i) Order the data frame by "weight" in ascending order grouped by "diet" and extract the last 6 records
ordered_chickweight <- ChickWeight[order(ChickWeight$Diet, ChickWeight$weight), ]
last_6_records <- tail(ordered_chickweight, 6)

cat("(i) Last 6 records after ordering by weight grouped by diet:\n")
print(last_6_records)

# (ii.a) Perform melting function based on "Chick", "Time", "Diet" features as ID variables
library(reshape2)
melted_chickweight <- melt(ChickWeight, id.vars = c("Chick", "Time", "Diet"))

# (ii.b) Perform cast function to display the mean value of weight grouped by Diet
cast_mean_weight <- dcast(melted_chickweight, Diet ~ variable, mean)

cat("\n(ii.b) Mean value of weight grouped by Diet:\n")
print(cast_mean_weight)

# (ii.c) Perform cast function to display the mode of weight grouped by Diet
library(dplyr)
cast_mode_weight <- melted_chickweight %>%
  group_by(Diet, variable) %>%
  summarise(mode = as.numeric(names(table(value))[which.max(table(value))]))

cat("\n(ii.c) Mode of weight grouped by Diet:\n")
print(cast_mode_weight)

# (iii.a) Create Box plot for "weight" grouped by "Diet"
boxplot(ChickWeight$weight ~ ChickWeight$Diet, main = "Box Plot of Weight Grouped by Diet", xlab = "Diet", ylab = "Weight")

# (iii.b) Create Histogram for "weight" features belong to Diet-1 category
hist(ChickWeight$weight[ChickWeight$Diet == 1], main = "Histogram of Weight for Diet-1", xlab = "Weight", col = "skyblue")

# (iii.c) Create Scatter plot for "weight" vs "Time" grouped by Diet
plot(ChickWeight$Time, ChickWeight$weight, col = ChickWeight$Diet, pch = 16,
     main = "Scatter Plot of Weight vs Time Grouped by Diet", xlab = "Time", ylab = "Weight", cex = 1.2, legend = TRUE)
legend("topright", legend = levels(as.factor(ChickWeight$Diet)), col = 1:4, pch = 16, title = "Diet")
