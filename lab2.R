install.packages("lmtest")
install.packages("car")
install.packages("ggplot2")
library(lmtest)
library(car)

# Exercise a)
# Read the laptop data set
laptop_data <- read.csv("data/laptop_data_cleaned.csv", header = TRUE, stringsAsFactors = FALSE)

# Create a subset including only Dell, Acer, and Hp brands
laptop_subset <- laptop_data[laptop_data$Company %in% c("Dell", "Acer", "HP"), ]

# Summarize the "Company" variable
company_summary <- table(laptop_subset$Company)
print(company_summary)

# Check the overall distribution of Price and Weight
price_distribution <- summary(laptop_subset$Price)
weight_distribution <- summary(laptop_subset$Weight)

# Print the distributions
cat("Price Distribution:\n")
print(price_distribution)
cat("\nWeight Distribution:\n")
print(weight_distribution)


# Exercise b)
# Subset the data for specific companies
laptop_subset <- laptop_data[laptop_data$Company %in% c("Dell", "Acer", "HP"), ]

# Load necessary libraries
library(zoo)
library(car) # For Levene's Test
library(lmtest) # For Durbin-Watson Test

# Normality Assumption Checking with Histograms and Shapiro-Wilk Test
## Price_euros
hist(laptop_subset$Price, main = "Histogram of Price", xlab = "Price in Euros")
shapiro.test(laptop_subset$Price)

## Weight
hist(laptop_subset$Weight, main = "Histogram of Weight", xlab = "Weight")
shapiro.test(laptop_subset$Weight)

# Homogeneity of Variances (Levene's Test)
leveneTest(Price ~ Company, data = laptop_subset)
leveneTest(Weight ~ Company, data = laptop_subset)

# Independence of Observations (Durbin-Watson Test)
model_price <- lm(Price ~ Company, data = laptop_subset)
dwtest(model_price)

model_weight <- lm(Weight ~ Company, data = laptop_subset)
dwtest(model_weight)


# Exercise c)
laptop_subset$Weight_inv <- 1 / laptop_subset$Weight
shapiro.test(laptop_subset$Weight_inv)

