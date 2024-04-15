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
library(car) # For Levene's Test
library(lmtest) # For Durbin-Watson Test


# Normality Check
#1. Normality Assumption Checking with Histograms and Shapiro-Wilk Test
## Price_euros
hist(laptop_subset$Price, main = "Histogram of Price", xlab = "Price in Euros")
shapiro.test(laptop_subset$Price)

## Weight
par(mar=c(7, 7, 7, 7))  # Further adjust the margins to accommodate larger text
hist(laptop_subset$Weight,
     main = "Histogram of Weight",
     xlab = "Weight",
     col = "lightblue", # Set color to match boxplot style
     border = "darkblue", # Set border color to match boxplot style
     cex.main=3.0, # Double the main title text size (same as boxplot)
     cex.lab=2.6, # Double the axis labels text size (same as boxplot)
     cex.axis=2.2) #
shapiro.test(laptop_subset$Weight) #Fails normality test


#1.1 Representing the weight data as a boxplot to visualize if there are any outliers
# Representation of the wight data using boxplots
par(mar=c(7, 7, 7, 7))  # Further adjust the margins to accommodate larger text

# Create a boxplot
boxplot(laptop_subset$Weight,
        main = "Box Plot of Laptop Weights",
        ylab = "Weight",
        xlab = "Laptops",
        col = "lightblue", # Set color
        border = "darkblue", # Set border color
        cex.main=3.0, # Double the main title text size (previously 1.5)
        cex.lab=2.6, # Double the axis labels text size (previously 1.3)
        cex.axis=2.2) # Double the axis annotation text size (previously 1.1)

#1.2 Removing Outliers
# Copy the original dataset to a new variable for filtering
laptop_subset_filtered <- laptop_subset

# Initiate a counter for tracking
iteration_counter <- 1

repeat {
  # Calculate outliers in the filtered dataset
  outliers <- boxplot.stats(laptop_subset_filtered$Weight)$out

  # Print the iteration and number of outliers found (optional)
  cat("Iteration", iteration_counter, ": Found", length(outliers), "outliers\n")

  # Break the loop if no outliers are found
  if(length(outliers) == 0) {
    break
  }

  # Filter out the outliers from the filtered dataset
  laptop_subset_filtered <- laptop_subset_filtered[!laptop_subset_filtered$Weight %in% outliers, ]

  # Increment the counter
  iteration_counter <- iteration_counter + 1
}

# Box plot for Weight
boxplot(laptop_subset_filtered$Weight,
        main = "Box Plot of Laptop Weights",
        ylab = "Weight",
        xlab = "Laptops",
        col = "lightblue", # Set color
        border = "darkblue",
        cex.main=3.0, # Double the main title text size (previously 1.5)
        cex.lab=2.6, # Double the axis labels text size (previously 1.3)
        cex.axis=2.2) # Double the axis annotation text size (previously 1.1)

hist(laptop_subset_filtered$Weight,
     main = "Histogram of Weight",
     xlab = "Weight", cex.main=3.0, # Double the main title text size (same as boxplot)
     col = "lightblue", # Set color to match boxplot style
     border = "darkblue", # Set border color to match boxplot style
     cex.lab=2.6, # Double the axis labels text size (same as boxplot)
     cex.axis=2.2)
shapiro.test(laptop_subset_filtered$Weight) #Fails normality test

# a) Log transformation
# Note: Adding a small constant to avoid taking log of zero if any values are zero
laptop_subset_filtered$Weight_log <- log(laptop_subset_filtered$Weight + 1)

# Histogram for the log transformed data
hist(laptop_subset_filtered$Weight_log, main = "Histogram of Log Transformed Weight", xlab = "Log(Weight)")

# Shapiro-Wilk normality test on the log transformed weights
shapiro.test(laptop_subset_filtered$Weight_log)

# b) Square root transformation
laptop_subset_filtered$Weight_sqrt <- sqrt(laptop_subset_filtered$Weight)

# Histogram for the square root transformed data
hist(laptop_subset_filtered$Weight_sqrt, main = "Histogram of Square Root Transformed Weight", xlab = "Sqrt(Weight)")

# Shapiro-Wilk normality test on the square root transformed weights
shapiro.test(laptop_subset_filtered$Weight_sqrt)

# c) Cube root transformation
laptop_subset_filtered$Weight_cbrt <- (laptop_subset_filtered$Weight)^(1/3)

# Histogram for the cube root transformed data
hist(laptop_subset_filtered$Weight_cbrt, main = "Histogram of Cube Root Transformed Weight", xlab = "Cbrt(Weight)")

# Shapiro-Wilk normality test on the cube root transformed weights
shapiro.test(laptop_subset_filtered$Weight_cbrt)


#2. Homogeneity of Variances (Levene's Test)
leveneTest(Price ~ Company, data = laptop_subset)
leveneTest(Weight ~ Company, data = laptop_subset)

#3. Independence of Observations (Durbin-Watson Test)
model_price <- lm(Price ~ Company, data = laptop_subset)
dwtest(model_price)

model_weight <- lm(Weight ~ Company, data = laptop_subset)
dwtest(model_weight)


# C) 
laptop_subset$Company <- as.factor(laptop_subset$Company)
laptop_subset$TouchScreen <- as.factor(laptop_subset$TouchScreen)

# ANOVA to test if there are statistically significant differences in the average prices of laptops by Brand
anova_result <- aov(Price ~ Company, data = laptop_subset)
summary(anova_result)

# D) 
# Assuming the dataset 'laptop_subset' is already loaded and properly formatted.
# Check for Normality
shapiro.test(laptop_subset$Price)

# Perform Levene’s Test separately for each factor
# Checking homogeneity of variances for 'Price' across different 'Company'
leveneTest(Price ~ Company, data = laptop_subset)

# Checking homogeneity of variances for 'Price' across 'Touchscreen' status
leveneTest(Price ~ TouchScreen, data = laptop_subset)

# Fit a two-way ANOVA model including interaction between brand and touchscreen
model_price <- aov(Price ~ Company + TouchScreen + Company:TouchScreen, data = laptop_subset)
summary(model_price)  

# Durbin-Watson test to check for independence of residuals
dw_test <- dwtest(model_price, alternative = "two.sided")
print(dw_test)


## Post-Hoc Tests ##
if (!require("multcomp")) {
    install.packages("multcomp", dependencies=TRUE)
    library(multcomp)
}
if (!require("agricolae")) {
    install.packages("agricolae", dependencies=TRUE)
    library(agricolae)
}


# 1. Tukey's HSD Test
tukey_test <- TukeyHSD(model_price, "Company")
print(tukey_test)

# 2. Dunnett's Test - Comparing each brand to a control group, say 'Dell' considered as control
dunnett_test <- glht(model_price, linfct = mcp(Company = "Dunnett"))
summary(dunnett_test)

# 3. Scheffé's Test - Useful for complex comparisons, generally more conservative
scheffe_test <- glht(model_price, linfct = mcp(Company = "Scheffe"))
summary(scheffe_test)