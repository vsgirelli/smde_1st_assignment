library(readr)

setwd("/home/vsgirelli/dev/upc/smde/smde_1st_assignment/")
laptop <- read.csv("data/laptop_data_cleaned.csv")

laptop$Company <- as.factor(laptop$Company)
laptop$TypeName <- as.factor(laptop$TypeName)
laptop$Cpu_brand <- as.factor(laptop$Cpu_brand)
laptop$Gpu_brand <- as.factor(laptop$Gpu_brand)
laptop$Os <- as.factor(laptop$Os)

laptop$TouchScreen <- as.logical(laptop$TouchScreen)
laptop$Ips <- as.logical(laptop$Ips)

laptop$Ram <- as.numeric(laptop$Ram)
laptop$Weight <- as.numeric(laptop$Weight)
laptop$Price <- as.numeric(laptop$Price)
laptop$Ppi <- as.numeric(laptop$Ppi)
laptop$HDD <- as.numeric(laptop$HDD)
laptop$SSD <- as.numeric(laptop$SSD)

###### Removing outliers

boxplot(laptop$SSD,
        main = "Box Plot of Laptop Weights",
        ylab = "Ram",
        xlab = "Laptops",
        col = "lightblue", # Set color
        border = "darkblue",
        cex.main=3.0, # Double the main title text size (previously 1.5)
        cex.lab=2.6, # Double the axis labels text size (previously 1.3)
        cex.axis=2.2) # Double the axis annotation text size (previously 1.1)

# Copy the original dataset to a new variable for filtering
laptop_subset_filtered <- laptop
columns_to_check <- c("Weight", "SSD", "HDD", "Ppi", "Ram") 
# Initialize iteration counter
iteration_counter <- 1

# Start the repeat loop
repeat {
  # Initialize an empty list to store filtered datasets for each column
  filtered_datasets <- list()
  
  # Iterate over each column
  for (col_name in columns_to_check) {
    # Calculate outliers in the filtered dataset for the current column
    outliers <- boxplot.stats(laptop_subset_filtered[[col_name]])$out
    
    # Filter out the outliers from the filtered dataset for the current column
    filtered_datasets[[col_name]] <- laptop_subset_filtered[!laptop_subset_filtered[[col_name]] %in% outliers, ]

    # Print the iteration and number of outliers found for the current column
    cat("Iteration", iteration_counter, "for column", col_name, ": Found", length(outliers), "outliers\n")
  }
  
  # Check if any of the filtered datasets are empty
  if (any(sapply(filtered_datasets, function(x) nrow(x)) == 0)) {
    break  # If any dataset is empty, break the loop
  }
  
  # Update the laptop_subset_filtered dataset with filtered datasets for each column
  common_rows <- Reduce(intersect, lapply(filtered_datasets, rownames))
  final_dataset <- filtered_datasets[[1]][common_rows, ]
  for (i in 2:length(filtered_datasets)) {
    final_dataset <- merge(final_dataset, filtered_datasets[[i]][common_rows, ], by = colnames(final_dataset), all = FALSE)
  }
  
  # Increment the counter
  iteration_counter <- iteration_counter + 1
}

boxplot(laptop_subset_filtered$Ram,
        main = "Box Plot of Laptop Weights",
        ylab = "Ram",
        xlab = "Laptops",
        col = "lightblue", # Set color
        border = "darkblue",
        cex.main=3.0, # Double the main title text size (previously 1.5)
        cex.lab=2.6, # Double the axis labels text size (previously 1.3)
        cex.axis=2.2) # Double the axis annotation text size (previously 1.1)


# Since we need to find the best *simple linear regression model*
# we have to assess each numerical variable separately to determine 
# which one has the strongest linear relationship with the price variable.
# First we analysed the correlations with price both through the general plot
# and through individual plots. 
# Since RAM and SSD have the stronger correlations, I will focus the rest
# of the analysis on them (it makes no sense to try to evaluate a regression
# model with weight for instance).

#install.packages("corrplot")
# Correlation analysis
library(corrplot)
summary(laptop_subset_filtered)
num_laptop <- laptop_subset_filtered[, sapply(laptop_subset_filtered, is.numeric)]
correlation_matrix <- cor(num_laptop)
corrplot(correlation_matrix, method = "color", tl.col = "black", tl.cex = 1, cl.cex = 0.8)
# In this corrplot, basically it means that the darker blue colors (essentially with RAM and SSD)
# indicate that there's a strong positive relationship
# Closer to white indicate no correlation
# Negative relationship would be the red colors,
# means that as the value of one variable increases, the value of the other variable tends to decrease.
# We don't have this case.
# These correlation values can also be seen in the matrix below.
# The correlation values are important to determine whether a relationship might exist.

correlation_with_price <- correlation_matrix["Price", -ncol(correlation_matrix)]
remaining_columns <- colnames(correlation_matrix)[-which(colnames(correlation_matrix) == "Price")]
barplot(correlation_with_price, main = "Correlation with Price", names.arg = names(correlation_with_price))
print(correlation_with_price)


# The following analysis is useful to identify outliers (62)
#install.packages("ggplot2")
library(ggplot2)
#print(names(laptop)[sapply(laptop, is.numeric)])


# Outliers: the right-most point is an outlier
ram_price <- ggplot(laptop_subset_filtered, aes_string(x = "Ram", y = "Price")) +
    theme(text = element_text(size = 16)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                       breaks = seq(min(laptop_subset_filtered$Price), max(laptop_subset_filtered$Price), length.out = 5)) +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.01),
                       breaks = seq(min(laptop_subset_filtered$Ram), max(laptop_subset_filtered$Ram), length.out = 7)) +
    geom_point() +
    labs(x = "Ram", y = "Price") +
    ggtitle(paste( "Ram vs. Price"))
print(ram_price)

weight_price <- ggplot(laptop_subset_filtered, aes_string(x = "Weight", y = "Price")) +
  theme(text = element_text(size = 16)) +
  geom_point() +
  labs(x = "Weight", y = "Price") +
  ggtitle(paste( "Weight vs. Price"))
print(weight_price)

ppi_price <- ggplot(laptop_subset_filtered, aes_string(x = "Ppi", y = "Price")) +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                     breaks = seq(min(laptop_subset_filtered$Price), max(laptop_subset_filtered$Price), length.out = 5)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     breaks = seq(min(laptop_subset_filtered$Ppi), max(laptop_subset_filtered$Ppi), length.out = 7)) +
  geom_point() +
  labs(x = "Ppi", y = "Price") +
  ggtitle(paste( "Ppi vs. Price"))
print(ppi_price)

hdd_price <- ggplot(laptop_subset_filtered, aes_string(x = "HDD", y = "Price")) +
  theme(text = element_text(size = 16)) +
  geom_point() +
  labs(x = "HDD", y = "Price") +
  ggtitle(paste( "HDD vs. Price"))
print(hdd_price)

# Outliers: should we consider the point 750 as outlier?
ssd_price <- ggplot(laptop_subset_filtered, aes_string(x = "SSD", y = "Price")) +
  theme(text = element_text(size = 16)) +
  geom_point() +
  labs(x = "SSD", y = "Price") +
  ggtitle(paste( "SSD vs. Price"))
print(ssd_price)

# MODELS
mean_price <- mean(laptop_subset_filtered$Price)

################ RAM MODEL
# (outlier removed)
#ram_2 <- (num_laptop$Ram)^2
model_ram <- lm(Price ~ Ram, data = num_laptop)
# first I'll test if the method is valid by testing if the residuals:
# 1. Its probability distribution is normal. (the mean of the dist is 0) (slide 53)
qqnorm(residuals(model_ram))
hist(residuals(model_ram))
shapiro.test(residuals(model_ram)) # should be higher than confidence level
# p-value = 0.252, normal

# 2. The standard deviation is a constant regardless of the value of x (slide 57)
plot(residuals(model_ram))
abline(h = 0, col = "red")
# We had an outlier:
# the plot shows that there's an outlier in RAM
# an outlier in a residual plot used to test for homoscedasticity
# may indicate potential issues with the assumption of constant variance of the residuals.
library(lmtest)
bptest(model_ram) # should be higher than the confidence level to indicate homocedasticity 
# We have homocedasticity as the variance of the error is constant
# and the p-value of the Breusch-Pagan test is 0.9558

# 3. The error values are independent (59)
dwtest(model_ram, alternative = "two.sided") # should be higher than the confidence level 
# the Durbin-Watson p-value is 0.02219 indicates that there is
# autocorrelation in the residuals as it is smaller than the significance level.
# Tried modifications, didn't work

Vale Pol entonces te explico
Estoy haciendo el lab 3, letra a. Ahí he removido muchos outliers y tal, pero

# We need to make an analysis of the residuals: examine the differences
# between the actual data points and those predicted by the linear equation
summ_ram <- summary(model_ram)
print(summ_ram)
# Residual standard error: 0.4448 on 1269 degrees of freedom
# the Price's mean is 10.82, so since the error is small in comparison
# to the price's mean, it means that the model is good.
# For analyzing the slope, we get that the p-value is almost 0 (<2.2e-16)
# so it indicates that we can reject the null hypothesis that the slope 
# is zero and conclude that there is evidence of a linear relationship 
# between the two variables.

# Coefficient of Determination: the strength of the relationship
# Multiple R-squared:  0.4832 indicates that 48% of the variation of
# the price is explained by the RAM.

# TODO 
# check the intervals (41) she also does it
# use the equation to predict a particular value



################ SSD MODEL
ssd_2 <- sqrt(num_laptop$SSD)

model_ssd <- lm(Price ~ SSD+ssd_2, data = num_laptop)
# first I'll test if the method is valid by testing if the residuals:
# 1. Its probability distribution is normal. (the mean of the dist is 0) (slide 53)
qqnorm(residuals(model_ssd))
hist(residuals(model_ssd))
shapiro.test(residuals(model_ssd))
# p-value = 0.00624 isn't normal


# 2. The standard deviation is a constant regardless of the value of x (slide 57)
plot(residuals(model_ssd))
abline(h = 0, col = "red")
library(lmtest)
bptest(model_ssd) # should be higher than the confidence level to indicate homocedasticity 
# We have homocedasticity as the variance of the error is constant
# and the p-value of the Breusch-Pagan test is 0.07201

# 3. The error values are independent (59)
dwtest(model_ssd, alternative = "two.sided") # should be higher than the confidence level 
# the Durbin-Watson p-value is 0.4353 indicates that there is no
# autocorrelation in the residuals.

# We need to make an analysis of the residuals: examine the differences
# between the actual data points and those predicted by the linear equation
summ_ssd <- summary(model_ssd)
print(summ_ssd)
# Residual standard error: 0.4661 on 1269 degrees of freedom
# the Price's mean is 10.82, so since the error is small in comparison
# to the price's mean, it means that the model is good.
# For analyzing the slope, we get that the p-value is almost 0 (<2.2e-16)
# so it indicates that we can reject the null hypothesis that the slope 
# is zero and conclude that there is evidence of a linear relationship 
# between the two variables.





print(names(correlation_with_price))
# Model (and then normality test over residuals)
models <- lapply(names(correlation_with_price), function(var) lm(Price ~ ., data = num_laptop[, c(var, "Price")]))
models2 <- lapply(names(correlation_with_price), function(var) lm(formula = Price ~ ., data = num_laptop))
summary_stats <- lapply(models, summary)
rsquared_values <- sapply(summary_stats, function(model_summary) model_summary$r.squared)

# Generating a model for all the other numerical variables and analysing
# their Coefficient of Determination to check that the most significant ones
# were RAM and SSD. the other ones present small Coefficient of Determination,
# meaning that the strength of the relationship is smaller.
models <- lapply(remaining_columns, function(var) lm(Price ~ ., data = num_laptop[, c(var, "Price")]))
#summary_stats <- lapply(models, summary)
#print(summary_stats)
rsquared_values <- sapply(summary_stats, function(model_summary) model_summary$r.squared)
best_model_index <- which.max(rsquared_values)
best_model <- models[[best_model_index]]
best_model_summary <- summary(best_model)
best_model_r_squared <- best_model_summary$r.squared

# Print the best model summary and R-squared value
print(best_model_summary)
print(paste("Best model R-squared:", best_model_r_squared))












