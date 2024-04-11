library(readr)

setwd("/home/vsoldera/dev/upc/2nd/smde/smde_1st_assignment/")
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
laptop <- laptop[- which(laptop$Ram == 64), ]

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
summary(laptop)
num_laptop <- laptop[, sapply(laptop, is.numeric)]
correlation_matrix <- cor(num_laptop)
corrplot(correlation_matrix, method = "color", tl.col = "black", tl.cex = 1, cl.cex = 0.8)
# In this corrplot, basically it means that the darker blue colors (essentially with RAM and SSD)
# indicate that there's a strong positive relationship
# Closer to white indicate no correlation
# I don't really understand the "negative relationship", but that would be the red colors
# (essentially with HDD and PPI). 
# These correlation values can also be seen in the matrix below.
# The correlation values are important to determine whether a relationship might exist.
correlation_with_price <- correlation_matrix["Price", -ncol(correlation_matrix)]
barplot(correlation_with_price, main = "Correlation with Price", names.arg = names(correlation_with_price))
print(correlation_with_price)


# The following analysis is useful to identify outliers (62)
#install.packages("ggplot2")
library(ggplot2)
#print(names(laptop)[sapply(laptop, is.numeric)])


# Outliers: the right-most point is an outlier
ram_price <- ggplot(laptop, aes_string(x = "Ram", y = "Price")) +
    theme(text = element_text(size = 16)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                       breaks = seq(min(laptop$Price), max(laptop$Price), length.out = 5)) +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.01),
                       breaks = seq(min(laptop$Ram), max(laptop$Ram), length.out = 7)) +
    geom_point() +
    labs(x = "Ram", y = "Price") +
    ggtitle(paste( "Ram vs. Price"))
print(ram_price)

weight_price <- ggplot(laptop, aes_string(x = "Weight", y = "Price")) +
  theme(text = element_text(size = 16)) +
  geom_point() +
  labs(x = "Weight", y = "Price") +
  ggtitle(paste( "Weight vs. Price"))
print(weight_price)

ppi_price <- ggplot(laptop, aes_string(x = "Ppi", y = "Price")) +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                     breaks = seq(min(laptop$Price), max(laptop$Price), length.out = 5)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     breaks = seq(min(laptop$Ppi), max(laptop$Ppi), length.out = 7)) +
  geom_point() +
  labs(x = "Ppi", y = "Price") +
  ggtitle(paste( "Ppi vs. Price"))
print(ppi_price)

hdd_price <- ggplot(laptop, aes_string(x = "HDD", y = "Price")) +
  theme(text = element_text(size = 16)) +
  geom_point() +
  labs(x = "HDD", y = "Price") +
  ggtitle(paste( "HDD vs. Price"))
print(hdd_price)

# Outliers: should we consider the point 750 as outlier?
ssd_price <- ggplot(laptop, aes_string(x = "SSD", y = "Price")) +
  theme(text = element_text(size = 16)) +
  geom_point() +
  labs(x = "SSD", y = "Price") +
  ggtitle(paste( "SSD vs. Price"))
print(ssd_price)

# Normality test (over raw values)
print(names(laptop)[sapply(laptop, is.numeric)])
# Ram
ram_shapiro <- shapiro.test(laptop$Ram)
ram_p_value <- ram_shapiro$p.value # normal
# Weight
weight_shapiro <- shapiro.test(laptop$Weight)
weight_p_value <- weight_shapiro$p.value
# Ppi
ppi_shapiro <- shapiro.test(laptop$Ppi)
ppi_p_value <- ppi_shapiro$p.value
# HDD
hdd_shapiro <- shapiro.test(laptop$HDD)
hdd_p_value <- hdd_shapiro$p.value
# SSD
ssd_shapiro <- shapiro.test(laptop$SSD)
ssd_p_value <- ssd_shapiro$p.value # normal


# MODELS
mean_price <- mean(laptop$Price)

################ RAM MODEL
# (outlier removed)
model_ram <- lm(Price ~ Ram, data = num_laptop)
# first I'll test if the method is valid by testing if the residuals:
# 1. Its probability distribution is normal. (the mean of the dist is 0) (slide 53)
qqnorm(residuals(model_ram))
hist(residuals(model_ram))
shapiro.test(residuals(model_ram))
# p-value = 0.0414
# All three indicate normality

# 2. The standard deviation is a constant regardless of the value of x (slide 57)
plot(residuals(model_ram))
abline(h = 0, col = "red")
# We had an outlier:
# the plot shows that there's an outlier in RAM
# an outlier in a residual plot used to test for homoscedasticity
# may indicate potential issues with the assumption of constant variance of the residuals.
library(lmtest)
bptest(model_ram)
# We observe in the plot that there seems to be an outlier,
# and the p-value of the Breusch-Pagan test is too small (3.393e-11),
# This indicates evidence of heteroscedasticity, suggesting that the 
# variance of the residuals varies across different values of the independent variables.
# After removing the outlier:
# We have homocedasticity as the variance of the error is constant
# and the p-value of the Breusch-Pagan test is 0.5586

# 3. The error values are independent (59)
dwtest(model_ram, alternative = "two.sided")
# the Durbin-Watson p-value is 0.4986 indicates that there is no
# autocorrelation in the residuals as it is higher than the significance level.

# We need to make an analysis of the residuals: examine the differences
# between the actual data points and those predicted by the linear equation
summ_ram <- summary(model_ram)
print(summ_ram)
# The Residual standard error: 0.4447 on 1270 degrees of freedom
# the Price's mean is 10.82, so since the error is small in comparison
# to the price's mean, it means that the model is good.
# For analyzing the slope, we get that the p-value is almost 0 (<2e-16)
# so it indicates that we can reject the null hypothesis that the slope 
# is zero and conclude that there is evidence of a linear relationship 
# between the two variables.

################ SSD MODEL
model_ssd <- lm(Price ~ SSD, data = num_laptop)
# first I'll test if the method is valid by testing if the residuals:
# 1. Its probability distribution is normal. (the mean of the dist is 0) (slide 53)
qqnorm(residuals(model_ssd))
hist(residuals(model_ssd))
shapiro.test(residuals(model_ssd))
# p-value = 0.07079 TODO HERE it's not normal

# 2. The standard deviation is a constant regardless of the value of x (slide 57)
plot(residuals(model_ssd))
abline(h = 0, col = "red")
library(lmtest)
bptest(model_ssd)
# We have homocedasticity as the variance of the error is constant
# and the p-value of the Breusch-Pagan test is 0.05876

# 3. The error values are independent (59)
dwtest(model_ssd, alternative = "two.sided")
# the Durbin-Watson p-value is 0.6523 indicates that there is no
# autocorrelation in the residuals.

# We need to make an analysis of the residuals: examine the differences
# between the actual data points and those predicted by the linear equation
summ_ssd <- summary(model_ssd)
print(summ_ssd)
# Residual standard error: 0.4663 on 1271 degrees of freedom
# the Price's mean is 10.82, so since the error is small in comparison
# to the price's mean, it means that the model is good.
# For analyzing the slope, we get that the p-value is almost 0 (<2e-16)
# so it indicates that we can reject the null hypothesis that the slope 
# is zero and conclude that there is evidence of a linear relationship 
# between the two variables.




# Coefficient of Determination: the strength of the relationship
# Multiple R-squared:  0.4631 indicates that 46% of the variation of
# the price is explained by the RAM.

# TODO 
# check the intervals (41) she also does it
# use the equation to predict a particular value






# Model (and then normality test over residuals)
#  Ram      Weight       Price         Ppi         HDD 
# 0.68051892  0.15138619  1.00000000  0.48068692 -0.09736141 
models <- lapply(names(correlation_with_price), function(var) lm(Price ~ ., data = num_laptop[, c(var, "Price")]))
summary_stats <- lapply(models, summary)

models <- lapply(names(correlation_with_price), function(var) lm(formula = Price ~ ., data = num_laptop))
rsquared_values <- sapply(summary_stats, function(model_summary) model_summary$r.squared)

# Select the best model
best_model_index <- which.max(rsquared_values)
best_model <- models[[best_model_index]]
best_model_summary <- summary(best_model)
best_model_r_squared <- best_model_summary$r.squared

# Print the best model summary and R-squared value
print(best_model_summary)
print(paste("Best model R-squared:", best_model_r_squared))












