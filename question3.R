library(readr)
laptop <- read.csv("data/laptop_data_cleaned.csv")
#write.csv(num_laptop, file = "no_outliers_numerical_laptop.csv", row.names = TRUE)
num_laptop <- read.csv("no_outliers_numerical_laptop.csv")

laptop$Company <- as.factor(laptop$Company)
laptop$TypeName <- as.factor(laptop$TypeName)
laptop$Cpu_brand <- as.factor(laptop$Cpu_brand)
laptop$Gpu_brand <- as.factor(laptop$Gpu_brand)
laptop$Os <- as.factor(laptop$Os)
laptop$Ram <- as.factor(laptop$Ram)

laptop$TouchScreen <- as.logical(laptop$TouchScreen)
laptop$Ips <- as.logical(laptop$Ips)

#laptop$Ram <- as.numeric(laptop$Ram)
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
columns_to_check <- c("Weight", "HDD", "Ppi", "SSD") 
num_laptop <- laptop[, sapply(laptop, is.numeric)]

# Initiate a counter for tracking
iteration_counter <- 1
repeat {
  outliers_removed <- FALSE
  
  for (column in columns_to_check) {
    # Calculate outliers in the filtered dataset
    outliers <- boxplot.stats(num_laptop[[column]])$out
    # Filter out the outliers from the filtered dataset
    num_laptop <- num_laptop[!num_laptop[[column]] %in% outliers, ]
    
    # Print the iteration and number of outliers found (optional)
    cat("Iteration", iteration_counter, "Column:", column, ": Found", length(outliers), "outliers\n")
    
    if (length(outliers) > 0) {
      outliers_removed <- TRUE
    }
  }
  # Increment the counter
  iteration_counter <- iteration_counter + 1
  # Break the loop if no outliers are found
  if(!outliers_removed) {
    break
  }
}


boxplot(num_laptop$Ppi,
        main = "Box Plot of Laptop Weights",
        ylab = "Ppi",
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
summary(num_laptop)
#num_laptop <- num_laptop[, sapply(num_laptop, is.numeric)]
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
weight_price <- ggplot(num_laptop, aes_string(x = "Weight", y = "Price")) +
  theme(text = element_text(size = 16)) +
  geom_point() +
  labs(x = "Weight", y = "Price") +
  ggtitle(paste( "Weight vs. Price"))
print(weight_price)

ppi_price <- ggplot(num_laptop, aes_string(x = "Ppi", y = "Price")) +
  theme(text = element_text(size = 16)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01),
                     breaks = seq(min(num_laptop$Price), max(num_laptop$Price), length.out = 5)) +
  scale_x_continuous(labels = scales::number_format(accuracy = 1),
                     breaks = seq(min(num_laptop$Ppi), max(num_laptop$Ppi), length.out = 7)) +
  geom_point() +
  labs(x = "Ppi", y = "Price") +
  ggtitle(paste( "Ppi vs. Price"))
print(ppi_price)
hist(num_laptop$Ppi)

hdd_price <- ggplot(num_laptop, aes_string(x = "HDD", y = "Price")) +
  theme(text = element_text(size = 16)) +
  geom_point() +
  labs(x = "HDD", y = "Price") +
  ggtitle(paste( "HDD vs. Price"))
print(hdd_price)

# Outliers: should we consider the point 750 as outlier?
ssd_price <- ggplot(num_laptop, aes_string(x = "SSD", y = "Price")) +
  theme(text = element_text(size = 16)) +
  geom_point() +
  labs(x = "SSD", y = "Price") +
  ggtitle(paste( "SSD vs. Price"))
print(ssd_price)

# MODELS
mean_price <- mean(num_laptop$Price)

# Price is normal
qqnorm(num_laptop$Price)
hist(num_laptop$Price)
shapiro.test(num_laptop$Price) 

################ SSD MODEL
model_ssd <- lm(Price ~ SSD, data = num_laptop)
# first I'll test if the method is valid by testing if the residuals:
# 1. Its probability distribution is normal. (the mean of the dist is 0) (slide 53)
qqnorm(residuals(model_ssd))
hist(residuals(model_ssd))
shapiro.test(residuals(model_ssd))
# p-value = 0.7561 normal


# 2. The standard deviation is a constant regardless of the value of x (slide 57)
plot(residuals(model_ssd))
abline(h = 0, col = "red")
library(lmtest)
bptest(model_ssd) # should be higher than the confidence level to indicate homocedasticity 
# We have homocedasticity as the variance of the error is constant
# and the p-value of the Breusch-Pagan test is 0.8345

# 3. The error values are independent (59)
dwtest(model_ssd, alternative = "two.sided") # should be higher than the confidence level 
# the Durbin-Watson p-value is 0.05383 indicates that there is no
# autocorrelation in the residuals.

# We need to make an analysis of the residuals: examine the differences
# between the actual data points and those predicted by the linear equation
summ_ssd <- summary(model_ssd)
print(summ_ssd)
# Residual standard error: 0.3955 on 407 degrees of freedom
# the Price's mean is 10.7778, so since the error is small in comparison
# to the price's mean, it means that the model is good.
# For analyzing the slope, we get that the p-value is almost 0 (<2.2e-16)
# so it indicates that we can reject the null hypothesis that the slope 
# is zero and conclude that there is evidence of a linear relationship 
# between the two variables.

# Coefficient of Determination: the strength of the relationship
# Multiple R-squared:  0.2191 indicates that 21% of the variation of
# the price is explained by the SSD



################ PPI MODEL
model_ppi <- lm(Price ~ Ppi, data = num_laptop)
# first I'll test if the method is valid by testing if the residuals:
# 1. Its probability distribution is normal. (the mean of the dist is 0) (slide 53)
qqnorm(residuals(model_ppi))
hist(residuals(model_ppi))
shapiro.test(residuals(model_ppi))
# p-value = 0.3332 normal


# 2. The standard deviation is a constant regardless of the value of x (slide 57)
plot(residuals(model_ppi))
abline(h = 0, col = "red")
library(lmtest)
bptest(model_ppi) # should be higher than the confidence level to indicate homocedasticity 
# We have homocedasticity as the variance of the error is constant
# and the p-value of the Breusch-Pagan test is p-value < 2.2e-16 (bad)

# 3. The error values are independent (59)
dwtest(model_ppi, alternative = "two.sided") # should be higher than the confidence level 
# the Durbin-Watson p-value is 0.7758 indicates that there is no
# autocorrelation in the residuals.

# We need to make an analysis of the residuals: examine the differences
# between the actual data points and those predicted by the linear equation
summ_ppi <- summary(model_ppi)
print(summ_ppi)
# Residual standard error: 0.4661 on 1269 degrees of freedom
# the Price's mean is 10.82, so since the error is small in comparison
# to the price's mean, it means that the model is good.
# For analyzing the slope, we get that the p-value is almost 0 (<2.2e-16)
# so it indicates that we can reject the null hypothesis that the slope 
# is zero and conclude that there is evidence of a linear relationship 
# between the two variables.


################ HDD MODEL
model_hdd <- lm(Price ~ HDD, data = num_laptop)
# first I'll test if the method is valid by testing if the residuals:
# 1. Its probability distribution is normal. (the mean of the dist is 0) (slide 53)
qqnorm(residuals(model_hdd))
hist(residuals(model_hdd))
shapiro.test(residuals(model_hdd))
# p-value = 0.312 normal


# 2. The standard deviation is a constant regardless of the value of x (slide 57)
plot(residuals(model_hdd))
abline(h = 0, col = "red")
library(lmtest)
bptest(model_hdd) # should be higher than the confidence level to indicate homocedasticity 
# We have homocedasticity as the variance of the error is constant
# and the p-value of the Breusch-Pagan test is 0.06139

# 3. The error values are independent (59)
dwtest(model_hdd, alternative = "two.sided") # should be higher than the confidence level 
# the Durbin-Watson p-value is 0.7677 indicates that there is no
# autocorrelation in the residuals.

# We need to make an analysis of the residuals: examine the differences
# between the actual data points and those predicted by the linear equation
summ_hdd <- summary(model_hdd)
print(summ_hdd)
# Residual standard error: 0.447 on 407 degrees of freedom
# the Price's mean is 10.82, so since the error is small in comparison
# to the price's mean, it means that the model is good.
# For analyzing the slope, we get that the p-value is 0.3602
# so it indicates that we cannot reject the null hypothesis that the slope 
# is zero, concluding that there is no evidence of a linear relationship 
# between the two variables.

# Coefficient of Determination: this corroborates the stated above,
# since it indicates the strength of the relationship
# Multiple R-squared:  0.002057 indicates that less than 1% of the variation of
# the price is explained by the HDD


################ Weight MODEL
model_weight <- lm(Price ~ Weight, data = num_laptop)
# first I'll test if the method is valid by testing if the residuals:
# 1. Its probability distribution is normal. (the mean of the dist is 0) (slide 53)
qqnorm(residuals(model_weight))
hist(residuals(model_weight))
shapiro.test(residuals(model_weight))
# p-value = 0.26 normal


# 2. The standard deviation is a constant regardless of the value of x (slide 57)
plot(residuals(model_weight))
abline(h = 0, col = "red")
library(lmtest)
bptest(model_weight) # should be higher than the confidence level to indicate homocedasticity 
# We have homocedasticity as the variance of the error is constant
# and the p-value of the Breusch-Pagan test is 3.525e-06

# 3. The error values are independent (59)
dwtest(model_weight, alternative = "two.sided") # should be higher than the confidence level 
# the Durbin-Watson p-value is 0.6137 indicates that there is no
# autocorrelation in the residuals.

# We need to make an analysis of the residuals: examine the differences
# between the actual data points and those predicted by the linear equation
summ_weight <- summary(model_weight)
print(summ_weight)
# Residual standard error: 0.4432 on 407 degrees of freedom
# the Price's mean is 10.82, so since the error is small in comparison
# to the price's mean, it means that the model is good.
# For analyzing the slope, we get that the p-value is 0.005229, which is a small
# value, but it doesn't indicate a really strong linear relationship 
# between the two variables.
# Coefficient of Determination: this corroborates the stated above,
# since it indicates the strength of the relationship
# Multiple R-squared:  0.019 indicates that around 2% of the variation of
# the price is explained by the HDD


# Part
# ------ Outlier removal ----
# Check and remove rows with NA values in specified columns
laptop <- na.omit(laptop)

# Calculate Mahalanobis distance
# install.packages("mvoutlier")
library(mvoutlier)
cov_matrix <- cov(laptop[, c("Weight", "Price", "Ppi", "HDD", "SSD")])
center_vector <- colMeans(laptop[, c("Weight", "Price", "Ppi", "HDD", "SSD")], na.rm = TRUE)
laptop$Mahalanobis <- mahalanobis(laptop[, c("Weight", "Price", "Ppi", "HDD", "SSD")], center = center_vector, cov = cov_matrix)

# Define threshold based on chi-squared distribution
threshold <- qchisq(0.999, df = 5)  # df is the number of variables

# Identify outliers
outliers <- which(laptop$Mahalanobis > threshold)
multi_laptop <- laptop[-outliers, ]

# For multivariate model, since we have to try every combination of the 
# available pairs of these four variables, we are utilizing a loop and 
# several lists to hold the information.
#
# In addition to shapiro, bp and dw tests,
# we are also checking the variance inflation factor for multicollinearity.

variables <- c("Weight", "Ppi", "HDD", "SSD")

two_model <- list()
two_summary <- list()
two_vif <- list()
two_shapiro <- list()
two_bp <- list()
two_dw <- list()
two_models <- list()
two_rsquared <- list()


for (i in 1:(length(variables)-1)) {
  for (j in (i+1):length(variables)) {
    # Build model
    two_model <- lm(Price ~ ., data = multi_laptop[, c("Price", variables[i], variables[j])])
    two_models[[paste(variables[i], variables[j], sep = "_")]] <- two_model
    
    two_summary[[paste("summ", variables[i], variables[j], sep = "_")]] <- summary(two_model)
    
    library(car)
    #correlation
    two_vif[[paste("vif", variables[i], variables[j], sep = "_")]] <- vif(two_model)
    qqnorm(residuals(two_model))
    hist(residuals(two_model))
    two_shapiro[[paste("shapiro", variables[i], variables[j], sep = "_")]] <- shapiro.test(residuals(two_model))
    
    plot(residuals(two_model))
    abline(h = 0, col = "red")
    
    library(lmtest)
    two_bp[[paste("bp", variables[i], variables[j], sep = "_")]] <- bptest(two_model)
    
    two_dw[[paste("dw", variables[i], variables[j], sep = "_")]] <- dwtest(two_model, alternative = "two.sided")
    
    two_rsquared[[paste(variables[i], variables[j], sep = "_")]] <- summary(two_model)$r.squared
    
    print(vif(two_model))
    print(bptest(two_model))
    print(dwtest(two_model))
    print(shapiro.test(residuals(two_model)))
  }
}

# Best model is Ppi_HDD for multivariate models
print(two_summary["summ_Ppi_HDD"])

# Part C
factors <- c("Company", "TypeName", "Cpu_brand", "Gpu_brand", "Os", "Ram")

factored_models <- list()

for (var in factors) {
  formula <- paste("Price ~ SSD +", var)
  model <- lm(formula, data = multi_laptop)
  factored_models[[var]] <- model
}

for (var in factors) {
  print(paste("Summary for model with", var, "as a factor:"))
  print(summary(factored_models[[var]]))
}


# Validity
n <- nrow(multi_laptop)
train.sample1 <- sample(1:n, round(0.67*n))
train.set1 <- multi_laptop[train.sample1, ]
test.set1 <- multi_laptop[-train.sample1, ]

train.model1 <- lm(Price ~ SSD + Cpu_brand, data = train.set1)
summary(train.model1)

yhat <- predict(train.model1, test.set1, interval = "prediction")
yhat

y <- test.set1$Price

error<-cbind(yhat[,1,drop=FALSE],y,(y-yhat[,1])^2)
sqr_err<-error[,3]
mse<-mean(sqr_err)

### Root Mean Square Error ###
RMSE1<-sqrt(mse/(nrow(test.set1)))
RMSE1

names(train.model1)
RMSE_train1<- sqrt(mean((train.model1$residuals)^2)/nrow(train.set1))
RMSE_train1
