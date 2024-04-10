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


#install.packages("corrplot")
# Correlation analysis
library(corrplot)
summary(laptop)
num_laptop <- laptop[, sapply(laptop, is.numeric)]
correlation_matrix <- cor(num_laptop)
corrplot(correlation_matrix, method = "color", tl.col = "black", tl.cex = 1, cl.cex = 0.8)

correlation_with_price <- correlation_matrix["Price", -ncol(correlation_matrix)]
barplot(correlation_with_price, main = "Correlation with Price", names.arg = names(correlation_with_price))
print(correlation_with_price)

#install.packages("ggplot2")
library(ggplot2)
#print(names(laptop)[sapply(laptop, is.numeric)])

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
ram_p_value <- ram_shapiro$p.value

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
ssd_p_value <- ssd_shapiro$p.value

print(names(correlation_with_price))
# Model (and then normality test over residuals)
models <- lapply(names(correlation_with_price), function(var) lm(Price ~ ., data = num_laptop[, c(var, "Price")]))
models2 <- lapply(names(correlation_with_price), function(var) lm(formula = Price ~ ., data = num_laptop))
summary_stats <- lapply(models, summary)
rsquared_values <- sapply(summary_stats, function(model_summary) model_summary$r.squared)

# Select the best model
best_model_index <- which.max(rsquared_values)
best_model <- models[[best_model_index]]
best_model_summary <- summary(best_model)
best_model_r_squared <- best_model_summary$r.squared

# Print the best model summary and R-squared value
print(best_model_summary)
print(paste("Best model R-squared:", best_model_r_squared))