# #########
# SMDE 1st Assignment
#
# Authors:
#    Javier Beiro Piñón
#    Valéria Soldera Girelli
#    Duru Degimli
# #########


# a) Import data set to R assigning the type of each variable correctly. (5p)
# Importing the dataset
laptop_data <- read.csv("data/laptop_data_cleaned.csv", header = TRUE, stringsAsFactors = FALSE)

# Converting numeric variables
laptop_data$Ram <- as.numeric(laptop_data$Ram)
laptop_data$Weight <- as.numeric(laptop_data$Weight)
laptop_data$Price <- as.numeric(laptop_data$Price)
laptop_data$Ppi <- as.numeric(laptop_data$Ppi)
laptop_data$HDD <- as.numeric(laptop_data$HDD)
laptop_data$SSD <- as.numeric(laptop_data$SSD)

# Converting boolean variables (assuming they are currently stored as integers 0 and 1 or as characters 'Yes'/'No')
laptop_data$TouchScreen <- as.logical(laptop_data$TouchScreen)
laptop_data$Ips <- as.logical(laptop_data$Ips)


# b) Create a dataset including only types of laptops: Ultrabook, Notebook and 2 in 1 Convertible.
filtered_laptop_data <- subset(laptop_data, TypeName %in% c("Ultrabook", "Notebook", "2 in 1 Convertible"))



# c) Summarize the variables Weight, Price and the categorical variables in the new created data set. 

# Summarize numerical variables
summary(filtered_laptop_data$Weight)
summary(filtered_laptop_data$Price)

# Summarize categorical variables
table(filtered_laptop_data$TypeName)
table(filtered_laptop_data$TouchScreen)
table(filtered_laptop_data$Ips)
table(filtered_laptop_data$Cpu_brand)
table(filtered_laptop_data$Gpu_brand)
table(filtered_laptop_data$Os)


# d) Cross clasification type of computer and the touch screen 
# Number of laptops with and without touch screen for each type
table_data <- table(filtered_laptop_data$TypeName, filtered_laptop_data$TouchScreen)

prob_touch_given_type <- prop.table(table_data, 1) # Normalize over rows (Type of Computer)

prob_type_given_touch <- prop.table(table_data, 2) # Normalize over columns (TouchScreen)


# e) 
table_data <- table(filtered_laptop_data$TypeName, filtered_laptop_data$TouchScreen)

# Print the results
chi_square_test <- chisq.test(table_data)
print(chi_square_test)


# f)
# This is only for the partial data set
# Histogram for visual inspection of overall Price distribution
hist(filtered_laptop_data$Price, main = "Histogram of Prices", xlab = "Price", breaks = 30, col = "blue")

# Density Plot
plot(density(filtered_laptop_data$Price), main = "Density Plot of Prices", xlab = "Price", ylab = "Density")


# Shapiro-Wilk normality test for overall Price distribution
shapiro_test_filtered_laptop_data <- shapiro.test(filtered_laptop_data$Price)

# Print the results
print(shapiro_test_filtered_laptop_data)


# This is for all the data set
# Histogram for visual inspection of overall Price distribution
hist(laptop_data$Price, main = "Histogram of Prices", xlab = "Price", breaks = 30, col = "blue")

# Density Plot
plot(density(laptop_data$Price), main = "Density Plot of Prices", xlab = "Price", ylab = "Density")

# Shapiro-Wilk normality test for overall Price distribution
shapiro_test_laptop_data <- shapiro.test(laptop_data$Price)

# Print the results
print(shapiro_test_laptop_data)


# g) Data frame with just Ultrabooks and Notebooks
ultrabook_notebook_data <- subset(filtered_laptop_data, TypeName %in% c("Ultrabook", "Notebook"))


# boxplot for Price distribution across Ultrabook and Notebook
boxplot(Price ~ TypeName, data = ultrabook_notebook_data,
        main = "Price Distribution: Ultrabook vs. Notebook",
        xlab = "Type of Laptop", ylab = "Price",
        col = c("lightblue", "salmon"), # Optional: adding colors
        notch = FALSE) # Optional: adding notches to compare medians statistically





# i) Compare the average price of Ultrabooks and Notebooks by using the appropriate method. Do not forget to test the assumptions.