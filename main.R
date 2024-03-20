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




