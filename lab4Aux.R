# Aux function for lab4.R
kmo <- function(x)
{
  x <- subset(x, complete.cases(x))       # Omit missing values
  r <- cor(x)                             # Correlation matrix
  r2 <- r^2                               # Squared correlation coefficients
  i <- solve(r)                           # Inverse matrix of correlation matrix
  d <- diag(i)                            # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2          # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0               # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

# install.packages("corrr")
library("corrr")
# install.packages("corrplot")
library("corrplot")
# install.packages("FactoMineR")
library("FactoMineR")
# install.packages("FactoMineR")
library("FactoMineR")
# install.packages("psych")
library(psych)  # for Bartlett's test and KMO

# 1. Load the decathlon dataset and prepare it for PCA
data("decathlon")
head(decathlon)
str(decathlon)
  #1.1 Erase no numeric columns
  decathlon_numeric <- decathlon[, sapply(decathlon, is.numeric)]

# 2. Correlation matrix and test the assumptions for PCA
corr_matrix <- cor(decathlon_numeric)

  # 2.1 Bartlett's Test of Spherecity
  n<-nrow(decathlon_numeric)
  cortest.bartlett(corr_matrix,n)

  # 2.2 Kaiser-Meyer-Olkin (KMO) Test
  kmo(decathlon_numeric)
  Pole.vault <- which(colnames(decathlon_numeric) == "Pole.vault")
  m1500 <- which(colnames(decathlon_numeric) == "1500m")
  Javeline <- which(colnames(decathlon_numeric) == "Javeline")
  kmo(decathlon_numeric[,-c(m1500, Pole.vault, Javeline)])



### PCA of heptathlon data ######
library(FactoMineR)
?PCA




#1. Erase no numeric columns


# 2. Normalization 
# decathlon_normalized <- scale(decathlon_numeric)

# 4. Scale Check: Ensuring similar scale post-normalization
apply(decathlon_normalized, 2, mean)  # Should be very close to 0
apply(decathlon_normalized, 2, sd)    # Should be close to 1

sapply(decathlon_numeric, function(x) shapiro.test(x)$p.value)


# TEst
# Check for constant columns explicitly and remove them
constant_columns <- sapply(decathlon_numeric, function(x) var(x, na.rm = TRUE) == 0)
if (any(constant_columns)) {
  decathlon_numeric <- decathlon_numeric[, !constant_columns]
}

# Summary before scaling to inspect data
print("Summary before scaling:")
print(summary(decathlon_numeric))

# Perform scaling
decathlon_normalized <- scale(decathlon_numeric)

# Check summary after scaling
print("Summary after scaling:")
print(summary(decathlon_normalized))

# Re-apply the Shapiro-Wilk test on scaled data
shapiro_results_after <- sapply(decathlon_normalized, function(x) {
  valid_data <- x[!is.na(x) & is.finite(x)]
  if (length(valid_data) >= 3) {
    shapiro.test(valid_data)$p.value
  } else {
    NA  # Not enough valid data to perform the test
  }
})

# Output results
print("Shapiro-Wilk results after scaling:")
print(shapiro_results_after)



# Print structure of the dataset
# Get the data type of each column in the data frame
# Plot histograms for a subset of columns
par(mfrow=c(2,2))  # Adjust the layout as needed
hist(decathlon_normalized[[1]], main="Histogram for Column 1")
hist(decathlon_normalized[[2]], main="Histogram for Column 2")
hist(decathlon_normalized[[3]], main="Histogram for Column 3")
hist(decathlon_normalized[[4]], main="Histogram for Column 4")



# Summary of the dataset to check for any anomalies like infinite values or incorrect data types
summary(decathlon_normalized)




# The result of the correlation matrix can be interpreted as follow: 
# The higher the value, the most positively correlated the two variables are.
# The closer the value to -1, the most negatively correlated they are.

# 4. PCA on the normalized data
pca_decathlon <- PCA(decathlon_normalized, graph = FALSE)

# 5. Explained variance by components
print(pca_decathlon$eig)
# plotting for easier interpretation
eig_vals <- pca_decathlon$eig[, 1]
total_variance <- sum(eig_vals)
perc_variance <- (eig_vals / total_variance) * 100

# Adjust the margins to accommodate larger text
par(mar=c(7, 7, 7, 7))

# Create a barplot with larger text
bp <- barplot(perc_variance, main="Scree Plot", 
              ylim=c(0, max(perc_variance)+5),
              col="lightblue", border="darkblue",
              cex.main=3.0, cex.lab=2.6, cex.axis=2.2, cex.names=2.2)

# Overlay a line plot on the bar plot with larger points
lines(bp, perc_variance, type="o", col="blue", pch=16, lwd=2, cex=2.2)

# Optionally, add text labels with the percentage values with adjusted size
text(bp, perc_variance, labels=round(perc_variance, 2), pos=3, cex=1.5)

# Use mtext to manually add axis labels with increased separation from the axis
mtext("Principal Components", side=1, line=5, cex=2.6)  # For the x-axis
mtext("Percentage of Variance", side=2, line=5, cex=2.6)  

# 6. Loading scores for the variables on principal components
print(pca_decathlon$var$coord)
corrplot(pca_decathlon$var$coord[,1:2], is.corr=FALSE)

# 7. Scores of the first two principal components for observations
print(pca_decathlon$ind$coord[,1:2])
plot(pca_decathlon$ind$coord[,1:2], pch=19, xlab="PC1", ylab="PC2", main="Scores on the First Two Principal Components")
text(pca_decathlon$ind$coord[,1:2], labels=row.names(decathlon), cex=0.7, pos=4)

# 8. Supplementary individuals (if any)
# Here you can plot any supplementary individuals if applicable.
# Example: plot(pca_decathlon, choix="ind", habillage="none", invisible="ind")

# 9. Contributions of variables
barplot(pca_decathlon$var$contrib[,1], main="Contributions of Variables to PC1", names.arg=colnames(decathlon_normalized), las=2)

# 10. Biplot of the first two principal components
biplot(pca_decathlon, cex=c(0.7, 0.7), col=c("blue", "red"))

# Continue with further analyses or interpretation as required.
