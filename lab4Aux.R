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
library(FactoMineR)

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

  # 2.3 Tests without 1500m and Javeline
  m1500 <- which(colnames(decathlon_numeric) == "1500m")
  Javeline <- which(colnames(decathlon_numeric) == "Javeline")

  # 2.4 Correlation matrix
  decathlon_numeric_fixed <- decathlon_numeric[,-c(m1500, Javeline)]
  corr_matrix <- cor(decathlon_numeric_fixed)

  # 2.4.1 Bartlett's Test of Spherecity
  n<-nrow(decathlon_numeric_fixed)
  cortest.bartlett(corr_matrix,n)

  # 2.4.2 Kaiser-Meyer-Olkin (KMO) Test
  kmo(decathlon_numeric_fixed)

# 3. Correlation matrix
corr_matrix <- cor(decathlon_numeric_fixed)
corrplot(corr_matrix)

# 4. PCA on the normalized data
decathlon_normalized <- scale(decathlon_numeric_fixed)
pca_decathlon <- PCA(decathlon_normalized, graph = FALSE)

# 5. Eigenvalues and explained variance by components
print(pca_decathlon$eig)

# 5.1 Scree plot
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
corrplot(pca_decathlon$var$coord[,1:5], is.corr=FALSE)


# 7. Biplot of the first two principal components
printAux<-princomp(decathlon_normalized,cor=TRUE) 
printAux$loadings
biplot(printAux)

corrplot(pca_decathlon$var$coord, is.corr=FALSE)