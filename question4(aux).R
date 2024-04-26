
# install.packages("psych")
library(dplyr)
library(corrplot)
library(psych)

# install.packages("FactoMineR")
library(FactoMineR)
data("decathlon")

plot(decathlon)
# str(decathlon)

non_quant <- sapply(decathlon, function(x) !is.numeric(x))
quant <- select(decathlon, -Competition)
quant_no_points <- select(quant, -Points)

correlation_matrix <- cor(quant)
print(correlation_matrix)

corrplot(correlation_matrix, method = "color", tl.col = "black", tl.cex = 1, cl.cex = 0.8)

n<-nrow(quant)
cortest.bartlett(correlation_matrix, n)

KMO(quant)
KMO(quant_no_points)

pca_quant <- PCA(quant)
pca_quant$eig
# plot(pca_quant$eig[,1], type="o", main="Scree Plot")
# 
# pca_quant$var$coord
# pca_quant$var$coord[,1:2]
# 
# pca_quant$ind$coord[,1:2]
plot(pca_quant)

