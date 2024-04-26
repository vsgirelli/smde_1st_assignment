###############################################################
######                                                   ######
###### R SCRIPT: Multiple Linear Regression and PCA      ######                      
######                                                   ######
######           Nihan Acar-Denizli, PhD                 ######
######                                                   ######
###############################################################


###############################################################
#### Linear Regression on Heptathlon Data Set ################
###############################################################

# Installing and loading necessary packages
install.packages("HSAUR")

data("heptathlon", package = "HSAUR")
plot(heptathlon)
head(heptathlon)

#Changing direction of some variables
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

### Correlation Matrix ###
cor(heptathlon)
plot(heptathlon)

### Correlation with score variable ###
cor(heptathlon)[8,]

### Simple Linear Regression ####
slr<-lm(score~longjump,data=heptathlon)
summary(slr)


### Multiple Linear Regression Model ###
reg_model1<-lm(score~., data=heptathlon)
summary(reg_model1)


### Testing Regression Assumptions ###
### 1. Normality of the Error Term ###
# Using QQ plot
qqnorm(residuals(reg_model1))
# Using Histogram
hist(residuals(reg_model1))
# It is skewed.
#Shapiro Wilks Test
shapiro.test(residuals(reg_model1))
# The error term does not follow a Normal distribution. (p<0.05)

### 2. Homogenity of Variance ###
# Residual Analysis #
plot(residuals(reg_model1))
##Breusch Pagan Test
library(lmtest)
bptest(reg_model1)
# H0 is accepted (p>0.05).
# The homogenity of variances is provided.

### 3. The independence of errors ### 
dwtest(reg_model1, alternative = "two.sided")
# There is not an autocorrelation in the data set (p>0.05).
# The errors/observations are independent.

### 4. Multicollinearity ###

## Correlation among Independent Variables ##
score <- which(colnames(heptathlon) == "score")
plot(heptathlon[,-score])
cor(heptathlon[,-score])

library(car)
vif(reg_model1)

## Eliminating longjump from data set
reg_model2<-lm(score~highjump+shot+run200m+javelin+run800m, data=heptathlon)
summary(reg_model2)

vif(reg_model2)

###1. Normality###
#Shapiro Wilks Test
shapiro.test(residuals(reg_model2))
# Using Histogram
hist(residuals(reg_model2))

### 2. Homogenity of Variance ###
# Residual Analysis #
plot(residuals(reg_model2))
##Breusch Pagan Test
library(lmtest)
bptest(reg_model2)

### 3. The independence of errors ### 
dwtest(reg_model2, alternative = "two.sided")

#### Stepwise Regression ###
step(reg_model2)

### Comparison of Regression models ###
anova(reg_model1, slr)
anova(reg_model2, slr)

anova(reg_model1, reg_model2)

#####################################
###### PCA On Heptathlon Data #######
#####################################
##### PCA with FactoMineR ###########
#####################################

###### Bartlett's Test of Spherecity#####

install.packages("psych")
library(psych)

## Bartlett test requires the computation of the correlation matrix R and the number of observations.
R<-cor(heptathlon)
print(R)

## Bartlett tests if the correlation matrix is an identity matrix. 
#### (H0:R=I) ##
### If we reject the null hypothesis the variables are correlated.
n<-nrow(heptathlon)
cortest.bartlett(R,n)


###### Kaiser-Meyer-Olkin (KMO) Test ###
## We can use kmo function written by Prof. Shigenobu Aok.
### (http://minato.sip21c.org/swtips/factor-in-R.pdf)
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

#KMO index
kmo(heptathlon)
kmo(heptathlon[,-score])

### PCA of heptathlon data ######
library(FactoMineR)
?PCA

pca_hep<-PCA(heptathlon,quanti.sup = 8)

### Explained Variation ###
pca_hep$eig
plot(pca_hep$eig[,1], type="o", main="Scree Plot")

## Component Loadings ## 
pca_hep$var$coord
pca_hep$var$coord[,1:2]

# Scores of PC1 and PC2
pca_hep$ind$coord[,1:2]
plot(pca_hep)

### Principal Component Regression ###
heptathlon$PC1<-pca_hep$ind$coord[,1]
heptathlon$PC2<-pca_hep$ind$coord[,2]
cor(heptathlon)

reg_pc<-lm(score~PC1 + PC2, data=heptathlon)
summary(reg_pc)

##### Assumptions ###
#1. Normality
#Shapiro Wilks Test
shapiro.test(residuals(reg_pc))
# The error term does not follow a Normal distribution. (p<0.05)

### 2. Homogenity of Variance ###
# Residual Analysis #
plot(residuals(reg_pc))
##
rownames(heptathlon)

##Breusch Pagan Test
bptest(reg_pc)
# H0 is accepted (p>0.05).
# The homogenity of variances is provided.

### 3. The independence of errors ### 
dwtest(reg_pc, alternative = "two.sided")
# There is not an autocorrelaiton in the data set (p>0.05).
# The errors/observations are independent.

### PCA without outlier ###
pca_hep2<-PCA(heptathlon[,1:8],quanti.sup = 8,ind.sup = 12)

reg_pc2<-lm(score~ pca_hep2$ind$coord[,1] + pca_hep2$ind$coord[,2], data=heptathlon[-12,])
summary(reg_pc2)

#1. Normality
#Shapiro Wilks Test
shapiro.test(residuals(reg_pc2))
# The error term follows a Normal distribution. (p<0.05)

### 2. Homogenity of Variance ###
# Residual Analysis #
plot(residuals(reg_pc2))

##Breusch Pagan Test
bptest(reg_pc2)
# H0 is accepted (p>0.05).
# The homogenity of variances is provided.

### 3. The independence of errors ### 
dwtest(reg_pc, alternative = "two.sided")

#### Varimax Rotation ###
?varimax
varimax(pca_hep2$var$coord[,1:2])

#####################################
### Example: USArrests data   #######
### USArrests data PCA and FA #######
#####################################

#install.packages("HSAUR")
library(HSAUR)
data<-USArrests

### PCA with princomp function (based on spectral value decomposition) ##
?princomp
pcUSA<-princomp(data,cor=TRUE) 
summary(pcUSA)

eigs<-pcUSA$sdev^2
eigs
plot(eigs,type="b")

pcUSA$loadings
biplot(pcUSA)

#### PCA with prcomp function (based on singular value decompositon)###
prusa<-prcomp(data,scale. = TRUE)
summary(prusa)

prusa$sdev^2
prusa$rotation

biplot(prusa)

##########################################
### PCA to Factor computations
#aij=sqrt(eig_j)*u_ji
a_11<-pcUSA$loadings[1,1]*sqrt(eigs[1])
a_12<-pcUSA$loadings[1,2]*sqrt(eigs[2])
eigs[1]

# Factor correlations
pcUSA$loadings[1,]*sqrt(eigs)
pcUSA$loadings[2,]*sqrt(eigs)
pcUSA$loadings[3,]*sqrt(eigs)
pcUSA$loadings[4,]*sqrt(eigs)

### Communalities
com1<-(pcUSA$loadings[1,]*sqrt(eigs))^2
sum(com1[1:2])
#########################################

#### FACTOR ANALYSIS OF USA arrest data ###
usafa <- PCA(data)
summary(usafa)
names(usafa)

usafa$eig

##Individual scores
usafa$ind$coord

#factor loadings
usafa$var
names(usafa$var)
usafa$var$cor
usafa$var$coord
usafa$var$coord

#Square of factor loadings communalities of variables
usafa$var$cos2
apply(usafa$var$cos2[,1:2],1,sum)

# Contributions of variables at each dimension 
usafa$var$contrib
apply(usafa$var$contrib[,1:2],2,sum)

#####################################
## Factor correlations to PC loadings
x1coef<-usafa$var$cor[1,]/sqrt(eigs)
x2coef<-usafa$var$cor[2,]/sqrt(eigs)
x3coef<-usafa$var$cor[3,]/sqrt(eigs)
x4coef<-usafa$var$cor[4,]/sqrt(eigs)

rbind(x1coef,x2coef,x3coef,x4coef)
#####################################


