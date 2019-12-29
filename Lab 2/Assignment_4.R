setwd("~/Programming/TDDE01/Lab 2")
library(fastICA)

data <- read.csv2("NIRSpectra.csv")

data1 <- data

#1
#Conduct a standard PCA by using the feature space and provide 
#a plot explaining how much variation is explained by each feature. 
#Does the plot show how many PC should be extracted? Select the minimal 
#number of components explaining at least 99% of the total variance. 
#Provide also a plot of the scores in the coordinates (PC1, PC2). 
#Are there unusual diesel fuels according to this plot?
data1$Viscosity = c()

res = prcomp(data1)

lambda = res$sdev^2

lambda


sprintf("%2.3f", (lambda)/sum(lambda)*100)

#Histogram of variance
screeplot(res)
#As can be seen in the plot above in combination with the 
#lambda there are two features that explains $99\%$ of the total variance.
plot(res$x[,1], res$x[,2], main = "PC1 vs. PC2", xlab = "PC1", ylab = "PC2")
#The plot above shows the diesel fuels according to 
#the features PC1 and PC2. There are a few outliers in this plot. 
#Mainly the ones with a high value of the PC1.


#2 
#Make trace plots of the loadings of the components selected in step 1. 
#Is there any principle component that is explained by mainly a 
#few original features?
U <- res$rotation
plot(U[,1], main="Traceplot for PC1")
#The plot above shows the traceplot for PC1. As can be seen 
#in the plot it is not mainly explained by just a few few original 
#features, but by a lot of them, but not so much by the ones around index 105.

plot(U[,2], main="Traceplot for PC2")
#The plot abovee shows the traceplot for PC2. As can be 
#seen in the plot it is mainly explained by a feew features 
#around the higher index around 120.

#3 
#Perform Independent Component Analysis with the number of 
#components selected in step 1 (set seed 12345). 
#Check the documentation for the fastICA method in R and do the following:
#  a. Compute W'= K ⋅ W and present the columns of 𝑊𝑊′ in form of the trace plots. Compare with the trace plots in step 2 and make conclusions. What kind of measure is represented by the matrix 𝑊𝑊′?
#  b. Make a plot of the scores of the first two latent features and compare it with the score plot from step 1.

set.seed(12345)
ICA <- fastICA(data1, n.comp = 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
               method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001,
               verbose = TRUE)

WTICK <- ICA$K %*% ICA$W

# Trace plot results from ICAs
plot(WTICK[,1], main= "Latent feature 1")
plot(WTICK[,2], main= "Latent feature 2")

#The two plots above both shows the latent features for the the W' 
#columns 1 and 2. As can be seen they are inverted along 
#the y-axis compared to the plots for PC1  and PC2.

# Plot of scores
plot(ICA$S[,1], ICA$S[,2], main = "Score", ylab = "Latent 2", xlab = "Latent 1")
#This plot is also a inverted version of the corresponding 
#plot, PC1 vs PC2, but along the x-axis.

