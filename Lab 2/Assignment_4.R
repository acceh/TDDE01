setwd("~/Programming/TDDE01/Lab 2")
library(fastICA)


data <- read.csv2("NIRSpectra.csv")


data1 <- data


#1
data1$Viscosity = c()

res = prcomp(data1)

lambda = res$sdev^2

lambda


sprintf("%2.3f", (lambda)/sum(lambda)*100)

#Histogram of variance
screeplot(res)

plot(res$x[,1], res$x[,2], main = "PC1 vs. PC2", xlab = "PC1", ylab = "PC2")


#2 
U <- res$rotation
plot(U[,1], main="Traceplot for PC1")
plot(U[,2], main="Traceplot for PC2")

#3 

set.seed(12345)
ICA <- fastICA(data1, n.comp = 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
               method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001,
               verbose = TRUE)

WTICK <- ICA$K %*% ICA$W


# Trace plot results from ICAs
plot(WTICK[,1], main= "Latent feature 1")
plot(WTICK[,2], main= "Latent feature 2")

# Plot of scores
plot(ICA$S[,1], ICA$S[,2], main = "Score", ylab = "Latent 2", xlab = "Latent 1")