library(fastICA)
library(pls)
set.seed(12345, sample.kind="Rounding")

# STEP 1 - Standard PCA by space feature
spectra_data<- read.csv2("NIRspectra.csv", dec=",")
head(spectra_data)


subdata=spectra_data
subdata$Viscosity=c()
pca.spectra=prcomp(subdata)

screeplot(pca.spectra) #Plot - Variation by feature

lambda=pca.spectra$sdev^2 #eigenvalues
sprintf("%2.3f",lambda/sum(lambda)*100) #proportion of variation

## PC1 and PC2 explains 93.3% + 6.3% = 99.6% > 99%

plot(pca.spectra$x[,1], pca.spectra$x[,2], xlab="PC1", ylab="PC2") #Plot of scopca.spectra PC1 and PC2
# There are 2(+5) unusual diesel fuels



## STEP 2 - Trace plot of PC1 and PC2

pca.spectra_rot = pca.spectra$rotation
plot(pca.spectra_rot[,1], main="Traceplot - PC1")
plot(pca.spectra_rot[,2], main="Traceplot - PC2")
# PC2 is mainly explained by the last 10 spectra.


## STEP 3 - Independent Component Analysis

set.seed(12345, sample.kind="Rounding")
ica.spectra = fastICA(subdata, n.comp = 2, alg.typ = "parallel", fun = "logcosh", alpha = 1, method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE)

#a) 
## W' = K * W, 
## K = pre-whitening matrix that projects data onto the first n.com principal componenets
## W = estimated un.mixing matrix
ica.spectra_Winv = ica.spectra$K %*% ica.spectra$W
plot(ica.spectra_Winv[,1], main="Traceplot - W' Col 1 ")
plot(ica.spectra_Winv[,2], main="Traceplot - W' Col 2")

#b)
plot(ica.spectra$S[,1], ica.spectra$S[,2], main = "Score", ylab = "Latent 2", xlab = "Latent 1")

