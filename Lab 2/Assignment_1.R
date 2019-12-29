setwd("~/Programming/TDDE01/Lab 2")

australian_crabs <- read.csv("australian-crabs.csv")
set.seed(12345)


#1
library(MASS)
library(ggplot2)
str(australian_crabs)

australian_crabs.gender = split(australian_crabs,australian_crabs$sex)

#Plots matrix in different colors in regards to gender
plot_crabs <- ggplot(australian_crabs, aes(x=CL, y=RW, color=australian_crabs$sex)) + geom_point()
plot_crabs

# 


#2

#LDA-model with target sex and features CL and RW. Gives proportional prior
lda_crabs <- lda(sex~RW+CL, data=australian_crabs)
print(lda_crabs)

#Predicts the LDA-model
lda_crabs.predicted = predict(lda_crabs)

#Scatterplots the lda-model in different colors in regard to sex
plot_crabs_lda <- ggplot(australian_crabs, aes(x=CL, y=RW, color=lda_crabs.predicted$class)) + geom_point()
plot_crabs_lda

confusionmatrix.lda <- table(lda_crabs.predicted$class, australian_crabs$sex)
print(confusionmatrix)

missclassification.lda <- 1-(sum(diag(confusionmatrix.lda))/sum(confusionmatrix.lda))
print(missclassification.lda)
#It is good

#3
#Sets the priors to 0.1=female and 0.9=male
lda_crabs_prior.predicted = predict(lda_crabs, prior=c(0.1,0.9))
#Plots like #2
plot_crabs_lda_prior <- ggplot(australian_crabs, aes(x=CL, y=RW, color=lda_crabs_prior.predicted$class)) + geom_point()
plot_crabs_lda_prior

confusionmatrix_prior.lda <- table(lda_crabs_prior.predicted$class, australian_crabs$sex)

print(confusionmatrix_prior.lda)

missclassification_prior.lda <- 1-(sum(diag(confusionmatrix_prior.lda))/sum(confusionmatrix_prior.lda))
print(missclassification_prior.lda)


#4

#Gives the GLM for the target sex with features RW and CL
glm_crabs = glm(sex~RW+CL, family=binomial, data=australian_crabs)
glm_crabs.predicted = predict(glm_crabs, type="response")
print(glm_crabs.predicted)
#Split data into Male and Female depending on the predicted value
glm_crabs.predicted <- ifelse(glm_crabs.predicted > 0.5, "Male", "Female") #Split up the model into spam and not spam
print(glm_crabs.predicted)

plot_glm_crabs <- ggplot(australian_crabs, aes(x=CL, y=RW, color=glm_crabs.predicted)) + geom_point()
plot_glm_crabs

confusionmatrix.glm <- table(glm_crabs.predicted, australian_crabs$sex)
print(confusionmatrix.glm)

missclassification.glm <- 1-(sum(diag(confusionmatrix.glm))/sum(confusionmatrix.glm))
print(missclassification.glm)


plot_glm_line <- plot_glm_crabs + stat_function(fun=function(x){(glm_crabs[["coefficients"]][["(Intercept)"]]
                                                  +glm_crabs[["coefficients"]][["CL"]]*x)/(-glm_crabs[["coefficients"]][["RW"]]) }, 
                                                xlim=c(5,50))
plot_glm_line



