library(readxl)
library(Metrics)
library(MASS)
library(ggplot2)
library(glmnet)
setwd("~/Programming/TDDE01/Lab 1")

set.seed(12345)
RNGversion('3.5.1')

#1
#Import data to R and create a plot of Moisture versus Protein. 
#Do you think that these data are described well by a linear model?

#Imports file
tecator <- read_excel("tecator.xlsx")
moisture = tecator$Moisture;
protein = tecator$Protein;

#Plots the moisture and protein
plot(protein, moisture, xlab="Moisture", ylab="Protein", main="Moisture vs. Protein")

#Quite well with a linear model. Prob quite large deviations sometimes though

#Below is just to see the linear model
data_moisture_protein = tecator[,103:104]

fit1 <- lm(formula = moisture ~ protein, data=data_moisture_protein)
summary(fit1)
#Gives the 
fitted1 = predict(fit1, interval="confidence")
#Plots line with protein as x and the new predicted fitted values as y
lines(protein, fitted1[, "fit"])
#This shows the line M1

#2
#Consider model 𝑀𝑀𝑖𝑖 in which Moisture is normally distributed, and the expected Moisture
#is a polynomial function of Protein including the polynomial terms up to power 𝑖𝑖
#(i.e M1 is a linear model, M2 is a quadratic model and so on). 
#Report a probabilistic model that describes 𝑀𝑀𝑖𝑖 . Why is it appropriate to use 
#MSE criterion when fitting this model to a training data?

#Consider the functions are:
#M1 = w0 + w1*x1 + e
#M2 = w0 + w1*x1 + w2*x2^2 + e
#and so on

#3
#Divide the data into training and validation sets( 50%/50%) and fit models 𝑀𝑀𝑖𝑖 , 𝑖𝑖 = 1 ... 6. 
#For each model, record the training and the validation MSE and present a plot showing how 
#training and validation MSE depend on i (write some R code to make this plot). 
#Which model is best according to the plot? How do the MSE values change and why? 
#Interpret this picture in terms of bias-variance tradeoff.

#Splits the data into training set and test set using only moisture and protein cols
n=dim(tecator[,103:104])
set.seed(12345)
id=sample(1:n[1], floor(n*0.5))
train=tecator[id,103:104]
test=tecator[-id,103:104]

#Splits up the test and training data to protein and moisture respectively
p_train = train$Protein
m_train = train$Moisture
p_test = test$Protein
m_test = test$Moisture


#Models below for M1-M6, regression
m1_model = lm(formula = Moisture ~ Protein, data=train)
m2_model = lm(formula = Moisture ~ Protein + I(Protein^2), data = train)
m3_model = lm(formula = Moisture ~ Protein + I(Protein^2) + I(Protein^3), data = train)
m4_model = lm(formula = Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4), data = train)
m5_model = lm(formula = Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4) + I(Protein^5), data = train)
m6_model = lm(formula = Moisture ~ Protein + I(Protein^2) + I(Protein^3) + I(Protein^4) + I(Protein^5) + I(Protein^6), data = train)

#Predictions with the help of the training data
m1_model_pred = predict(m1_model, newdata=train)
m2_model_pred = predict(m2_model, newdata=train)
m3_model_pred = predict(m3_model, newdata=train)
m4_model_pred = predict(m4_model, newdata=train)
m5_model_pred = predict(m5_model, newdata=train)
m6_model_pred = predict(m6_model, newdata=train)

#Predictions with the help of the test data
m1_model_pred_test = predict(m1_model, newdata=test)
m2_model_pred_test = predict(m2_model, newdata=test)
m3_model_pred_test = predict(m3_model, newdata=test)
m4_model_pred_test = predict(m4_model, newdata=test)
m5_model_pred_test = predict(m5_model, newdata=test)
m6_model_pred_test = predict(m6_model, newdata=test)


#Mean squared error for the training data
mse_train <- vector()
mse_train[1] <- mse(m_train, m1_model_pred)
mse_train[2] <- mse(m_train, m2_model_pred)
mse_train[3] <- mse(m_train, m3_model_pred)
mse_train[4] <- mse(m_train, m4_model_pred)
mse_train[5] <- mse(m_train, m5_model_pred)
mse_train[6] <- mse(m_train, m6_model_pred)

for (i in 1:6) {
  cat("MSE for training data with i=", i)
  print(mse_train[i])
}

#Mean squared error for the testing data
mse_test <- vector()
mse_test[1] <- mse(m_test, m1_model_pred_test)
mse_test[2] <- mse(m_test, m2_model_pred_test)
mse_test[3] <- mse(m_test, m3_model_pred_test)
mse_test[4] <- mse(m_test, m4_model_pred_test)
mse_test[5] <- mse(m_test, m5_model_pred_test)
mse_test[6] <- mse(m_test, m6_model_pred_test)

for (i in 1:6) {
  cat("MSE for testing data with i=", i)
  print(mse_test[i])
}

plot(1:6, mse_train, col="blue", type="l", xlab="i", ylab="MSE", ylim=c(23,45), main="MSE dependencie of i. Where green = test, blue= train")
lines(1:6, mse_test,col="green")



#4

fitted_fat <- lm(tecator$Fat ~ ., data=tecator[,2:101])
steps <- stepAIC(fitted_fat, direction="both")

step$anova

print("Number of selected variables:")
print(length(steps$coefficients)-1) 
#5
#Fit a Ridge regression model with the same predictor and response variables. 
#Present a plot showing how model coefficients depend on the log of the penalty factor lambda and report how the coefficients change with lambda.
#Takes the scaled tecator of Channel1-100

## Scale takes (x - mean(x)) / sd(x))
covariates=scale#Scales all varables other than 
(tecator[,2:101])
#Scales the response-variable, which in this case is the fat
response=scale(tecator[,102])
#Using glmnet with alpha=0 gives the Ridge-Regression
model_ridge=glmnet(as.matrix(covariates), response, alpha=0,family="gaussian")
plot(model_ridge, xvar="lambda", label=TRUE, main="Ridge Regression\n")

#6
#Repeat step 5 but fit LASSO instead of the Ridge regression and compare the plots from steps 5 and 6. Conclusions?
model_lasso = glmnet(as.matrix(covariates), response, alpha = 1, family="gaussian")
plot(model_lasso, xvar="lambda", label=TRUE, main="Lasso Regression\n")

#7
#Use cross-validation to find the optimal LASSO model (make sure that case
#𝜆𝜆 = 0 is also considered by the procedure) , report the optimal 𝜆𝜆 and how many variables were chosen by the model and make conclusio
#ns. Present also a plot showing the dependence of the CV score and comment how the CV score changes with 𝜆𝜆.


cv_lasso_model = cv.glmnet(as.matrix(covariates), response, alpha=1, family="gaussian", nfolds=40)
plot(cv_lasso_mode1, main="Plot CV")
coef(cv_lasso_model, s="lambda.min")
print(cv_lasso_model$lambda.min)
