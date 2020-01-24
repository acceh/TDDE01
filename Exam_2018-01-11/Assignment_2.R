library("kernlab")
data(spam)

data <- spam

n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.5))
train = data[id, ]
test = data[-id, ]

width <- 0.05
filter_0_5 <-
	ksvm(type ~ .,
			 data = train,
			 kernel= rbfdot(sigma=width),
			 C=0.5)

filter_1 <-
	ksvm(type ~ .,
			 data = train,
			 kernel= rbfdot(sigma=width),
			 C=1)

filter_5 <-
	ksvm(type ~ .,
			 data = train,
			 kernel= rbfdot(sigma=width),
			 C=5)

missCl <- function(data) {
	return(1 - sum(diag(data)) / sum(data))
}

filter_0_5.pred.train <- predict(filter_0_5, newdata = train)
filter_0_5.pred.test <- predict(filter_0_5, newdata = test) 
filter_0_5.confMa.train <-
	table(train$type , filter_0_5.pred.train) 
print("Confma for C=0.5 with training data: ")
filter_0_5.confMa.train
filter_0_5.confMa.test <- table(test$type , filter_0_5.pred.test)
print("Confma for C=0.5 with training data: ")
filter_0_5.confMa.test
filter_0_5.missCl.train <- missCl(filter_0_5.confMa.train)
print("Misscl for C=3 with training data: ")
filter_0_5.missCl.train
filter_0_5.missCl.test <- missCl(filter_0_5.confMa.test)
print("Misscl for C=3 with testing data: ")
filter_0_5.missCl.test
filter_1.pred.train <- predict(filter_1, newdata = train)
filter_1.pred.test <- predict(filter_1, newdata = test)
filter_1.confMa.train <- table(train$type , filter_1.pred.train)
print("Confma for C=3 with training data: ")
filter_1.confMa.train
filter_1.confMa.test <- table(test$type , filter_1.pred.test)
print("Confma for C=3 with training data: ")
filter_1.confMa.test
filter_1.missCl.train <- missCl(filter_1.confMa.train)
print("Misscl for C=3 with training data: ")
filter_1.missCl.train
filter_1.missCl.test <- missCl(filter_1.confMa.test)
print("Misscl for C=3 with testing data: ")
filter_1.missCl.test
filter_5.pred.train <- predict(filter_5, newdata = train)
filter_5.pred.test <- predict(filter_5, newdata = test)
filter_5.confMa.train <- table(train$type , filter_5.pred.train)
print("Confma for C=5 with training data: ")
filter_5.confMa.train
filter_5.confMa.test <- table(test$type , filter_5.pred.test)
print("Confma for C=5 with training data: ")
filter_5.confMa.test
filter_5.missCl.train <- missCl(filter_5.confMa.train)
print("Misscl for C=5 with training data: ")
filter_5.missCl.train
filter_5.missCl.test <- missCl(filter_5.confMa.test)
print("Misscl for C=5 with testing data: ")
filter_5.missCl.test
print("Summary: ")
filter_0_5.missCl.train
filter_1.missCl.train
filter_5.missCl.train
print("Summary: ")
filter_0_5.missCl.test
filter_1.missCl.test
filter_5.missCl.test

#Best is C=1
filter_1
#Above prints the model



#2 

library(neuralnet)
library(ggplot2)
set.seed(1234567890)
#50 values between 0 and 10
Var <- runif(50, 0, 10)
# Create dataset
trva <- data.frame(Var, Sin=sin(Var))
# Divide dataset into training and validation set
tr <- trva[1:25,] # Training
va <- trva[26:50,] # Validation
# Random initialization of the weights in the interval [-1, 1]
# 31 weights are used
# Nw = (I+1)*H1 +(H1+1)*H2 +(H2+1)*O, where H is hidden
# layer, I is inputs and O is outputs.
# So in this case it is Nw=(1+1)*10+(10+1)*1=31
winit <- runif(31, -1, 1)
# Function predicting MSE
mse <- function(prediction, observation) {
	return (mean((observation - prediction)^2))
}
mse_val <- numeric()
mse_train <- numeric()
threshold <- numeric()
m_sq_err <- function(pred, obs) {
	return (mean((observation - prediction)^2))
}
for(i in 1:10) {
	nn <- neuralnet(Sin ~ Var, data = tr, startweights = winit, hidden = c(10),
									threshold = i/1000, lifesign = "minimal")
	pred_train <- compute(nn, covariate=tr)$net.result
	pred_val <- compute(nn, covariate=va)$net.result
	threshold[i] <- i/1000
	mse_val[i] <- mse(pred_val, va$Sin)
	mse_train[i] <- mse(pred_train, tr$Sin)
	print(i)
}
plot(threshold, mse_val, type="o", ylab="MSE", main = "Validation dataset")
plot(threshold, mse_train, type="o", ylab="MSE", main = "Training dataset")

# Two hidden layers
# (1+1)*3+(3+1)*3+(3+1)*1 = 22
winit <- runif(22, -1, 1)
mse_val2 <- numeric()
mse_train2 <- numeric()
threshold2 <- numeric()

for(i in 1:10) {
	nn2 <- neuralnet(formula = Sin ~ Var, data = tr, hidden = c(3,3), startweights = winit,
									threshold = i/1000, lifesign = "full")
	
	pred_train2 <- compute(nn2, covariate=tr)$net.result
	pred_val2 <- compute(nn2, covariate=va)$net.result
	threshold2[i] <- i/1000
	mse_val2[i] <- mse(pred_val2, va$Sin)
	mse_train2[i] <- mse(pred_train2, tr$Sin)
	print(i)
}

plot(threshold2, mse_val2, type="o", ylab="MSE", main = "Validation dataset two hidden layers")
plot(threshold2, mse_train2, type="o", ylab="MSE", main = "Training dataset two hidden layers")
