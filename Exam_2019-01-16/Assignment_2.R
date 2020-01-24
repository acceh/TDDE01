library(neuralnet) 
library(Metrics)
library(ggplot2)
set.seed(1234567890)
Var <- runif(50, 0, 3)
tr <- data.frame(Var, Sin=sin(Var)) 
Var <- runif(50, 3, 9)
te <- data.frame(Var, Sin=sin(Var))



winit <- runif(10, -1, 1)

mse_test <- numeric()
mse_train <- numeric()
threshold <- numeric()

for(i in 1:10) {
	nn <- neuralnet(Sin ~ Var, data = tr, startweights = winit, hidden = c(3),
									threshold = i/1000)
	pred_train <- compute(nn, covariate=tr)$net.result
	pred_test <- compute(nn, covariate=te)$net.result
	threshold[i] <- i/1000
	mse_test[i] <- mse(pred_test, te$Sin)
	mse_train[i] <- mse(pred_train, tr$Sin)
	print(i)
}
plot(threshold, mse_train, type="o", ylab="MSE", main = "Training dataset")
plot(threshold, mse_test, type="o", ylab="MSE", main = "Test dataset")

nn <- neuralnet(Sin ~ Var, data = tr, startweights = winit, hidden = c(3))


pred_nn <- predict(nn, newdata=te)
plot_net <- ggplot() + geom_point(aes(te$Var, te$Sin, color="Testing data")) + geom_point(aes(tr$Var, tr$Sin, color="Training data")) +
	geom_point(aes(te$Var, data.frame(pred_nn)$pred_nn, colour = "Predicted data"))  + scale_color_manual(name =
																																														 	'', values = c('Predicted data' = 'red',
																																														 								 'Training data' = 'black',
																																														 								 'Testing data' = 'blue'))
plot_net

#SVM

RNGversion('3.5.1') 
library("kernlab")
data(spam)
data <- spam
n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.8))
train = data[id, ]
test = data[-id, ]

width <- 0.05
error <- c()

for (i in seq(1,35,1)) {
	print(i)
	filter<-
		ksvm(type ~ .,
				 data = train,
				 kernel = rbfdot(sigma = width),
				 C = i)
	confMa <- table(predict(filter, test), test$type)?
	error[i] <- 1- sum(diag(confMa))/sum(confMa)
}
plot(error)

match(min(error), error)



error.cv <- c()

for (i in seq(1,35,1)) {
	print(i)
	filter<-
		ksvm(type ~ .,
				 data = train,
				 kernel = rbfdot(sigma = width),
				 C = i,
				 cross=5)
	
	error.cv[i] <- filter@error
}

plot(error.cv)
match(min(error.cv), error.cv)

filter <-ksvm(type ~ .,
              data = train,
              kernel = rbfdot(sigma = width),
              C = 29)
predict(filter)
confMa <- table(predict(filter, test), test$type)
missCl <- 1- sum(diag(confMa))/sum(confMa)
missCl
