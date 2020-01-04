setwd("~/Programming/TDDE01/Exam_2017-04-17")
RNGversion('3.5.1')

library(kernlab)
#In this assignment, you are asked to use the R package kernlab to learn SVMs for 
#classifying the spam dataset that is included with the package. Consider the radial 
#basis function kernel (also known as Gaussian) with a width of 0.05. For the C 
#parameter, consider values 1, 10 and 100.
#(2p) Estimate the error for the three values of C. Use cross-validation with 2 folds. 
#Hint: Use the argument cross=2 when calling the function ksvm. Use the function cross() 
#to print out the error estimate. Use set.seed(1234567890).
#(2p) In the previous question, the error estimate may not be mono- tone with respect to 
#the value of C. Explain why this happens.
data(spam)

data <- spam
set.seed(1234567890)
model1 <-
  ksvm(
    type ~ .,
    C = 1,
    kernel = rbfdot(sigma = 0.05),
    cross = 2,
    data = data
  )
set.seed(1234567890)

model10 <-
  ksvm(
    type ~ .,
    C = 10,
    kernel = rbfdot(sigma = 0.05),
    cross = 2,
    data = data
  )
set.seed(1234567890)

model100 <-
  ksvm(
    type ~ .,
    C = 100,
    kernel = rbfdot(sigma = 0.05),
    cross = 2,
    data = data
  )

cross(model1)
cross(model10)
cross(model100)

#NN
#In this assignment,you are asked to use the Rpackage neuralnet to train a NN to learn the trigonometric sine function. 
#To produce the learning data, sample 50 points uniformly at random in the interval [0, 10] and, then, 
#apply the sine function to each point.Your task is to estimate the mean squared error of a 
#NN with a single hidden layer of 10 units for the regression task described above. Use cross-validation 
#with 2 folds. For the training, initialize the weights of the NN to random values in the interval [−1, 1]. 
#Stop the training when the partial derivatives of the error function are below a threshold value of 0.001.
library(neuralnet)
set.seed(1234567890)
Var <- runif(50, 0, 10)
tr <- data.frame(Var, Sin = sin(Var))
tr1 <- tr[1:25, ] # Fold 1
tr2 <- tr[26:50, ] # Fold 2

set.seed(1234567890)

winit <- runif(31,-1, 1)
set.seed(1234567890)

nn1 <-
  neuralnet(
    Sin ~ Var,
    data = tr1,
    startweights = winit,
    hidden = c(10),
    threshold = 1 / 1000
  )
nn1.pred <- predict(nn1, newdata = tr2)
mse1 <- mse(tr1$Sin, nn1.pred)
set.seed(1234567890)

nn2 <-
  neuralnet(
    Sin ~ Var,
    data = tr2,
    startweights = winit,
    hidden = c(10),
    threshold = 1 / 1000
  )
nn2.pred <- predict(nn2, newdata = tr1)
mse2 <- mse(tr2$Sin, nn2.pred)

mean(c(mse1, mse2))



