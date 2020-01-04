setwd("~/Programming/TDDE01/Lab 3")
RNGversion('3.5.1')
library("kernlab")

data(spam)
data <- spam


n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.7))
train = data[id, ]
test = data[-id, ]

width <- 0.05

filter_0_5 <-
  ksvm(type ~ .,
       data = train,
       kernel = rbfdot(sigma = width),
       C = 0.5)
filter_3 <-
  ksvm(type ~ .,
       data = train,
       kernel = rbfdot(sigma = width),
       C = 3)
filter_5 <-
  ksvm(type ~ .,
       data = train,
       kernel = rbfdot(sigma = width),
       C = 5)
missCl <- function(data) {
  return(1 - sum(diag(data)) / sum(data))
}

filter_0_5.pred.train <- predict(filter_0_5, newdata = train)
filter_0_5.pred.test <- predict(filter_0_5, newdata = test)
filter_0_5.confMa.train <-
  table(train$type  , filter_0_5.pred.train)
print("Confma for C=0.5 with training data: ")
filter_0_5.confMa.train
filter_0_5.confMa.test <- table(test$type  , filter_0_5.pred.test)
print("Confma for C=0.5 with training data: ")
filter_0_5.confMa.test



filter_0_5.missCl.train <- missCl(filter_0_5.confMa.train)
print("Misscl for C=3 with training data: ")
filter_0_5.missCl.train
filter_0_5.missCl.test <- missCl(filter_0_5.confMa.test)
print("Misscl for C=3 with testing data: ")
filter_0_5.missCl.test


filter_3.pred.train <- predict(filter_3, newdata = train)
filter_3.pred.test <- predict(filter_3, newdata = test)
filter_3.confMa.train <- table(train$type  , filter_3.pred.train)
print("Confma for C=3 with training data: ")
filter_3.confMa.train
filter_3.confMa.test <- table(test$type  , filter_3.pred.test)
print("Confma for C=3 with training data: ")
filter_3.confMa.test



filter_3.missCl.train <- missCl(filter_3.confMa.train)
print("Misscl for C=3 with training data: ")
filter_3.missCl.train
filter_3.missCl.test <- missCl(filter_3.confMa.test)
print("Misscl for C=3 with testing data: ")
filter_3.missCl.test


filter_5.pred.train <- predict(filter_5, newdata = train)
filter_5.pred.test <- predict(filter_5, newdata = test)
filter_5.confMa.train <- table(train$type  , filter_5.pred.train)
print("Confma for C=5 with training data: ")
filter_5.confMa.train
filter_5.confMa.test <- table(test$type  , filter_5.pred.test)
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
filter_3.missCl.train
filter_5.missCl.train

print("Summary: ")
filter_0_5.missCl.test
filter_3.missCl.test
filter_5.missCl.test

#Best is C=5.
filter_5
#Above prints the model