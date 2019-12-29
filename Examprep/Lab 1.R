# Lab 1
RNGversion('3.5.1')

##Assigntment 1
library(readxl)

data <- read_xlsx("spambase.xlsx")


n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.5))
train = data[id,]
test = data[-id,]

##Assignment 2
glm.model <- glm(Spam ~ ., family = binomial, data = train)

glm.pred.train <-
  predict(glm.model, newdata = train, type = "response")
glm.pred.test <-
  predict(glm.model, newdata = test, type = "response")

glm.categorized.train <- ifelse(glm.pred.train > 0.5, 1, 0)
glm.categorized.test <- ifelse(glm.pred.test > 0.5, 1, 0)

glm.confMa.train <- table(train$Spam, glm.categorized.train)
glm.confMa.test <- table(test$Spam, glm.categorized.test)
glm.confMa.train
glm.confMa.test

missCla <- function(confMa) {
  return(1 - (sum(diag(confMa)) / sum(confMa)))
}

glm.missCla.train <- missCla(glm.confMa.train)
glm.missCla.test <- missCla(glm.confMa.test)
glm.missCla.train
glm.missCla.test

##Assignment 3
### adj for adjusted to 0.8
glm.categorized.train.adj <- ifelse(glm.pred.train > 0.8, 1, 0)
glm.categorized.test.adj <- ifelse(glm.pred.test > 0.8, 1, 0)

glm.confMa.train.adj <- table(train$Spam, glm.categorized.train.adj)
glm.confMa.test.adj <- table(test$Spam, glm.categorized.test.adj)
glm.confMa.train.adj
glm.confMa.test.adj

glm.missCla.train.adj <- missCla(glm.confMa.train.adj)
glm.missCla.test.adj <- missCla(glm.confMa.test.adj)
glm.missCla.train.adj
glm.missCla.test.adj

##Assignment 4
library(kknn)

kknn.model.train <- kknn(Spam ~ .,
                         train = train,
                         test = test,
                         k = 30)
kknn.model.test <- kknn(Spam ~ .,
                        train = test,
                        test = test,
                        k = 30)

kknn.pred.train <- predict(kknn.model.train)
kknn.pred.test <- predict(kknn.model.test)

kknn.categorized.train <- ifelse(kknn.pred.train > 0.5, 1, 0)
kknn.categorized.test <- ifelse(kknn.pred.test > 0.5, 1, 0)


kknn.confMa.train <- table(train$Spam, kknn.categorized.train)
kknn.confMa.test <- table(test$Spam, kknn.categorized.test)
kknn.confMa.train
kknn.confMa.test

kknn.missCla.train <- missCla(kknn.confMa.train)
kknn.missCla.test <- missCla(kknn.confMa.test)
kknn.missCla.train
kknn.missCla.test

##Assignment 5

kknn.model.train.k1 <- kknn(Spam ~ .,
                            train = train,
                            test = test,
                            k = 1)
kknn.model.test.k1 <- kknn(Spam ~ .,
                           train = test,
                           test = test,
                           k = 1)

kknn.pred.train.k1 <- predict(kknn.model.train.k1)
kknn.pred.test.k1 <- predict(kknn.model.test.k1)

kknn.categorized.train.k1 <- ifelse(kknn.pred.train.k1 > 0.5, 1, 0)
kknn.categorized.test.k1 <- ifelse(kknn.pred.test.k1 > 0.5, 1, 0)


kknn.confMa.train.k1 <- table(train$Spam, kknn.categorized.train.k1)
kknn.confMa.test.k1 <- table(test$Spam, kknn.categorized.test.k1)
kknn.confMa.train.k1
kknn.confMa.test.k1

kknn.missCla.train.k1 <- missCla(kknn.confMa.train.k1)
kknn.missCla.test.k1 <- missCla(kknn.confMa.test.k1)
kknn.missCla.train.k1
kknn.missCla.test.k1

# Assignment 2

data2 <- read_xlsx("machines.xlsx")
library("datasets")

# Assignment 3
data3 <- swiss

linnear_model <- function(X, Y, Xpred) {
  X1 <- cbind(1, X)
  Xpred1 <- cbind(1, Xpred)
  beta <- solve(t(X1) %*% X1) %*% (t(X1) %*% Y)
  Res    <- Xpred1 %*% beta
  return(Res)
}

myCV = function(X , Y, Nfolds) {
  n <- length(Y)
  p <- ncol(X)
  set.seed(12345)
  ind      <- sample(n, n)
  X1       <- X[ind,]
  Y1       <- Y[ind]
  sF       <- floor(n / Nfolds)
  MSE      <- numeric(2 ^ p - 1)
  Nfeat    <- numeric(2 ^ p - 1)
  Features <- list()
  curr     <- 0# We assume 5 features.
  for (f1in0:1)
    for (f2in0:1)
      for (f3in0:1)
        for (f4in0:1)
          for (f5in0:1) {
            model <- c(f1, f2, f3, f4, f5)
            if (sum(model) == 0)
              next()
            SSE <- 0
            for (kin1:Nfolds) {
              # Compute which indices should belong to current fold
              index   <- (((k - 1) * sF) + 1):(k * sF)
              # Implement cross-validation for model with features in "model"
              # and iteration k.
              test_x  <- X1[index, which(model == 1)]
              train_x <- X1[-index, which(model == 1)]
              Yp      <- Y1[index]
              train_y <- Y1[-index]
              # Get the predicted values for fold'k', Ypred, and the original# values for fold'k', Yp.
              Ypred <- mylin(train_x, train_y, test_x)
              SSE   <- SSE + sum((Ypred - Yp) ^ 2)
            }
            curr             <- curr + 1
            MSE[curr]        <- SSE / n
            Nfeat[curr]      <- sum(model)
            Features[[curr]] <- model
          }
  # Plot MSE against number of features
  plot(Nfeat, MSE)
  # Best fit model
  i <- which.min(MSE)
  return(list(CV = MSE[i], Features = Features[[i]]))
}



