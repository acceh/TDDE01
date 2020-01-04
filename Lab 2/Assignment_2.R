setwd("~/Programming/TDDE01/Lab 2")
library(tree)
library(readxl)
library(MASS)
library(e1071)

data <- read_excel("creditscoring.xls")
set.seed(12345)

#Splits data into training, validation and test
n=dim(data)[1]
id=sample(1:n, floor(n*0.5))
train=data[id,]
id1=setdiff(1:n, id)
set.seed(12345)
id2=sample(id1, floor(n*0.25))
valid=data[id2,]
id3=setdiff(id1,id2)
test=data[id3,]


#Train and test for deviance
fit.train.dev <- tree(ifelse(good_bad == "good", 1, 0) ~ . , data=train, split="deviance")

#Train and test for gini
fit.train.gini <- tree(ifelse(good_bad == "good", 1, 0)  ~ . , data=train, split="gini")

#Predictions
fit_pred.train.dev <- predict(fit.train.dev, newdata=train)

fit_pred.train.gini <- predict(fit.train.gini, newdata=train)

fit_pred.test.dev <- predict(fit.train.dev, newdata=test)

fit_pred.test.gini <- predict(fit.train.gini, newdata=test)

#Conf matrix
confMa.train.dev <- table(train$good_bad,(ifelse(fit_pred.train.dev > 0.5, "good", "bad")))
confMa.test.dev <- table(test$good_bad,(ifelse(fit_pred.test.dev > 0.5, "good", "bad")))
confMa.train.gini <- table(train$good_bad,(ifelse(fit_pred.train.gini > 0.5, "good", "bad")))
confMa.test.gini <- table(test$good_bad,(ifelse(fit_pred.test.gini > 0.5, "good", "bad")))

#Misclassification rates
misCl.train.dev <- 1-(sum(diag(confMa.train.dev))/sum(confMa.train.dev))
misCl.test.dev <- 1-(sum(diag(confMa.test.dev))/sum(confMa.test.dev))
misCl.train.gini <- 1-(sum(diag(confMa.train.gini))/sum(confMa.train.gini))
misCl.test.gini <- 1-(sum(diag(confMa.test.gini))/sum(confMa.test.gini))

#Prints the miscl. rates
misCl.train.dev
misCl.test.dev
misCl.train.gini
misCl.test.gini

#I choose dev as it has the best miscl-rate

#3 
trainScore=rep(0,9)
validationScore=rep(0,9)

for (i in 2:9) {
  prunedTree <- prune.tree(fit.train.dev, best=i)
  prediction <- predict(prunedTree, newdata = valid, type = "tree") # Predict with the pruned tree and val set
  trainScore[i] <- deviance(prunedTree) # Calculate deviance of test set
  validationScore[i] <- deviance(prediction) # Calculate deviance of val set
}

plot(2:9, trainScore[2:9], type="b", col="green", ylim=c(40,100), ylab="Deviance", xlab="No. of leaves")
points(2:9, validationScore[2:9], type="b", col="blue", ylim=c(40,100))
legend("top", legend=c("Training score (green)", "Validation score (blue)"))

#Optimal number of leaves
match(min(validationScore[2:9]),validationScore)
optPrunedTree <- prune.tree(fit.train.dev, best=match(min(validationScore[2:9]),validationScore))
plot(optPrunedTree, sub="asd")
text(optPrunedTree, pretty=0)
title("Pruned tree with 6 leaves")

#Estimates classification 
predict.tree <- predict(optPrunedTree, newdata = test)
confMa.prune <- table(ifelse(predict.tree>0.5, "good", "bad"), test$good_bad)
testMisClass <- 1-sum(diag(confMa.prune)/sum(confMa.prune))
print(testMisClass)


#4
####NAIVE BAYES#######
#List the main advantage of Navie Bayes?
#A Naive Bayes classifier converges very quickly as compared to other models like logistic regression. 
#As a result, we need less training data in case of naive Bayes classifier .
set.seed(12345)


naive.model = naiveBayes(good_bad ~ ., data=train)
naive.model$levels<-c('bad', 'good')

#Sets training data for naive
set.seed(12345)
naive.train = predict(naive.model, newdata=train)
confMa.naive.train = table(naive.train, train$good_bad)
misCl.naive.train = 1-(sum(diag(confMa.naive.train))/sum(confMa.naive.train))
print(confMa.naive.train)
print(misCl.naive.train)

#Sets test for naive
set.seed(12345)
naive.test = predict(naive.model, newdata=test)
confMa.naive.test = table(naive.test, test$good_bad)
misCl.naive.test = 1-(sum(diag(confMa.naive.test))/sum(confMa.naive.test))
print(confMa.naive.test)
print(misCl.naive.test)


#5
pi = seq(0.05, 0.95, by=0.05)
tree.model.dataframe = data.frame("0.05" = c(rep(0,250)))
tree.model.tpr = c(rep(0,length(pi)))
tree.model.fpr = c(rep(0,length(pi)))

for(k in 1:length(pi)) {
  tree.model.dataframe[, k] = ifelse(predict.tree > pi[k], 1, 0)
  confMa.tmp = table(ifelse(test$good_bad == 'good', 1, 0), tree.model.dataframe[, k])
  print(confMa.tmp)
  
  
  
  if (dim(confMa.tmp)[2] == 1) {
    if (colnames(confMa.tmp) == "1") {
      confMa.tmp <- cbind(as.matrix(c(0, 0)), confMa.tmp)
    } else {
      confMa.tmp <- cbind(confMa.tmp, as.matrix(c(0, 0)))
    }
  }
  
  tree.model.tpr[k] = confMa.tmp[2, 2] / (confMa.tmp[2, 1] + confMa.tmp[2, 2])
  tree.model.fpr[k] = confMa.tmp[1, 2] / (confMa.tmp[1, 1] + confMa.tmp[1, 2])
  
}
plot(x = tree.model.fpr, y = tree.model.tpr, type = "l", col="green", main="Green = tree, Blue = naive")


set.seed(12345)
#ROC for naive
naive.test.raw = predict(naive.model, newdata = test, type="raw")

naive.model.dataframe = data.frame("0.05" = c(rep(0,250)))
naive.model.tpr = c(rep(0,length(pi)))
naive.model.fpr = c(rep(0,length(pi)))


for (k in 1:length(pi)) {
  naive.model.dataframe[, k] = ifelse(naive.test.raw[, 2] > pi[k], 1, 0)
  confMa.tmp = table(ifelse(test$good_bad == 'good', 1, 0), naive.model.dataframe[, k])
  
  if (dim(confMa.tmp)[2] == 1) {
    if (colnames(confMa.tmp) == "1") {
      confMa.tmp <- cbind(as.matrix(c(0, 0)), confMa.tmp)
    } else {
      confMa.tmp <- cbind(confMa.tmp, as.matrix(c(0, 0)))
    }
  }
  
  naive.model.tpr[k] = confMa.tmp[2, 2] / (confMa.tmp[2, 1] + confMa.tmp[2, 2])
  naive.model.fpr[k] = confMa.tmp[1, 2] / (confMa.tmp[1, 1] + confMa.tmp[1, 2])
  
}
lines(x = naive.model.fpr, y = naive.model.tpr, col="blue")

#6 
naive.train.raw = predict(naive.model, newdata = train, type="raw")


#1*p(bad|x) > 10*p(good|x) -> bad, L12=1, L21=10, L12/L21=10
confMa.train.naive.loss = table(train$good_bad, ifelse(naive.train.raw[,1]/naive.train.raw[,2] > 0.1, "bad", "good"))
print(confMa.train.naive.loss)
misCl.train.naive.loss <- 1-(sum(diag(confMa.train.naive.loss))/sum(confMa.train.naive.loss))
print(misCl.train.naive.loss)

confMa.test.naive.loss = table(test$good_bad, ifelse(naive.test.raw[,1]/naive.test.raw[,2] > 0.1, "bad", "good"))
  

print(confMa.test.naive.loss)
misCl.test.naive.loss <- 1-(sum(diag(confMa.test.naive.loss))/sum(confMa.test.naive.loss))
print(misCl.test.naive.loss)



