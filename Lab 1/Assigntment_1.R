library(kknn)

#1.
data <- read.csv2("spambase.csv")

n=dim(data)[1]
set.seed(45)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

#2.
model <- glm(Spam ~ ., family=binomial, data=train)

predictModel= predict(model, newdata=test, type="response")

probability <- ifelse(predictModel > 0.5, "1", "0") #Split up the model into spam and not spam

tableModel <- table(probability, test[,"Spam"])

modelDiag <- diag(tableModel)

missClMa1 = 1-(sum(modelDiag)/sum(tableModel))


print("Confusion matrix 2:")
print(tableModel)
print("missclassification 2:")
print(missClMa1)

#3.
probability2 <- ifelse(predictModel > 0.8, "1", "0") #Split up the model into spam and not spam

tableModel2 <- table(probability2, test[,"Spam"])

modelDiag2 <- diag(tableModel2)

missClMa2 = 1-(sum(modelDiag2)/sum(tableModel2))


print("Confusion matrix 3:")
print(tableModel2)
print("missclassification 3:")
print(missClMa2)

#4.

#KKNN with K=30

kknn_K30 = kknn(Spam ~ ., train=train, test=test, k=30)
kknn_K30_pred = numeric(length(predict(kknn_K30)))


for (i in 1:length(fitted(kknn_K30))){
  if (fitted(kknn_K30)[i] > 0.5) {
    kknn_K30_pred[i] = 1
  }
}


confusionMatrix_K30_kknn = table(kknn_K30_pred, test[,"Spam"])
misclassification_rate_K30_kknn = 1-sum(diag(confusionMatrix_K30_kknn)/sum(confusionMatrix_K30_kknn))

print("missclassification 4:")
print(misclassification_rate_K30_kknn)


#5.
#KKNN with K=1
kknn_K1 = kknn(Spam ~ ., train=train, test=test, k=1)
kknn_K1_pred = numeric(length(predict(kknn_K1)))


for (i in 1:length(fitted(kknn_K1))){
  if (fitted(kknn_K1)[i] > 0.5) {
    kknn_K1_pred[i] = 1
  }
}


confusionMatrix_K1_kknn = table(kknn_K1_pred, test[,"Spam"])
misclassification_rate_K1_kknn = 1-sum(diag(confusionMatrix_K1_kknn)/sum(confusionMatrix_K1_kknn))

print("missclassification 5:")
print(misclassification_rate_K1_kknn)
