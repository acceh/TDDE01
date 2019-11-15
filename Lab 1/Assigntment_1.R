library(kknn)

#1.
data <- read.csv2("spambase.csv")

#Split data into training and test set.

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

#2.

model <- glm(Spam ~ ., family=binomial, data=train) #GLM model for data with family binomial --> only 0s and 1s

predictModel= predict(model, newdata=test, type="response") 

probability <- ifelse(predictModel > 0.5, "1", "0") #Split up the model into spam and not spam

confMatrix <- table(probability, test[,"Spam"]) #Confusionmatrix from the model

modelDiag <- diag(confMatrix) #Diagonal of the 

missClMa1 = 1-(sum(modelDiag)/sum(confMatrix)) #Missclassfication rate by dividing the diagonal from the confusionmatricx with the whole confusionmatrix

#Prints results
print("Confusion matrix 2:")
print(confMatrix)
print("missclassification 2:")
print(missClMa1)

#3.
probability2 <- ifelse(predictModel > 0.8, "1", "0") #Split up the model into spam and not spam

confMatrix2 <- table(probability2, test[,"Spam"]) 

modelDiag2 <- diag(confMatrix2)

missClMa2 = 1-(sum(modelDiag2)/sum(confMatrix2))


print("Confusion matrix 3:")
print(confMatrix2)
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


confMa_K30 = table(kknn_K30_pred, test[,"Spam"])
misCl_K30 = 1-sum(diag(confMa_K30)/sum(confMa_K30))

print("missclassification 4:")
print(misCl_K30)


#5.
#KKNN with K=1
kknn_K1 = kknn(Spam ~ ., train=train, test=test, k=1)
kknn_K1_pred = numeric(length(fitted(kknn_K1)))


for (i in 1:length(fitted(kknn_K1))){
  if (fitted(kknn_K1)[i] > 0.5) {
    kknn_K1_pred[i] = 1
  }
}


confMa_K1 = table(kknn_K1_pred, test[,"Spam"])
misCl_K1 = 1-sum(diag(confMa_K1)/sum(confMa_K1))

print("missclassification 5:")
print(misCl_K1)
