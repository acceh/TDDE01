#1.
data <- read.csv2("spambase.csv")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

#2.
model <- glm(Spam ~ ., family=binomial, data=train)

predictModel= predict(model, newdata=test, type="response")

probability <- ifelse(predictModel > 0.5, "1", "0") #Split up the model into spam and not spam

tableModel <- table(probability, test[,"Spam"])

print(tableModel)

modelDiag <- diag(tableModel)

missClMa1 = 1-(sum(modelDiag)/sum(tableModel))

cat("diag",(sum(modelDiag)))

cat("table",(sum(tableModel)))


print(modelDiag)

print(missClMa1)

#3.
probability2 <- ifelse(predictModel > 0.8, "1", "0") #Split up the model into spam and not spam

tableModel2 <- table(probability2, test[,"Spam"])

modelDiag2 <- diag(tableModel2)

missClMa2 = 1-(sum(modelDiag2)/sum(tableModel2))


print(modelDiag2)

print(missClMa2)
