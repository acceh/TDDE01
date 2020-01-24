setwd("~/Programming/TDDE01/Exam_2019-01-16")
RNGversion('3.5.1')

data <- read.csv("influenza.csv")



#2
library(glmnet)
data.scaled <- data.frame(scale(data[-3])) 
data.scaled$Mortality <- data$Mortality

n=dim(data.scaled)[1] 
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data.scaled[id,] 
test=data.scaled[-id,]

model <- cv.glmnet(as.matrix(train[-9]), train$Mortality, alpha=1, family="poisson")
plot(model)
print(model$lambda.min)

mse(model$glmnet.fit, train$Mortality)
predict.model <- predict(model, s=model$lambda.min, newx=as.matrix(test[-9]))
mse.model <- mse(predict.model, test$Mortality)

library(tree)

model.tree <- tree(Mortality ~ ., data=data)
model.tree.cv <- cv.tree(model.tree, FUN=prune.tree, )
plot(model.tree.cv)

model.tree.pruned <- prune.tree(model.tree, best = 10)
#Plots the pruned tree with 3 leaves
plot(model.tree.pruned) 
text(model.tree.pruned)


model.tree.mse = mse(test$Mortality,predict(model.tree, test, type="vector"))

#MSE is not as high as with the LASSO regression



#4

res <- prcomp(~ . - Mortality, data=data)

lambda = res$sdev^2
sprintf("%2.3f", (lambda)/sum(lambda)*100)
screeplot(res)
plot(res)
#2 components
plot(res$x[,1], res$x[,2], main = "PC1 vs. PC2", xlab = "PC1", ylab = "PC2")

pcdata = data.frame(Mortality = data$Mortality, PC1 = res$x[,1], PC2 = res$x[,2])


model.pc.glmnet <- cv.glmnet(as.matrix(pcdata[-1]), pcdata$Mortality, alpha=1, family="poisson", lambda=seq(0,50,0.1))


plot(model.pc.glmnet)


