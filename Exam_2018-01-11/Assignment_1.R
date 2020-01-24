setwd("~/Programming/TDDE01/Exam_2018-01-11")
library(Metrics)
library(MASS)
data <- read.csv("video.csv")
RNGversion('3.5.1')

n = dim(data)[1]
set.seed(12345)
id = sample(1:n, floor(n * 0.5))
train = data[id, ]
test = data[-id, ]

comp.model <- prcomp( ~ . - codec - utime, data = train)
comp.model.scaled <-
	prcomp( ~ . - codec - utime, data = train, scale = TRUE)

sprintf("%2.3f", (lambda) / sum(lambda) * 100)
sprintf("%2.3f", (lambda) / sum(lambda) * 100)

lambda = comp.model$sdev ^ 2
lambda.scaled = comp.model.scaled$sdev ^ 2

variation <- (lambda) / sum(lambda) * 100
variation.scaled <- (lambda.scaled) / sum(lambda.scaled) * 100

i <- 0
tot <- 0
while (tot <= 95) {
	i = i + 1
	tot <- tot + variation.scaled[i]
	
}
cat("The scaled variation has",
		i,
		"components to reach",
		tot,
		"% of the total variation.")

sprintf("%2.3f", variation)
sprintf("%2.3f", variation.scaled)

# The reason for only needing one component compared to the 9 needed for the scaled component is that
# when scaling all the numerical value the amount of relative variation is a lot smaller than when
# it is not scaled. So one value that has a lot of variation is not normalized and that makes

#2
library(pcr)
library(pls)
library(ggplot2)
mse.train <- c()
mse.test <- c()

for (i in 1:17) {
	pcr.model <-
		pcr(
			utime ~ . - codec,
			data = train,
			scale = TRUE,
			ncomp = i,
			validation = "none"
		)
	pcr.model.predicted <- predict(pcr.model, ncomp = i)
	pcr.model.predicted.test <-
		predict(pcr.model, ncomp = i, newdata = test)
	mse.train[i] <- mse(train$utime, pcr.model.predicted)
	mse.test[i] <- mse(test$utime, pcr.model.predicted.test)
}
plottrain <-
	ggplot() + geom_line(aes(x = 1:17, y = mse.train, color = "Train")) + geom_line(aes(x =
																																												1:17, y = mse.test, color = "Test")) + scale_color_manual(name = '',
																																																																									values = c('Train' = 'red', 'Test' = 'purple'))
plottrain

#The bias variance trade-off of the plots shows that there is a bit of overfitting with the trainign data as the mse is
#al

#3

pcr.model.8 <- pcr(
	utime ~ . - codec,
	data = train,
	scale = TRUE,
	ncomp = 8,
	validation = "none"
)
mean(residuals(pcr.model.8)^2)
Yloadings(pcr.model.8)


#4
data4 <- data
data4$class <- ifelse(data4$codec=='mpeg4', 'mpeg', 'other')

plotclass <-
	ggplot(data4, aes(x = duration, y = frames, color = data4$class)) + geom_point()
plotclass

#The plot shows that it can be done, but it is not perfect for LDA as there are a few otuliers

#5
data4.scaled <- data.frame(scale(data4[,-c(2, 20)]))
data4.scaled$codec <- data4$codec
data4.scaled$class <- data4$class
lda.class <- lda(class ~ duration + frames, data = data4.scaled)

lda.class.predicted <- predict(lda.class)

plotclass.lda <- ggplot(data4.scaled,aes(x = duration, y = frames, color = lda.class.predicted$class)) + geom_point()
plotclass.lda

confMa.lda <- table(lda.class.predicted$class, data4.scaled$class)
confMa.lda
missCl.lda <- 1-sum(diag(confMa.lda))/sum(confMa.lda)
missCl.lda

# Lda wasn''t able to achieve a perfect classification as it

#4
library(tree)
data4$codec <- c()

tree.model <- tree(as.factor(class) ~ duration + frames, data = data4)

model.tree.cv <- cv.tree(tree.model, FUN = prune.tree)

plot_cv_tree <-
	ggplot() + geom_line(aes(x = model.tree.cv$size, y = model.tree.cv$dev), color="green") + labs(x = "Tree")
plot_cv_tree
model.tree.pruned <- prune.tree(tree.model, best = 11)

plot(model.tree.pruned) 
text(model.tree.pruned)

tree.model.predicted <- predict(tree.model,  type="class")

confMa.tree <- table(tree.model.predicted, data4$class)
confMa.tree
missCl.tree <- 1-sum(diag(confMa.tree))/sum(confMa.tree)
missCl.tree

