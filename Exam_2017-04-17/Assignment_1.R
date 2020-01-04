setwd("~/Programming/TDDE01/Exam_2017-04-17")

data <- read.csv("australian-crabs.csv")
library(ggplot2)
#1
#Plot the dependence of CW versus BD where the points are colored by Species. 
#Are CW and BD good predictors of the Species?
plot1 <-
	ggplot(aes(
		x = data$CW,
		y = data$BD,
		color = data$sex
	), data = data) + geom_point()
plot1
#No, it is not that good of a classifier for the data

#2
#Create a Naïve Bayes classifier model with Species as target and CW and BD as predictors. 
#Present the confusion matrix and comment on the quality of the classification. 
#Based on the assumptions of the Naïve Bayes, explain why this model 
#is not appropriate for these data 
library(e1071)
model <- naiveBayes(sex ~ CW + BD, data = data)
model.pred <- predict(model, newdata = data)
confMa.naive <- table(model.pred, data$sex)
confMa.naive
missCla.naive <- 1 - (sum(diag(confMa.naive)) / sum(confMa.naive))
missCla.naive
#This model is not good for the data as it uses the


#3
#Fit the logistic regression now with Species as target and CW and BD as predictors 
#and present the equation of the decision boundary. Plot the classified data and the 
#decision boundary and comment on the quality of the classification
model.logistic <- glm(species ~ CW + BD, data = data, family = binomial)
model.logistic.pred <- predict(model.logistic, type = "response")

model.logistic.pred.classified <-
	ifelse(model.logistic.pred > 0.5, "orange", "blue")

plot(model.logistic.pred)

plot2 <-
	ggplot(aes(x = (seq(1, 200, 1)), y = model.logistic.pred, color = data$species),
				 data = data) + geom_point() + geom_hline(yintercept = 0.5)
plot2

#The classification is quite good! There are only a few missclassified samples.


#4
#Scale variables CW and BD and perform principal component analysis with these two variables. 
#Present the proportion of variation explained by PC1 and PC2 and based on results from step 1
#explain why the first principal component contains so much variation. Present the equations
#expressing principal component coordinates through the original coordinates.
res = prcomp(~CW+BD, data = data, scale = TRUE)

pcdata = data.frame(species = data$species, PC1 = res$x[,1], PC2 = res$x[,2])

#5
#Create a Naïve Bayes classifier model with Species as target and PC1 and PC2 as predictors.
#Compute the confusion matrix and explain how much the classification quality has changed and why.
model5 <- naiveBayes(species ~ PC1+PC2, data=pcdata)
model5.pred <- predict(model5, newdata=pcdata)
ConfMa <- table(model5.pred, data$species)
missCl <- 1-sum(diag(ConfMa))/sum(ConfMa)



