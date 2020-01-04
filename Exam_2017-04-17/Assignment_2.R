setwd("~/Programming/TDDE01/Exam_2017-04-17")
RNGversion('3.5.1')

data <- read.csv2("bank.csv")

model <-
	glm(Visitors ~ Time, data = data, family = poisson(link = log))

fit <- fitted(model)
fit
plot(fit, xlab = "Time", xaxt = "n")
axis(1, at = 1:length(data$Time), labels = data$Time)

data2 <- data.frame(Time = data$Time, Visitors = fit)
plot <-
	ggplot(aes(x = data2$data.Time, y = data2$fit), data = data2) + geom_point()
plot

#2
library(boot)

bootfunc <- function(databoot) {
	modelboot<-glm(Visitors ~ Time,
			data = databoot,
			family = poisson(link = log))
	return(fitted(modelboot))
}

bootstrapped <-
	boot(
		data = data2,
		statistic = bootfunc,
		R = 1000,
		
		sim = "parametric"
	)
