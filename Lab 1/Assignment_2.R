library(readxl)

set.seed(12345)

#1
#imports file
machines <- read_excel("machines.xlsx")

str(machines)
head(machines)

#2
#Comes from  p(x|θ) = θ*exp(-θx) and then using thew likelihood function with it and then minimizing that with -log(L(θ))=-log((θ^n*exp(-θ*sum(x)). 
likelihoodlog = function(θ,x) {
  return(-dim(x)[1]*log(θ) + θ*sum(x))
}
print(likelihoodlog(1,machines))

#Plot curve
curve(likelihoodlog(x,machines),xlim=c(0,4), ylim=c(0,60) , col="blue")


#Min theta. Comes from deriving the loglikelihood-function. The more data I have the more exactly I can pinpoint the exact point of failure
minvalue = dim(machines)/sum(machines)
print("Min θ:")
print((dim(machines)[1])/sum(machines))


#3
curve(likelihoodlog(x,machines[1:6,]), from=0, to=4, col="red", add = TRUE)
print((dim(machines[1:6,])[1])/sum(machines[1:6,]))
