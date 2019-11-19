library(readxl)

set.seed(12345)

#1
#imports file
machines <- read_excel("machines.xlsx")

str(machines)
head(machines)

#2
#Comes from  p(x|θ) = θ*exp(-θx) and then using thew likelihood function with it and then minimizing that with -log(L(θ))=-log((θ^n*exp(-θ*sum(x)). 
likelihoodlog = function(x, θ) {
  return(-dim(x)[1]*log(θ) + θ*sum(x))
}
print(likelihoodlog(machines[1], 1))

#Plot curve
curve(likelihoodlog(machines, x),xlim=c(0,4), ylim=c(0,60) , col="blue")

#Min theta. Comes from deriving the loglikelihood-function. The more data I have the more exactly I can pinpoint the exact point of failure
minvalue = dim(machines)/sum(machines)
print("Min θ:")
minthetalikelihood = function(x) {
  return (dim(x)[1])/sum(x)
}
print(minthetalikelihood(machines))


#3
curve(likelihoodlog(machines[1:6,],x), from=0, to=4, col="red", add = TRUE)
print((dim(machines[1:6,])[1])/sum(machines[1:6,]))

#4
#TODO NEDAN ÄR FEL! KÖR ENDAST LIKELIHOOD PÅ BETINGADE FUNKTIONEN
bayesianfunc = function(x, θ, λ) {
  #Osäker om +1 eller ej!
  return(likelihoodlog(x, θ) - log(λ) + λ*θ)
} 

print("Bayesianfunction:")
print(bayesianfunc(machines,1,10))

curve(bayesianfunc(machines,x,10), xlab="θ", ylab="l(θ)", from=0, to=4, col="green", add=TRUE)

#min theta for bayesianfunc
print("Min θ bayesian func.:")
print(dim(machines)[1]/(sum(machines)+10))


#5
θ = minthetalikelihood(machines)

newdata = rexp(50, rate=θ)
print(newdata)
olddata <- machines$Length

dev.new()
hist(olddata, col="red", xlim=c(0,5), xlab="x")
hist(newdata, col="blue", xlim=c(0,5), add=TRUE, xlab="x")

