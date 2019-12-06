library(readxl)
setwd("~/Programming/TDDE01/Lab 1")


set.seed(12345)

#1
#imports file
machines <- read_excel("machines.xlsx")

#2
#Assume the probability model p(x|theta)=theta*exp(-theta*x) for x=Length in which
#observations are independent and identically distributed. What is the distribution type of x? Write a function that 
#computes the log-likelihood log(p(x|𝜃)) for a gven 𝜃 and a given data vector x.
#Plot the curve showing the dependence of log-likelihood on 𝜃 where the entire data is used for fitting.
#What is the maximum likelihood value of 𝜃𝜃 according to the plot?

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
  return (dim(x)[1]/sum(x))
}
print(minthetalikelihood(machines))


#3
#Repeat step 2 but use only 6 first observations from the data, 
#and put the two log-likelihood curves (from step 2 and 3) in the same plot. 
#What can you say about reliability of the maximum likelihood solution in each case?

curve(likelihoodlog(machines[1:6,],x), from=0, to=4, col="red", add = TRUE)
print((dim(machines[1:6,])[1])/sum(machines[1:6,]))

#4
#Assume now a Bayesian model with p(x|theta)=theta*exp(-theta*x) and a prior theta=lambda*exp(-labda*theta), lambda=10.
#Write a function computing l(theta)=log(p(x|theta)p(theta)). What kind of measure is actually computed by this function? 
#Plot the curve showing the dependence of l(theta) on theta computed using the entire data and overlay it with a plot 
#from step 2. Find an optimal theta and compare your result with the previous findings.

bayesianfunc = function(x, θ, λ) {
  retur#Osäker om +1 eller ej!
  n(likelihoodlog(x, θ) - log(λ) + λ*θ)
} 

print("Bayesianfunction:")
print(bayesianfunc(machines,1,10))

curve(bayesianfunc(machines,x,10), xlab="θ", ylab="l(θ)", from=0, to=4, col="green", add=TRUE)

#min theta for bayesianfunc. I get it from deriving the bayesianfunc with respect to theta and set it to = 0 to get the min
print("Min θ bayesian func.:")
print(dim(machines)[1]/(sum(machines)+10))


#5
#Use theta value found in step 2 and generate 50 new observations from p(x|theta)=theta*exp(-theta*x)
#(use standard random number generators). Create the histograms of the original and the new data and make conclusions.

theta = minthetalikelihood(machines)

##Creaes 5 new data points with the rate from 2.
newdata = rexp(50, rate=theta)
print(newdata)
#Stores the old data from the Length col in the machines
olddata <- machines$Length

#Creages new window for plots
hist(olddata, col="red", xlim=c(0,5), ylim=c(0,20), xlab="x")
hist(newdata, col="blue", xlim=c(0,5), ylim=c(0,20), add=TRUE, breaks="FD")

