# ECON 114
# April Dawn Kester
# Winter 2014

#install package
install.packages("moments")
library(moments)

#import data
library(quantmod)
getSymbols("MJNA", src="yahoo", from="2013-02-04", to="2014-02-03")

MJNA
myData <- diff(MJNA$MJNA.Adjusted)
myData = myData[-1, ]
myData
sigma = sd(myData)
mu = mean(myData)
n = length(myData)

summary(myData)
     Index            MJNA.Adjusted       
 Min.   :2013-02-04   Min.   :-0.1200000  
 1st Qu.:2013-05-05   1st Qu.:-0.0100000  
 Median :2013-08-03   Median : 0.0000000  
 Mean   :2013-08-03   Mean   :-0.0003586  
 3rd Qu.:2013-10-31   3rd Qu.: 0.0000000  
 Max.   :2014-02-03   Max.   : 0.0900000  
                      NA's   :1   
 
 #standard plot
 par(bg="white")
 plot(myData, main="Medical Marijuana Inc.")
 
#compute moments using package 
skewness(myData)
	MJNA.Adjusted 
     -0.3906613  #negative skewness indicates a relatively long left tail
kurtosis(myData)
	MJNA.Adjusted 
     10.8718 #excess kurtosis is positive, the distribution has ligher than normal tails
     
#compute moments using equation
sk = (sum(((myData-mu)/sigma)^3))/n
sk
[1] -0.388329
ku = (sum(((myData-mu)/sigma)^4))/n
ku
[1] 10.78534 # Difference due to error estimation

#kernals standard/robust
par(mfrow=c(1,2), bg="white")
observ = 1000000
normalDens = rnorm(observ, mean(myData), sd(myData))
plot(density(myData), xlim=c(-0.13,0.10), main="Standard Estimates")
lines(density(normalDens), lty=2)

normalDens = rnorm(observ, median(myData), mad(myData))
plot(density(myData), xlim=c(-0.13,0.10), main="Robust Estimates")
lines(density(normalDens), lty=2)

#qq plot 
qqnorm(myData, datax=TRUE) # datax = TRUE behaving badly!
qqline(myData)

#mixtures - 
par(mfrow=c(2,2))
#number of data observations from the mixture distribution 100, 1000, 10000, 100000
N = 100

#sample N random Uniform distributions
U = runif(N)

#vector to store observations
observ = rep(NA,N)

for(i in 1:N){
	if(U[i]<.03){
		observ[i] = rnorm(1,2,11)
	}else if (U[i]<.13){
		observ[i] = rnorm(1,5,7.2)
	}else{
		observ[i] = rnorm(1,0,1)
	}
}

normalDens = rnorm(N, mean(observ), sd(observ))
plot(density(observ), main="Mixture")
lines(density(normalDens), lty=2)
#kurtosis
kurtosis(observ)
[1] 11.94508
[1] 16.5434
[1] 18.92898
[1] 20.31306

x = seq(-20,20,.1)
realDens = .03*dnorm(x,2,11) + .10*dnorm(x,5,7.2) + .87*dnorm(x,0,1)
lines(x,realDens,col="red", lwd=2)
kurtosis(realDens)
[1] 14.16375






     
     