# ECON 114
# April Dawn Kester
# Winter 2014

#Question #1

#mixtures
library(moments)

#number of data observations from the mixture distribution 100, 1000, 10000, 100000
N = 100

#sample N random Uniform distributions
U = runif(N)

#vector to store observations
observ = rep(NA,N)

for(i in 1:N){
	if(U[i]<.40){
		observ[i] = rnorm(1,5,sqrt(15))
	}else{
		observ[i] = rnorm(1,5,sqrt(10))
	}
}

kurtosis(observ)
##[1] 3.322186

#vector to store new observations
observNew = rep(NA,N)

for(i in 1:N){
	if(U[i]<.40){
		observNew[i] = rnorm(1,3,sqrt(15))
	}else{
		observNew[i] = rnorm(1,5,sqrt(10))
	}
}

kurtosis(observNew)
##[1] 2.348179

#Question #2
# sample from a Poisson distribution
n = 100000
lamb = 4
x = rpois(n, lamb)
plot(density(x))

# Compute MLE of lambda and asymptotic standard error
lambHat = (1/n)*sum(x) ##mean(x) 
##[1] 3.99919
sdlambHat = sqrt(lamb/n) ##expected
##[1] 0.006324555

varCheck = lamb^2/sum(x) ##observed
sdCheck = sqrt(varCheck)
##[1] 0.006325196

# Compute 95% confidence interval for lambda 
conIntAlow = lambHat - 1.96*sdlambHat
[1] 3.986794
conIntAhigh = lambHat + 1.96*sdlambHat
[1] 4.011586

# Compute bootstrap and standard error
# Number of resamples
B = 1000

# Draw B resamples and store each sample in column of yMat
yMat = matrix(0, nrow=n, ncol=B)
for(i in 1:B) yMat[,i] = sample(x, n, replace=TRUE) #fill in i column, sample method to bootstrap

# Compute MLE for each resample (i.e. using each column of yMat)
lambHatBoot = rep(0, length=B) ##MLE for each boot strap (vector)
for(i in 1:B){
	lambHatBoot[i] = (1/n)*sum(yMat[,i])
}

mulambHatBoot = mean(lambHatBoot)
##[1] 3.999354
sdlambHatBoot = sd(lambHatBoot)
##[1] 0.006247919

# Compute 95% confidence interval for lambda ##USING mulambHatBoot
conIntDlow = mulambHatBoot - 1.96*sdlambHatBoot
conIntDhigh = mulambHatBoot + 1.96*sdlambHatBoot
conIntDlow 
##[1] 3.986655
conIntDhigh 
##[1] 4.011979


mean(conIntDlow)##<--------------------------------these were from taking lambHatBoot
##[1] 3.987108
mean(conIntDhigh)
##[1] 4.0116

conIntE = quantile(lambHatBoot, probs=c(.025, 0.975))
##    2.5%    97.5% 
##3.987980 4.011882 

##Question #3
newX = x[1:50]
newN = 50

# Compute MLE of lambda and asymptotic standard error
lambHat = (1/newN)*sum(newX) ##mean(x) 
##[1] 4.3
sdlambHat = sqrt(lamb/newN) 
##[1] 0.2828427

varCheck = lamb^2/sum(newX)
sdCheck = sqrt(varCheck)
[1] 0.2727977

# Compute 95% confidence interval for lambda 
conIntAlow = lambHat - 1.96*sdlambHat
##[1] 3.745628
conIntAhigh = lambHat + 1.96*sdlambHat
##[1] 4.854372

# Compute bootstrap and standard error
# Number of resamples
B = 1000

# Draw B resamples and store each sample in column of yMat
yMat = matrix(0, nrow=newN, ncol=B)
for(i in 1:B) yMat[,i] = sample(newX, newN, replace=TRUE) #fill in i column, sample method to bootstrap

# Compute MLE for each resample (i.e. using each column of yMat)
lambHatBoot = rep(0, length=B) ##MLE for each boot strap (vector)
for(i in 1:B){
	lambHatBoot[i] = (1/newN)*sum(yMat[,i])
}

mulambHatBoot = mean(lambHatBoot)
##[1] 4.28764
sdlambHatBoot = sd(lambHatBoot)
##[1] 0.3474973

# Compute 95% confidence interval for lambda 
conIntDlow = mulambHatBoot - 1.96*sdlambHatBoot
conIntDhigh = mulambHatBoot + 1.96*sdlambHatBoot
conIntDlow
##[1] 3.640248
conIntDhigh
##[1] 4.964672

mean(conIntDlow)##<--------------------------------for lambHatBoot
##[1] 3.606545
mean(conIntDhigh)
##[1] 4.968735

conIntE = quantile(lambHatBoot, probs=c(.025, 0.975))
 ##2.5% 97.5% 
 ##3.60  4.98 


