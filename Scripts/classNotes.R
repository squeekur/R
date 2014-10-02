# ECON 114
# April Dawn Kester
# Winter 2014

# Graph any normal dist with mu and sigma
mu = 16.45
sigma = 3

x = rnorm(1000000, mu, sigma)
x = seq(5, 30, length=1000)
y = dnorm(x, mu, sigma)

plot(x,y, type='l')

mu = 5
sig = 2
qnorm(0.7, mu, sig)
qnorm(c(0.2,0.3, 0.7, 0.9), mu, sig)

# Sim Sample
nSim = 500
dat = rnorm(nSim, mu, sig)
edf = ecdf(dat)

#true CDF
x = seq(mu-4*sig, mu+4*sig, length = 1000)
head(x)
y = pnorm(x, mu, sig)
plot(x,y)
plot(x,y, type='l')
plot(edf)
lines(x,y)

#standardize
datStand = (dat-mean(dat))/sd(dat)
edf = ecdf(datStand)
y = pnorm(x, 0.1, col='red')
qqnorm(dat)
qqline(dat)


plot(density(x))
ls()
dim(Garch)
read.csv() # Needs to be in the same directory
setwd
length(diff_bp)

d <- density(diff_twtr)
plot(d, main="Density Estimate w/Normal Curve-Mean-Stdev")
xfit <- seq(min(-0.2), max(0.2), length=100)
yfit <- dnorm(xfit, mean=mean(diff_twtr), sd=sd(diff_twtr))
lines(xfit, yfit, lwd=2, lty=2)

y = scan() #import file
w1=y^1-1
w.6=(y^0.66-1)/0.66

lm(y~w.6) #predict y with values from x

par(mfrow = c(2,3))
p=c(0.25....etc) #vector 
for(i in 1:6){
	qqnorm(diff_bp), main=paste("Normal Plot, p =", p[i])
	abline(lm(qnorm(c(p[i],1-p[i]))~quantile(diff_bp, probs=c(p[i],1-p[i])))) # draw line, linear model, 
	
	}
}
#CODE FROM 1-21
mu = 5
sig = 2
qnorm(c(0.2,0.3,0.7,0.9), mu, sig)

# Simulated sample
nSim = 500
dat = rnorm(nSim, mu, sig)

# Standardize
datStand = (dat-mean(dat))/sd(dat)
edf = ecdf(datStand)

# True CDF
x = seq(mu-4*sig, mu+4*sig, length=1000)
y = pnorm(x, 0, 1)

# Plot
plot(edf)
lines(x,y, col="red")


library(quantmod)
getSymbols("XLF",src="yahoo")

#Code form 1-23
# Number of observations
nSim = 1000

# True parameters
alpha = 2
beta = 0.3

# Create the covariate
x = rnorm(nSim, 5, 5)

# Generate a set of random shocks
eps = rnorm(nSim, 0, 0.25)

# Create the data set
y = exp(alpha + beta*x + eps)
plot(x,log(y))

# Estimate the regression
summary(lm(log(y) ~ x))

nAlpha = 20
alpha = seq(0.2,5,length=nAlpha)
sally = seq(0,1,length=100000)
plot(sally, sally^alpha[1], type='l', ylim=c(0,1))
for(i in 2:nAlpha){	
	bob = sally^alpha[i]
	lines(sally, bob, col=i)
}

#Code from 2-28
mu = 0
sigma = 1
xGrid = seq(mu-4*sigma, mu+4*sigma, length=1000000)
y = dnorm(xGrid, mu, sigma)
plot(xGrid, y, type='l')

nSim = c(10,100,1000,10000,100000,1000000)
ySim = list()
for(i in 1:length(nSim)){
ySim[[i]] = rnorm(nSim[i])
lines(density(ySim[[i]]))
}
lines(density(ySim[[6]]), col='red')