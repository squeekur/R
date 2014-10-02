# ECON 114
# April Dawn Kester
# Winter 2014

###########################################################################
# Sample from a Pareto with inverse probability transform
###########################################################################

# True parameters
alpha = 2
beta = 5

# Number of draws
n = 30#100000

# Draw a bunch of uniforms
U = runif(n)

# Apply to inverse of the Pareto 
y = alpha/((1-U)^(1/beta))

# Plot density estimate of data (this is clearly not normal!)
#plot(density(y), xlim=c(0,5))

###########################################################################
# Compute Likelihood of beta, given alpha and data
###########################################################################

betaGrid = seq(0.1, 10, length=10000)
logLike = n*log(betaGrid) + n*betaGrid*log(alpha) - (1+betaGrid)*sum(log(y))
#plot(betaGrid, logLike, type='l')
#add line

###########################################################################
# Compute MLE of beta and asymptotic standard error
###########################################################################

betaHat = n/(sum(log(y)) - n*(log(alpha))) #calculate beta hats from equation 
sdBetaHat = betaHat/sqrt(n) #use for confidence interval

# Add a vertical line at the MLE to the log like plot
#abline(v=betaHat)

###########################################################################
# Compute bootstrap standard error
###########################################################################

# Number of resamples
B = 1000

# Draw B resamples and store each sample in column of yMat
yMat = matrix(0, nrow=n, ncol=B)
for(i in 1:B) yMat[,i] = sample(y, n, replace=TRUE) #fill in i column, sample method to bootstra

# Compute MLE for each resample (i.e. using each column of yMat)
betaHatBoot = rep(0, length=B) ##MLE for each boot strap (vector)
for(i in 1:B){
	betaHatBoot[i] = n/(sum(log(yMat[,i])) - n*(log(alpha))) #change for distribution
}
muBetaHatBoot = mean(betaHatBoot)
sdBetaHatBoot = sd(betaHatBoot)

quantile(betaHatBoot, probs=c(.025, 0.975))

