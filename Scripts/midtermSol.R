# ECON 114
# April Dawn Kester
# Winter 2014

dat = read.table("midtermDat.txt")$V1
returns = diff(log(dat))

#############################################################################
# Full sample
#############################################################################

# Time series plot
plot.ts(returns)
abline(h=0.0002,lty=3)
abline(h=-0.0002,lty=3)

# KDE
plot(density(returns))

# QQ plots
qqnorm(returns)
qqline(returns)

#############################################################################
# Subsample
#############################################################################

# Subsample
returnsSub = returns[abs(returns)<0.0002]

# Statistics
mu = mean(returnsSub)
sigma = sd(returnsSub)

# Time series plot
plot.ts(returnsSub)

# Normal
x = seq(mu-3.5*sigma, mu+3.5*sigma, length=10000)
y = dnorm(x, mu, sigma)

# KDE
par(mfrow=c(1,2))
plot(density(returnsSub))
lines(x,y,lty=3)
plot(density(returnsSub, adjust=10), ylim=c(0,max(y)))
lines(x,y,lty=3)

# QQ plots
qqnorm(returnsSub)
qqline(returnsSub)

