# ECON 114
# April Dawn Kester
# Winter 2014

#import data
library(quantmod)
getSymbols("TWTR", src="yahoo", from="2013-12-01", to="2013-12-31")
diff_twtr = diff(log(TWTR$TWTR.Adjusted))
diff_twtr = diff_twtr[-1, ]

#standard plot
par(bg="white")
plot(diff_twtr, main="Twitter")

#histogram
par(bg="white")
hist(diff_twtr, breaks=30, xlab="twitter", main="Histogram")

#kernels standard/robust
par(mfrow=c(1,2), bg="white")

normalDens = rnorm(1000000, mean(diff_twtr), sd(diff_twtr))
plot(density(diff_twtr), xlim=c(-0.2,0.2), main="Standard Estimates")
lines(density(normalDens), lty=2)

normalDens = rnorm(1000000, median(diff_twtr), mad(diff_twtr))
plot(density(diff_twtr), xlim=c(-0.2,0.2), main="Robust Estimates")
lines(density(normalDens), lty=2)

summary(diff_twtr)
     Index            TWTR.Adjusted      
 Min.   :2013-12-03   Min.   :-0.139728  
 1st Qu.:2013-12-09   1st Qu.:-0.005822  
 Median :2013-12-16   Median : 0.043064  
 Mean   :2013-12-16   Mean   : 0.022260  
 3rd Qu.:2013-12-23   3rd Qu.: 0.055760  
 Max.   :2013-12-31   Max.   : 0.089123  

> sd(diff_twtr)
[1] 0.05505819