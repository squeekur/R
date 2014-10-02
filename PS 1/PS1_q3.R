# ECON 114
# April Dawn Kester
# Winter 2014

#locate directory
getwd()
[1] "/Users/akester"

#load data
mydata = read.table("myst.txt")

#info
summary(mydata)
       V1         
 Min.   :-6.2599  
 1st Qu.:-1.3810  
 Median :-0.1951  
 Mean   :-0.2129  
 3rd Qu.: 1.1412  
 Max.   : 5.3602  
 
sd(mydata$V1)
[1] 1.974754

#standard plot
par(bg="white")
plot(mydata$V1, main="Mystery")

#histogram
par(bg="white")
hist(mydata$V1, breaks=30, xlab="Mystery", main="Histogram")

#Kernel standard/robust
par(mfrow=c(1,2), bg="white")

normalDens = rnorm(1000000, mean(mydata$V1), sd(mydata$V1))
plot(density(mydata$V1), xlim=c(-6.3,5.4), main="Standard Estimates")
lines(density(normalDens), lty=2)

normalDens = rnorm(1000000, median(mydata$V1), mad(mydata$V1))
plot(density(mydata$V1), xlim=c(-6.3,5.4), main="Robust Estimates")
lines(density(normalDens), lty=2)

#plot q
par(mfrow=c(3,2))
qqnorm(mydata$V1, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.25", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.25,0.75))~quantile(mydata$V1, probs=c(0.25,0.75))))
qqnorm(mydata$V1, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.10", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.10,0.90))~quantile(mydata$V1, probs=c(0.10,0.90))))
qqnorm(mydata$V1, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.05", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.05,0.95))~quantile(mydata$V1, probs=c(0.05,0.95))))
qqnorm(mydata$V1, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.025", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.025,0.975))~quantile(mydata$V1, probs=c(0.025,0.975))))
qqnorm(mydata$V1, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.01", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.01,0.99))~quantile(mydata$V1, probs=c(0.01,0.99))))
qqnorm(mydata$V1, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.0025", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.0025,0.9975))~quantile(mydata$V1, probs=c(0.0025,0.9975))))