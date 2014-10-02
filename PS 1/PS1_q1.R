# ECON 114
# April Dawn Kester
# Winter 2014

#import data
install.packages("Ecdat")
library(Ecdat)

#inspect data
data(Garch)
summary(Garch)

#declare variable pointer to data vector
diff_bp <- diff(Garch$bp)

#plot 
par(mfrow=c(3,2))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.25", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.25,0.75))~quantile(diff_bp, probs=c(0.25,0.75))))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.10", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.10,0.90))~quantile(diff_bp, probs=c(0.10,0.90))))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.05", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.05,0.95))~quantile(diff_bp, probs=c(0.05,0.95))))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.025", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.025,0.975))~quantile(diff_bp, probs=c(0.025,0.975))))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.01", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.01,0.99))~quantile(diff_bp, probs=c(0.01,0.99))))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Sample Quantiles, p=0.0025", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.0025,0.9975))~quantile(diff_bp, probs=c(0.0025,0.9975))))

#create random normal
test <- rnorm(1866)
summary(test)

#plot
par(mfrow=c(3,2))
qqnorm(test, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Random Sample Quantiles, p=0.25", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.25,0.75))~quantile(test, probs=c(0.25,0.75))))
qqnorm(test, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Random Sample Quantiles, p=0.10", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.10,0.90))~quantile(test, probs=c(0.10,0.90))))
qqnorm(test, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Random Sample Quantiles, p=0.05", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.05,0.95))~quantile(test, probs=c(0.05,0.95))))
qqnorm(test, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Random Sample Quantiles, p=0.025", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.025,0.975))~quantile(test, probs=c(0.025,0.975))))
qqnorm(test, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Random Sample Quantiles, p=0.01", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.01,0.99))~quantile(test, probs=c(0.01,0.99))))
qqnorm(test, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Random Sample Quantiles, p=0.0025", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.0025,0.9975))~quantile(test, probs=c(0.0025,0.9975))))

#declare variable pointer to data vector THIS TIME WITH LOG
diff_bp <- diff(log(Garch$bp))
summary(diff_bp)

#plot
par(mfrow=c(3,2))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Log Sample Quantiles, p=0.25", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.25,0.75))~quantile(diff_bp, probs=c(0.25,0.75))))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Log Sample Quantiles, p=0.10", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.10,0.90))~quantile(diff_bp, probs=c(0.10,0.90))))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Log Sample Quantiles, p=0.05", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.05,0.95))~quantile(diff_bp, probs=c(0.05,0.95))))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Log Sample Quantiles, p=0.025", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.025,0.975))~quantile(diff_bp, probs=c(0.025,0.975))))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Log Sample Quantiles, p=0.01", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.01,0.99))~quantile(diff_bp, probs=c(0.01,0.99))))
qqnorm(diff_bp, main = "Normal Q-Q Plot", xlab = "Theoretical Quantiles", ylab = "Log Sample Quantiles, p=0.0025", plot.it = TRUE, datax = TRUE)
abline(lm(qnorm(c(0.0025,0.9975))~quantile(diff_bp, probs=c(0.0025,0.9975))))