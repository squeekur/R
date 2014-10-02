# ECON 114
# April Dawn Kester
# Winter 2014

# Number of observations in the data
nObs = 6

# True parameters of the model
alpha = 2.87
beta = 5.1

# Indpendent variable in the sample
x1 = c(3.5, 9.1, 0.11, 8.76, 5.4, 8.3)

# Random variation added in the mode
eps = rnorm(nObs,0,5)

# Compute the dependent variable in the dataset
y = alpha + beta*x1 + eps

# Run the univariate regression
regSingle = lm(y~x1)

# Plots the data
plot(x1,y)
abline(regSingle, lty=3)

# Create new independent variables that are meaningless
x2 = rnorm(nObs)
x3 = rnorm(nObs)
x4 = rnorm(nObs)
x5 = rnorm(nObs)

# Run the multivariate regression
regMult = lm(y~x1+x2+x3+x4+x5)

# Check the residuals - we should have a perfect fit
regMult$resid

# Suppose we get new data
x1New = c(5.6, 4.3, 7.2, 7.1, 8.77, 0.7)
x2New = rnorm(nObs)
x3New = rnorm(nObs)
x4New = rnorm(nObs)
x5New = rnorm(nObs)
epsNew = rnorm(nObs,0,5)
yNew = alpha + beta*x1New + epsNew

# Forecast yNew after only observing x1New with simple model
ySingle = regSingle$coef[1] + regSingle$coef[2]*x1New
sum((ySingle - yNew)^2)

# Forecast yNew after only all new x variables with full model
yMult = regMult$coef[1] + regMult$coef[2]*x1New + regMult$coef[3]*x2New + regMult$coef[4]*x1New + regMult$coef[5]*x4New + regMult$coef[6]*x5New
sum((yMult - yNew)^2)