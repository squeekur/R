# ECON 114
# April Dawn Kester
# Winter 2014

#number of simulated points
nSim = 100

#draw one set of shocks and use for each AR(1)
eps = rnorm(nSim, 0, 0.42)

#only simulating for one value of phi
phi = 0.9
#phi = c(0.9, 0.6)

y = matrix(0, nrow=nSim, ncol=length(phi))

y[1,] = eps[1]

for(j in 1:length(phi)){
	for(i in 2:nSim){
		y[i,j] = 5.2 + phi[j]*y[i-1,j]+eps[i]
	}
}

#plot time series and acf
plot(y)
acf(y)

#linear approx
yNew = y[2:100]
x = y[1:99]
reg = lm(yNew~x)

Coefficients:
(Intercept)            x  
     5.4132       0.8971
#pretty close
summary(reg)

Call:
lm(formula = yNew ~ x)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.1326 -0.2794  0.0185  0.3051  0.8864 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 5.413169   0.194431   27.84   <2e-16 ***
x           0.897137   0.004007  223.89   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.4296 on 97 degrees of freedom
Multiple R-squared:  0.9981,	Adjusted R-squared:  0.998 
F-statistic: 5.013e+04 on 1 and 97 DF,  p-value: < 2.2e-16     
     
#Number of resamples
B = 1000

yMat = matrix(0, nrow=nSim, ncol=B)
for(i in 1:B) yMat[,i] = sample(y, nSim, replace=TRUE)

phatboot = rep(0, length=B)
for (i in 1:B){
	phatboot[i] = (1/nSim)*sum(yMat[,i]) #MLE of auto regressive process?
}

mub=mean(phatboot)
mub
[1] 47.44814


