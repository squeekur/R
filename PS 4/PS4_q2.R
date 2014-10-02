# ECON 114
# April Dawn Kester
# Winter 2014

#CHECKING RISIDUALS

Box.test(myData, lag=10, fitdf=5)

#	Box-Pierce test

#data:  myData
#X-squared = 28568.38, df = 5, p-value < 2.2e-16

myArima <- arima(myData, order = c(2,0,3))
acf(myArima$resid)

y=residuals(myArima)
Box.test(y, lag=6, fitdf=1)

#Box-Pierce test

#data:  y
#X-squared = 1.0466, df = 5, p-value = 0.9587
#Accept null, uncorrelated

myArima2 <- arima(myData, order =c(0,0,3))
acf(myArima2$resid)

y=residuals(myArima2)
Box.test(y, lag=6, fitdf=1)

#	Box-Pierce test

#data:  y
#X-squared = 3.4958, df = 5, p-value = 0.624
#Accept null, uncorrelated

Box.test(y, lag=11, fitdf=1)

#	Box-Pierce test

#data:  y
#X-squared = 16.0206, df = 10, p-value = 0.09904

Box.test(y, lag=16, fitdf=1)

#	Box-Pierce test

#data:  y
#X-squared = 24.2649, df = 15, p-value = 0.06073

Box.test(y, lag=21, fitdf=1)

#	Box-Pierce test

#data:  y
#X-squared = 27.8544, df = 20, p-value = 0.1129
#df 5, 10, 15, 20 insignificant

plot(y)
qqnorm(y, datax=TRUE)

