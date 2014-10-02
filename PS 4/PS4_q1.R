# ECON 114
# April Dawn Kester
# Winter 2014

#load data
myData = read.table("guessTheModel1.txt")

#plot acf
acf(myData)
#hypothesis MA(3)

summary(myData)
#       V1          
# Min.   :-24.2031  
# 1st Qu.: -3.4241  
# Median :  0.3123  
# Mean   :  0.3201  
# 3rd Qu.:  4.1095  
# Max.   : 24.2537
 
sd(myData$V1)
#[1] 5.5818

install.packages("forecast")
library(forecast)

auto.arima(myData)#(myData,max.q=0,max.P=0,max.Q=0)

#Series: myData 
#ARIMA(2,0,3) with non-zero mean 

#Coefficients:
#          ar1     ar2     ma1     ma2     ma3  intercept
#      -0.0853  0.0297  0.5813  0.2465  0.1002     0.3201
#s.e.   0.0576  0.0275  0.0575  0.0163  0.0068     0.0282
#coef-0/se (tstat)

#sigma^2 estimated as 23.79:  log likelihood=-300354.4
#AIC=600722.8   AICc=600722.8   BIC=600789.4
#Akaike AIC = 2k-2ln(L)

myArima <- arima(myData, order = c(2,0,3))
confint(myArima)

#                2.5 %     97.5 %
#ar1       -0.19813190 0.02758815
#ar2       -0.02431636 0.08366985
#ma1        0.46860851 0.69398169
#ma2        0.21456333 0.27843826
#ma3        0.08679693 0.11354889
#intercept  0.26490246 0.37532628

plot(myArima$resid)
#y_t=beta1 y_{t-1}+beta2*y_{t-2}+error

auto.arima(myData,max.p=0)
Series: myData 
#ARIMA(0,0,3) with non-zero mean 
#
#Coefficients:
#         ma1     ma2     ma3  intercept
#      0.4963  0.2342  0.0956     0.3201
#s.e.  0.0032  0.0034  0.0031     0.0282

#sigma^2 estimated as 23.79:  log likelihood=-300355.3
#AIC=600720.6   AICc=600720.6   BIC=600768.2

myArima2 <- arima(myData, order = c(0,0,3))
confint(myArima2)

#               2.5 %    97.5 %
#ma1       0.49012657 0.5024777
#ma2       0.22748222 0.2409924
#ma3       0.08945562 0.1017258
#intercept 0.26486904 0.3752757

newData = myData$V1[1:100]
newData2 = myData$V1[1:1000]

auto.arima(newData)
#Series: newData 
#ARIMA(1,0,0) with zero mean     

#Coefficients:
#         ar1
#      0.3863
#s.e.  0.0921

#sigma^2 estimated as 30.16:  log likelihood=-312.3
#AIC=628.61   AICc=628.73   BIC=633.82

myArima3 <- arima(newData, order = c(1,0,0))
confint(myArima3)
#               2.5 %    97.5 %
#ar1        0.1952751 0.5588057
#intercept -1.0625977 2.3645759

auto.arima(newData2)
#Series: newData2 
#ARIMA(2,0,4) with zero mean     

#Coefficients:
#          ar1      ar2     ma1     ma2     ma3     ma4
#      -0.8094  -0.4045  1.3037  1.0721  0.4850  0.0883
#s.e.   0.3936   0.2257  0.3954  0.4002  0.2222  0.1055

#sigma^2 estimated as 23.44:  log likelihood=-2996.25
#AIC=6006.49   AICc=6006.61   BIC=6040.85

myArima4 <- arima(newData2, order = c(2,0,4))
confint(myArima4)

#                2.5 %      97.5 %
#ar1       -1.56065776 -0.03943925
#ar2       -0.84761931  0.03918770
#ma1        0.52864504  2.05716013
#ma2        0.28521534  1.84287698
#ma3        0.04790143  0.90964966
#ma4       -0.11922261  0.28955049
#intercept -0.19430706  0.87165607


