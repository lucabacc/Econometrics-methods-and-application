#POINT C)
mymodel <- lm(INTRATE ~ INFL + PROD + UNEMPL + COMMPRI, data = TE3_TaylorRule)
taylor <- lm(INTRATE ~ INFL + PROD, data=TE3_TaylorRule)

#compare summary statistics
summary(mymodel)
summary(taylor)
AIC(mymodel)
AIC(taylor)
BIC(mymodel)
BIC(taylor)

#POINT D)
#Chow Break Test----
install.packages("strucchange")
library(strucchange)
model1=Fstats(Break1980$INTRATE~Break1980$INFL+Break1980$PROD, from=0.01)
sctest(model1)
plot.ts(Break1980$INTRATE)

# Pvalue is less than 5% our hipothesis is rejected, there is structural change

strucchange::breakpoints(Break1980$INTRATE~Break1980$INFL+Break1980$PROD)

#Forecast test----
install.packages("forecast")
library("forecast")
tsbreak <- ts(Break1980$INTRATE,frequency=12 ) #Frequency 12 because monthly data
#plot time series
plot(tsbreak)
#Use auto arima function to get the optimal arima model
autoarima1 <- auto.arima(tsbreak)
forecast1 <- forecast(autoarima1, h=48) # h is forecasting period in line with the previous one, in this case 48 months
forecast1
plot(forecast1)
#plot residuals
plot(forecast1$residuals)
#Get accuracy statistics
summary(autoarima1)

#Jarque Bera Test----
install.packages("tseries")
library(tseries)
jarque.bera.test(taylor$residuals)

#Test skewness and kurtosis
install.packages("e1071")
library("e1071")
skewness(taylor$residuals)
kurtosis(taylor$residuals)
plot.ts(taylor$residuals)






