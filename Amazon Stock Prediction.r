library(pacman)
pacman::p_load(forecast,quantmod,tseries,timeSeries,xts)

#Variable plugin 
stockname= 'AMZN'
startdate='2015-01-01'
enddate='2021-07-07'

##Loading the data 
#Pull request to extract data from yahoo finance 
stockvar=getSymbols(c(stockname),src = 'yahoo',from=startdate,to= enddate,auto.assign=FALSE)
stockvar=na.omit(stockvar)

#Chart time series
chartSeries(stockvar,theme = "black",name = c(stockname)[1])

#Pull close price series at 4th column of the dataset 

price= stockvar[,4]

##Decomposing data 
stockvar.ts= ts(price,start = 2015-01-02,frequency = 120)
stockvar.de= decompose(stockvar.ts)
plot(stockvar.de)

graphics.off()
##Smoothing the data
par(mfrow=c(4,2))

#Log Prices
logprice= log(price)
plot(logprice,type='l',xlab='Time',ylab='Log(price)',main='Logaritmic Price Returns')

#Square root prices 
sqrtprice= sqrt(price)
plot(sqrtprice,type='l',xlab='Time',ylab='Sqrt(price)',main='Square Root Price Returns')

#Difference logarithmic Price Returns 
dlogprice= diff(log(price),lag=1)
dlogprice= dlogprice[!is.na((dlogprice))]
plot(dlogprice,type='l',xlab='Time',ylab='Log(Price)',main='Differenced Logarithmic Price Returns')

#Difference Square Root Price Returns 
dsqrtprice= diff(sqrt(price),lag=1)
dsqrtprice= dsqrtprice[!is.na(dsqrtprice)]
plot(dsqrtprice,type='l',xlab='Time',ylab='Sqrt(Price)',main="Differenced Square Root Price Returns")

##Performing the ADF test
#Performing Augmented Dickey Fuller test

print(adf.test(logprice))
print(adf.test(sqrtprice))
print(adf.test(dlogprice))
print(adf.test(dsqrtprice))

##Correlograms/Autocorrelation
#For logarithmic price
par(mfrow=c(1,2))
acf(dlogprice, main= 'ACF for Logarithmic Price Returns')
pacf(dlogprice,main= 'PACF for Logarithmic Price Returns')

#For Squareroot Price Returns
par(mfrow=c(1,2))
acf(dsqrtprice, main= 'ACf for the Square root Prices')
pacf(dsqrtprice, main = 'PACF for the sqaure roor Prices')

##Programming a fitted forecast 
#Initialize real log returns via xts
realreturn= xts(0,as.Date("2020-11-25","%Y-%m-%d"))
#Intitialize forecasted returns via dataframe
forecastreturn= data.frame(Forecasted= numeric())

split=floor(nrow(dlogprice)*(2.9/3))
for (s in split:(nrow(dlogprice)-1)){
  
  dlogprice_training= dlogprice[1:s,]
  dlogprice_testing= dlogprice[(s+1):nrow(dlogprice),]
  
  fit= arima(dlogprice_training,order= c(2,0,2),include.mean=FALSE)
  summary(fit)
  
  arima.forecast= forecast(fit,h=1)
  summary(arima.forecast)
  
  Box.test(fit$residuals, lag = 1, type='Ljung-Box')
  
  forecastreturn= rbind(forecastreturn,arima.forecast$mean[1])
  colnames(forecastreturn)= c("Forecasted")
  returnseries= dlogprice[(s+1),]
  realreturn= c(realreturn,xts(returnseries))
  rm(returnseries)
}

realreturn= realreturn[-1]
forecastreturn=xts(forecastreturn,index(realreturn))
plot(realreturn,type='l',main='Actual Returns vs Forecasted Returns')
lines(forecastreturn,lwd=2,col='red')
comparision = merge(realreturn,forecastreturn)
comparision$accuracy= sign(comparision$realreturn)==sign(comparision$Forecasted)
print(comparision)

Accuracy_perc = sum(comparision$accuracy==1)*100/length(comparision$accuracy)
print(Accuracy_perc)