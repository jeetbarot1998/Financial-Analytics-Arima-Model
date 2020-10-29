SPY=read.csv("SPY.csv",header = TRUE, sep=",")

# Save the date in a separate identifier as character
dates = as.character(SPY$Date)
# Remove date values from table
SPY$Date = NULL
#Announce time series data
SPY=xts(SPY$Price, as.POSIXct(dates,format="%m/%d/%Y"))
head(SPY)
#Plot the data
plot(SPY, col="darkred", main="Price Series from 2007-2019") 

# Stationarity testing
StationarityTest = ur.df(SPY,type="none",selectlags = "AIC")
summary(StationarityTest)
#Stationarity Tesing on first Differences
D.SPY=diff(SPY,lag=1,differences = 1)
plot(D.SPY,col="darkgreen")

#ACF and PACF  
ggAcf(D.SPY, lag.max = 10) + theme_bw()
ggPacf(D.SPY, lag.max = 10) + theme_bw()


Fitted.Armina=auto.arima(SPY)
q=forecast(Fitted.Armina,h=10) # h= no. of lags one wanna look ahead
summary(q)
plot(q,include=50)  # how many datapoints to show before the forecast point, to get a broad view.
