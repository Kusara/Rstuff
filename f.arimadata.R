f.arimadata <- function(fitme,freq=7,nahead=14) {

library(ggplot2)
library(lubridate)
library(forecast)

startdate <- head(fitme[1],n=1)[1,1]

# Handle NA date values.
fitme[1] <- seq(startdate,by=1,length.out=nrow(fitme))

# Handle NA row values.
fitme[is.na(fitme[2]),2] <- .01

enddate <- tail(fitme[1],n=1)[1,1]

fitme.ts <- ts(fitme[2], frequency=freq)

fitme.arima <- forecast(fitme.ts, h=nahead)

tstodf <- function(timeseries, startdate)
{
        startdate <- as.character(startdate)
        startdate <- as.Date(startdate,format='%Y-%m-%d')
        timeseries <- as.data.frame(timeseries)
        dataframe <- as.data.frame(seq(startdate,by=1,length.out=nrow(timeseries)))
        dataframe <- cbind(dataframe,timeseries)
        names(dataframe)[1] = 'date'
        return(dataframe)

}

fitme.data.mean <- tstodf(fitme.arima$mean, enddate+1)
fitme.data.mean[3] <- tstodf(fitme.arima$upper, enddate+1)[2]
fitme.data.mean[4] <- tstodf(fitme.arima$lower, enddate+1)[2]
names(fitme.data.mean) <- c('date','mean','upper','lower')
fitme.data.x <- tstodf(fitme.arima$x, startdate)

return(list(fitme.data.x,fitme.data.mean))

}

