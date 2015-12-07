f.arimaplot <- function(fitme,freq=7,nahead=14) {

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

fitme.plot.mean <- tstodf(fitme.arima$mean, enddate+1)
fitme.plot.mean[3] <- tstodf(fitme.arima$upper, enddate+1)[2]
fitme.plot.mean[4] <- tstodf(fitme.arima$lower, enddate+1)[2]
names(fitme.plot.mean) <- c('date','mean','upper','lower')
fitme.plot.x <- tstodf(fitme.arima$x, startdate)

ggplot(fitme.plot.mean, aes(date,mean)) +
geom_ribbon(aes(date,ymin=lower,ymax=upper), alpha=.2, fill='green') +
geom_line(color='blue') +
geom_line(data=fitme,aes_string('date',names(fitme)[2])) +
geom_vline(x=as.numeric(max(fitme$date)), lty=2, color='red', alpha=.5)

}
