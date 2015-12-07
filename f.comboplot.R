f.comboplot <- function(raw, trimmed, nahead=28, freq=7) {

library(ggplot2)
library(lubridate)
library(forecast)

startdate <- head(trimmed[1],n=1)[1,1]

# Handle NA date values.
trimmed[1] <- seq(startdate,by=1,length.out=nrow(trimmed))

# Handle NA row values.
trimmed[is.na(trimmed[2]),2] <- 0.01

enddate <- tail(trimmed[1],n=1)[1,1]

trimmed.ts <- ts(trimmed[2], frequency=freq)

trimmed.hw <- HoltWinters(trimmed.ts)

trimmed.hw.adjust <- trimmed.hw

trimmed.hw.adjust$fitted <- trimmed.hw.adjust$fitted[1:(NROW(trimmed.hw.adjust$fitted)-7),]

trimmed.pr <- predict(trimmed.hw, n.ahead=nahead, prediction.interval=TRUE)

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

trimmed.plot.pr <- tstodf(trimmed.pr, enddate)
trimmed.plot.hw <- tstodf(trimmed.hw$fitted, startdate)

ggplot(trimmed.plot.pr, aes(date,fit)) +
geom_line(color='blue',alpha=.8) +
geom_ribbon(aes(date,ymin=lwr,ymax=upr), alpha=.2, fill='green') +
# geom_line(data=trimmed.plot.hw,aes(date,xhat),color='blue',alpha=.8,level=0) +
geom_line(data=raw,aes_string('date',names(raw)[2])) +
geom_vline(x=as.numeric(max(raw$date)), lty=2, color='red',alpha='.5')

}
