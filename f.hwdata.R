f.hwdata <- function(fitme, nahead=14, freq=7) {

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

fitme.hw <- HoltWinters(fitme.ts)

fitme.pr <- predict(fitme.hw, n.ahead=nahead,prediction.interval=TRUE)

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

fitme.fitted <- tstodf(fitme.hw$fitted, startdate)
fitme.data <- tstodf(fitme.pr, enddate+1)

return(list(fitme.fitted,fitme.data))

}
