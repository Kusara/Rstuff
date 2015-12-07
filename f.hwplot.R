f.hwplot <- function(fitme, nahead=14, freq=7,bin='day') {

library(ggplot2)
library(lubridate)
library(forecast)

startdate <- head(fitme[1],n=1)[1,1]

# Handle NA date values.
fitme[1] <- seq(startdate,by=bin,length.out=nrow(fitme))

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
	dataframe <- as.data.frame(seq(startdate,by=bin,length.out=nrow(timeseries)))
	dataframe <- cbind(dataframe,timeseries)
	names(dataframe)[1] = 'date'
	return(dataframe)

}

fitme.plot.df <- tstodf(fitme.pr, enddate + 1)
temp <- fitme.plot.df[1,]
temp2 <- tail(fitme[,c(1,2)],1)
temp2$upr <- 0
temp2$lwr <- 0
names(temp2)[1:2] <- c("date","fit")
fitme.plot.df <- rbind(temp2, fitme.plot.df)

ggplot(fitme.plot.df, aes(date,fit)) +
geom_ribbon(aes(date,ymin=lwr,ymax=upr), alpha=.2, fill='green') +
geom_line(data=fitme.plot.df[-1,],color='blue') +
geom_line(data=fitme,aes_string('date',names(fitme)[2])) +
geom_vline(x=as.numeric(max(fitme$date)), lty=2, color='red', alpha=.5) + 
ylab('Values') + xlab('Time')
}
