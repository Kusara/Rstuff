f.weekify <- function(dataframe) {

	library(plyr)
	library(lubridate)

	dataframe <- ddply(dataframe,.(date),transform,week=floor_date(date,'week'))
	
	dataframe <- ddply(dataframe,.(week),numcolwise(sum))

	# dataframe.dply <- group_by(dataframe,date) %>% mutate(week=floor_date(date,'week')) %>% 

	return(dataframe)

}
