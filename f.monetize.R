	f.monetize <- function(dataframe) {

	
	colfun <- function(column) {

		if (is.numeric(column)) {
		
			return(column)
		
		}

		column <- as.character(column)

		if (any(grepl("(\\$|,| -)", column))) {	

			column <- gsub("(\\$|,)","",column)
			column <- gsub(" -","0",column)
			column <- as.numeric(column) 

		}

		return(column)

	}

	if (NCOL(dataframe)>1) {
		
		namelist <- names(dataframe)

		dataframe <- as.data.frame(lapply(1:NCOL(dataframe), function(x) colfun(dataframe[,x])))

		names(dataframe) <- namelist

	} else if (NCOL(dataframe)==1) {

		dataframe <- colfun(dataframe)

	} else warning("Monetize does not know how to handle that.")

	return(dataframe)
	
}
