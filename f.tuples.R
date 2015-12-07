f.tuples <- function(column, by=',') {

  library(plyr, quietly=TRUE)
  temp <- gsub("[( )$]","",column)
  temp <- strsplit(temp, by)
  returnme <- data.frame()
  returnme <- ldply(temp, .fun = function(x) rbind(c(x[1],x[2])))

  return(returnme)
}
