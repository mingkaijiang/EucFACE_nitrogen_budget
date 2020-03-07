splitbydate <- function(dfr, datevec){
    
    datevec <- as.Date(datevec)
    
    l <- list()
    for(i in 2:length(datevec)){
        l[[i]] <- subset(dfr, Date >= datevec[i-1] & Date < datevec[i])
    }
    l[[1]] <- NULL
    
    return(l)
}
