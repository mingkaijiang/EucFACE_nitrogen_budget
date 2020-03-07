make_smooth_lai_variable <- function(timestep="1 day", kgam=15, return.option="datamframe"){
    

    ## read in the lai variable data
    dat <- download_lai_variable()
    
    dat <- subset(dat, select=c(Date, Ring, LAI))
    names(dat)[3] <- "lai_variable"
    
    rings <- split(dat, dat$Ring)
    smoothlai <- lapply(rings, function(x){
        
        x <- x[order(x$Date),]
        
        gamfit <- smoothplot(as.numeric(Date),lai_variable,data=x,kgam=kgam, plotit=FALSE)
        
        dfr <- data.frame(X=as.numeric(seq.Date(min(x$Date), max(x$Date), by=timestep)))
        dfr$LAIsmooth <- predict(gamfit[[1]],dfr)
        names(dfr)[1] <- "Date"
        dfr$Date <- as.Date(dfr$Date, origin="1970-1-1")
        
        dfr$dLAI <- c(NA, diff(dfr$LAIsmooth))
        dfr$ndays <- c(NA, diff(dfr$Date))
        return(dfr)
    })
    
    if (return.option=="dataframe") {
        #head(smoothlai$R1)
        smoothlai$R1$Ring <- 1
        smoothlai$R2$Ring <- 2
        smoothlai$R3$Ring <- 3
        smoothlai$R4$Ring <- 4
        smoothlai$R5$Ring <- 5
        smoothlai$R6$Ring <- 6
        
        out <- rbind(smoothlai$R1, smoothlai$R2, smoothlai$R3, smoothlai$R4, smoothlai$R5, smoothlai$R6)
        out <- out[,c("Date", "Ring", "LAIsmooth", "dLAI", "ndays")]
        return(out)
    } else (
        return(smoothlai)
    )

}