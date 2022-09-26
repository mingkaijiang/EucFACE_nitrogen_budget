make_atmospheric_deposition_n_flux <- function() {
    
    # unit for N deposition: t/ha/day
    myDF <- read.csv("temp_files/EUC_met_DRY_AMB_daily_2012_2019.csv",skip=4, header=T)
    
    names(myDF)[names(myDF)=="X.year"] <- "year"
    
    myDF$Date <- as.Date(myDF$doy, origin=paste0(myDF$year-1, "-12-31"))
    
    outDF <- myDF[,c("Date", "ndep")]
    
    ### convert unit
    outDF$n_deposition_flux <- with(outDF, ndep * 100 * 1000)
    
    outDF$Ring <- 1
    
    outDF2 <- outDF
    outDF2$Ring <- 2
    
    outDF3 <- outDF
    outDF3$Ring <- 3
    
    outDF4 <- outDF
    outDF4$Ring <- 4
    
    outDF5 <- outDF
    outDF5$Ring <- 5
    
    outDF6 <- outDF
    outDF6$Ring <- 6
    
    outDF <- rbind(outDF, rbind(outDF2, rbind(outDF3, rbind(outDF4, rbind(outDF5, outDF6)))))
    
    
    
    out <- outDF[,c("Date", "Ring", "n_deposition_flux")]
    
    out$Days <- 1
    
    
    
    return(out)
        

}