#- Make the soil P concentration
make_soil_p_concentration <- function(func){
    # return ring-specific, time series data of soil P content 
    # need to read in multiple P data sources
    # and soil bulk density data

    # download the data
    download_soil_p_data()
    
    ## read in data - soil property data
    myDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2012.csv"))
    
    myDF3 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2013.csv"))   
    
    myDF4 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2014.csv"))
    
    myDF5 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2015.csv"))
    
    myDF2 <- myDF2[,1:10]
    myDF3 <- myDF3[,1:10]
    myDF4 <- myDF4[,1:10]
    myDF5 <- myDF5[,1:10]
    
    colnames(myDF2) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF3) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF4) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF5) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    
    myDF <- rbind(myDF2, myDF3, myDF4, myDF5)
    myDF$Date <- dmy(myDF$Date)
    
    # average across depths first, unit: ppm which is mg/kg
    myDF2 <- summaryBy(totP_ppm~Date+ring+plot,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    # then checking the mean, min or max across date and ring
    myDF.m <- summaryBy(totP_ppm~Date+ring,data=myDF2,FUN=func,keep.names=T,na.rm=T)
    
    
    myDF.m <- data.frame(lapply(myDF.m, trimws), stringsAsFactors = FALSE)
    
    myDF.m$totP_ppm <- as.numeric(myDF.m$totP_ppm)
    
    myDF.m$PercP <- myDF.m$totP_ppm / 10000
    
    myDF.m <- myDF.m[complete.cases(myDF.m),]

    myDF.out <- myDF.m[,c("Date", "ring", "PercP")]
    colnames(myDF.out) <- c("Date", "Ring", "PercP")
    
    return(myDF.out)
    
}
