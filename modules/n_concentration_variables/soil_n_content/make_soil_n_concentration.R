#- Make the soil N concentration
make_soil_n_concentration <- function(func){
    # return ring-specific, time series data of soil N content 

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
    
    # average across depths first, unit: %
    myDF2 <- summaryBy(totN~Date+ring+plot,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    # then checking the mean, min or max across date and ring
    myDF.m <- summaryBy(totN~Date+ring,data=myDF2,FUN=mean,keep.names=T,na.rm=T)
    
    
    myDF.m$PercN <- as.numeric(myDF.m$totN)
    
    myDF.m <- myDF.m[complete.cases(myDF.m),]

    myDF.out <- myDF.m[,c("Date", "ring", "PercN")]
    colnames(myDF.out) <- c("Date", "Ring", "PercN")
    
    return(myDF.out)
    
}
