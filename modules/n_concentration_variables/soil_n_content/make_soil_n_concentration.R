#- Make the soil N concentration
make_soil_n_concentration <- function(){
    # return ring-specific, time series data of soil N content 

    # download the data
    download_soil_data()
    
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
    
    myDF <- rbind(myDF2, rbind(myDF3, rbind(myDF4, myDF5)))
    myDF$Date <- dmy(myDF$Date)
    
    myDF$depth <- gsub(" 0-10cm", "0_10", myDF$depth)
    myDF$depth <- gsub("0-10cm", "0_10", myDF$depth)
    myDF$depth <- gsub(" 10-20cm", "10_30", myDF$depth)
    myDF$depth <- gsub("10-20cm", "10_30", myDF$depth)
    myDF$depth <- gsub(" 20-30cm", "10_30", myDF$depth)
    myDF$depth <- gsub("20-30cm", "10_30", myDF$depth)
    
    myDF <- myDF[,c("Date", "ring", "depth", "totN")]
    colnames(myDF) <- c("Date", "Ring", "Depth", "PercN")
    
    # average across depths first, unit: %
    myDF.m <- summaryBy(PercN~Date+Ring+Depth,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    myDF.m$PercN <- as.numeric(myDF.m$PercN)
    
    myDF.m <- myDF.m[complete.cases(myDF.m),]
    
    return(myDF.m)
    
}
