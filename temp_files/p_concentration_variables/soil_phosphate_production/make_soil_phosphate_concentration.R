
make_soil_phosphate_concentration <- function(func) {
    
    # phosphate:	Phosphate-P concentrations in kg dry soil (mg/kg) 
    
    # download the data
    download_soil_phosphate_data()
    
    ## read in data - extractable NP data
    myDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_extractableNP_L1_20140314-2014-11-17_V1.csv"))
    
    myDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_extractableNP_L1_20150310-20151130_V1.csv"))
    
    myDF1 <- myDF1[,1:8]
    myDF2 <- myDF2[,c("Date", "sample_number", "ring", "plot", "depth", "nitrate", "ammonium", "phosphate")]
    names(myDF2) <- names(myDF1)
    
    myDF3 <- rbind(myDF1, myDF2)
    myDF3$date <- dmy(myDF3$date)
    
    # get only top 10 cm
    myDF3 <- myDF3[which(myDF3$depth %in% " 0_10cm"),]

    # average across rings, dates, and depths, unit: mg/kg PO4-P
    myDF3.m <- summaryBy(phosphate~date+ring+depth,data=myDF3,FUN=func,keep.names=T,na.rm=T)
    
    myDF3.m <- myDF3.m[,c("date", "ring", "phosphate")]
    
    #myDF3.m <- myDF3.m[-c(1:6),]
    
    # read in Shun's data to expand the temporal coverages of the previous data
    myDF4 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILEXTRACTABLENUTRIENTS_L3_20120613-20140310.csv"))
    
    myDF4.m <- summaryBy(phosphate~date+ring,data=myDF4,FUN=func,keep.names=T,na.rm=T)
    myDF4.m$date <- as.Date(myDF4.m$date)
    
    # combine both dataframes
    myDF <- rbind(myDF4.m, myDF3.m)
    
    # convert PO4-P in mg/kg to %
    myDF$PercP <- myDF$phosphate / 10000
    
    # output table
    myDF.out <- myDF[,c("date", "ring", "PercP")]
    colnames(myDF.out) <- c("Date", "Ring", "PercP")
    

    return(myDF.out)
}