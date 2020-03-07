make_understorey_percent_live_estimate <- function() {
    
    
    ### download data
    download_understorey_aboveground_data()
    
    ### read in the data 
    inDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_P0061_RA_PATHARE_UNDERSTORY_ABOVEGROUND_BIOMASS_L2_20150201_20160730.csv"))
    
    ### read in Matthias's harvest data 
    inDF2 <- read.csv("temp_files/EucFACE_GrassStrip_Harvest_20170523.csv")
    
    ### process inDFs
    inDF1$Date <- paste0(as.character(inDF1$month), "-1")
    inDF1$Date <- as.Date(inDF1$Date, "%y-%b-%d")
    
    inDF1$live_g <- inDF1$grasses_live_g + inDF1$forbs_g
    
    tempDF1 <- data.frame(inDF1$ring, inDF1$Date, 
                          inDF1$live_g, inDF1$dead_g, inDF1$total_g)
    colnames(tempDF1) <- c("Ring", "Date", "Live_g", "Dead_g", "Total_g")
    
    
    
    tempDF2 <- data.frame(inDF2$Ring, "2017-05-01",
                          inDF2$LiveBiomassDW, inDF2$DeadBiomassDW,
                          inDF2$LiveBiomassDW + inDF2$DeadBiomassDW)
    colnames(tempDF2) <- c("Ring", "Date", "Live_g", "Dead_g", "Total_g")
    
    # combine data
    myDF <- rbind(tempDF1, tempDF2)
    
    # calculate percentage
    myDF$percent_live <- with(myDF, Live_g/Total_g)
    
    # summary data frame
    out <- summaryBy(percent_live~Date+Ring, data=myDF, FUN=mean, keep.names=T, na.rm=T)

    return(out)
}

