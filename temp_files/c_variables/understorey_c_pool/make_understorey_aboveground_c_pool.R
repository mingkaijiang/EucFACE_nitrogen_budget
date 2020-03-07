make_understorey_aboveground_c_pool <- function(c_frac,
                                                strip_area) {
    
    
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
    
    #- average across rings and dates
    liveDF <- summaryBy(Live_g~Date+Ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    deadDF <- summaryBy(Dead_g~Date+Ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    totDF <- summaryBy(Total_g~Date+Ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    # convert from g per 0.1 m-2 to g/m2, and make an assumption for C fraction
    outDF <- cbind(liveDF, deadDF$Dead_g, totDF$Total_g)
    names(outDF) <- c("Date", "Ring", "Live_g", "Dead_g", "Total_g")
    outDF$Live_g_C_m2 <- outDF$Live_g / strip_area * c_frac
    outDF$Dead_g_C_m2 <- outDF$Dead_g / strip_area * c_frac
    outDF$Total_g_C_m2 <- outDF$Live_g_C_m2 + outDF$Dead_g_C_m2
    out <- outDF[,c("Date", "Ring", "Live_g_C_m2", "Dead_g_C_m2", "Total_g_C_m2")]
    
    return(out)
}

