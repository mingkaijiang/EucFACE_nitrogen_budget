#- Make the soil bulk density variable
make_soil_bulk_density <- function(){
    # return ring-specific soil density data for top 3 depths
    
    # download the data
    download_soil_bulk_density_data()
    
    df <- read.csv(file.path(getToPath(), 
                             "FACE_P0088_RA_BULKDENSITY_L1_20170914.csv"))
    
    df <- df[,1:7]
    names(df)[7] <- "bulk_density"
    df$bulk_density <- as.character(df$bulk_density)
    df <- subset(df, bulk_density != "na")
    df$bulk_density <- as.numeric(df$bulk_density)
    
    # average across rings and depths, unit: g/cm3
    df.m <- summaryBy(bulk_density~ring+Depth,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    # unit conversion: g/cm3 to kg/m3
    df.m$bulk_density_kg_m3 <- df.m$bulk_density * g_to_kg / cm3_to_m3
    
    # revise the depth classification
    df.m$Depth_rev <- ifelse(df.m$Depth=="0-10", "0_10", 
                             ifelse(df.m$Depth==" 10-20", "10_30",
                                    ifelse(df.m$Depth=="20-30", "10_30",
                                           ifelse(df.m$Depth=="30-60", "transition",
                                                  ifelse(df.m$Depth=="30-44", "transition", NA)))))
    
    df.m <- df.m[complete.cases(df.m$Depth_rev),]
    
    # revise names
    df.m <- df.m[,c("ring", "Depth_rev", "bulk_density_kg_m3")]
    
    colnames(df.m) <- c("Ring", "Depth", "bulk_density_kg_m3")
    
    # calculate averages
    out <- summaryBy(bulk_density_kg_m3~Ring+Depth, FUN=mean,
                     data=df.m, na.rm=T, keep.names=T)
    
    
    ### only ring 5 and 6 have transition bulk density, 
    ### so fill the other rings by either taking the average
    ### or using Gimeno's relationship
    ### note ring 6 measured value is so different from the other, so
    ### revise it as well, based on Yolima's calculation sheet
    
    #trans.bk <- mean(out$bulk_density_kg_m3[out$Depth=="transition"])
    #tmpDF1 <- out[out$Ring==6&out$Depth=="transition",]
    #tmpDF1$bulk_density_kg_m3 <- trans.bk
    
    #tmpDF1$Ring <- 1
    #tmpDF2 <- tmpDF3 <- tmpDF4 <- tmpDF1
    
    #tmpDF2$Ring <- 2
    #tmpDF3$Ring <- 3
    #tmpDF4$Ring <- 4
    
    #outDF <- rbind(out, rbind(tmpDF1, rbind(tmpDF2, rbind(tmpDF3, tmpDF4))))
    
    out$bulk_density_kg_m3[out$Ring==6&out$Depth=="transition"] <- 1778
    
    tmpDF1 <- data.frame("Ring"=c(1,2,3,4), 
                         "Depth"=c("transition",
                                   "transition",
                                   "transition",
                                   "transition"),
                         "bulk_density_kg_m3"=c(1824,
                                                1861,
                                                1756,
                                                1779))
    
    outDF <- rbind(out, tmpDF1)
    
    ### order
    outDF <- outDF[order(outDF$Ring, outDF$Depth),]
    
    return(outDF)
    
}
