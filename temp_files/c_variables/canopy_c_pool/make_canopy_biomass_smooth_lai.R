make_canopy_biomass_pool_smooth_lai <- function(lai_variable, sla_variable){
    ### This function returns canopy biomass, not C biomass!
    
    SLA <- summaryBy(sla_variable~Ring, FUN=mean, data=sla_variable, keep.names=T, na.rm=T)
    
    tDF <- lai_variable

    # assign sla to time series data frame 
    tDF <- merge(lai_variable, sla_variable, 
                 by.x = c("Date", "Ring"), by.y = c("Date", "Ring"), all = T)
        
    # linearly interpolate SLA to fill missing values
    for (i in 1:6) {
            tDF[tDF$Ring == i, "SLA"] <- na.interpolation(tDF[tDF$Ring == i, "sla_variable"])
    }
        

    # Leaf pool
    tDF$leaf_pool <- tDF$LAIsmooth / (10^-4 * tDF$SLA) * c_fraction
    tDF$dLEAF <- tDF$dLAI / (10^-4 * tDF$SLA) * c_fraction
    
    out <- tDF[,c("Date", "Ring", "leaf_pool", "dLEAF", "ndays", "LAIsmooth", "dLAI", "SLA")]
    
    return(out)

}
