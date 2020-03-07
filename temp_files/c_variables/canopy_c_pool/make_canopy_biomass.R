make_canopy_biomass_pool <- function(lai_variable, sla_variable, sla_option){
    ### This function returns canopy biomass, not C biomass!
    
    if (sla_option == "mean") {
        # Use average SLA over campaigns
        SLA <- mean(sla_variable$sla_variable, na.rm=TRUE)
        
        # LAI
        out <- lai_variable[,c("Date","Ring")]
        
        # Leaf pool
        out$leaf_pool <- lai_variable$lai_variable / (10^-4 * SLA)
        
    } else if (sla_option == "variable") {
        # use ring specific and time varying SLA
        sla_variable$Year <- year(sla_variable$Date)
        sla_variable$Month <- month(sla_variable$Date)
        
        # create year-month time series
        tDF <- data.frame(lai_variable$Date, lai_variable$Ring, NA, NA)
        colnames(tDF) <- c("Date", "Ring", "Year", "Month")
        tDF$Month <- month(tDF$Date)
        tDF$Year <- year(tDF$Date)
        tDF <- tDF[,c("Ring", "Year", "Month")]
        tDF <- unique(tDF)    
        
        # assign sla to time series data frame 
        for (i in 1:6) {
            for (j in unique(sla_variable$Year)) {
                for (k in unique(sla_variable$Month)) {
                    tDF[tDF$Ring == i & tDF$Year == j & tDF$Month == k, "SLA"] <- mean(sla_variable[sla_variable$Ring == i & sla_variable$Year == j & sla_variable$Month == k, "sla_variable"], 
                                                                                       na.rm=T)
                }
            }
        }
        
        # linearly interpolate SLA to fill missing values
        for (i in 1:6) {
            tDF[tDF$Ring == i, "SLA"] <- na.interpolation(tDF[tDF$Ring == i, "SLA"])
        }
        
        # assin SLA onto LAI data frame
        lai_variable$Month <- month(lai_variable$Date)
        lai_variable$Year <- year(lai_variable$Date)
        
        for (i in 1:6) {
            for (j in unique(lai_variable$Year)) {
                for (k in unique(lai_variable$Month)) {
                    lai_variable[lai_variable$Ring == i & lai_variable$Year == j & lai_variable$Month == k, "SLA"] <- tDF[tDF$Ring == i & tDF$Year == j & tDF$Month == k, "SLA"]
                }
            }
        }
        
        # Leaf pool
        out <- lai_variable[,c("Date", "Ring")]
        out$leaf_pool <- lai_variable$lai_variable / (10^-4 * lai_variable$SLA)
    }
    
    out$leaf_pool <- out$leaf_pool * c_fraction
    
    return(out)

}
