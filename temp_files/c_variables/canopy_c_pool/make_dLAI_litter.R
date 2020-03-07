make_dLAI_litter <- function(litter, sla_variable){
    
    # LAI by ring with smoother
    canopy <- make_smooth_lai_variable(kgam=kgam, timestep="1 day", return.option="list")
    
    # Make LAI change, combine with litter fall
    litterDates <- sort(unique(litter$Date))
    dat <- lapply(canopy, splitbydate, litterDates)
    
    # Get change in LAI for each inter-litter interval.
    getdlai <- function(x){
        do.call(rbind,lapply(x, function(z){
            n <- nrow(z)
            dLAI <- z$LAIsm[n] - z$LAIsm[1]
            d <- diff(z$LAIsm)
            dnegLAI <- sum(d[d < 0])
            dposLAI <- sum(d[d > 0])
            return(data.frame(dLAI=dLAI, dnegLAI=dnegLAI, dposLAI=dposLAI, LAI=mean(z$LAIsm)))
        }))
    }
    
    # r will be a dataframe with litterfall and change in LAI
    r <- list()
    for(i in 1:6){
        r[[i]] <- cbind(data.frame(Date=litterDates[2:length(litterDates)]), getdlai(dat[[i]]))
        r[[i]] <- merge(r[[i]], subset(litter, Ring == i), by=c("Date"))
    }
    r <- do.call(rbind,r)
    
    # Absolute change in LAI
    r$absdLAI <- with(r, -dnegLAI + dposLAI)
    r$LAIchange <- as.factor(r$dLAI > 0)
    levels(r$LAIchange) <- c("decreasing","increasing")
    
    ### add sla information to obtain leaf biomass and change in leaf biomass
    # assign sla to time series data frame 
    tDF <- merge(r, sla_variable, 
                 by.x = c("Date", "Ring"), by.y = c("Date", "Ring"), all = T)
    
    # linearly interpolate SLA to fill missing values
    for (i in 1:6) {
        tDF[tDF$Ring == i, "SLA"] <- na.interpolation(tDF[tDF$Ring == i, "sla_variable"])
    }
    
    
    # Leaf pool
    tDF$leaf_pool <- tDF$LAI / (10^-4 * tDF$SLA) * c_fraction
    tDF$dLEAF <- tDF$dLAI / (10^-4 * tDF$SLA) * c_fraction
    
    # convert leaf flux from unit of mg C m-2 d-1 to gC m-2 period-1
    tDF$leaflit <- tDF$leaf_flux / 1000 * tDF$Days
    
    
    out <- tDF[,c("Date", "Ring", "dLEAF", "leaflit", "dLAI", "LAI", "leaf_pool", "Start_date", "End_date", "Days", "SLA",
                  "LAIchange", "dnegLAI", "dposLAI", "absdLAI")]
    
    return(out)
    

}