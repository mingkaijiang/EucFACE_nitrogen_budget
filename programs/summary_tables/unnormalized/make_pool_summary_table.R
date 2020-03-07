
#### To make EucFACE N summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for pools

make_pool_summary_table <- function() {
    
    ### Define pool variable names
    terms <- c("Wood N Pool", "Canopy N Pool", "Canopy Litter N Pool",
               "Fine Root N Pool",
               "Coarse Root N Pool", "Understorey N Pool", 
               "Understorey Litter N Pool",
               "Microbial N Pool", 
               "Soil N Pool")
    
    treatDF <- data.frame(terms)
    treatDF$R1 <- rep(NA, length(treatDF$terms))
    treatDF$R2 <- rep(NA, length(treatDF$terms))
    treatDF$R3 <- rep(NA, length(treatDF$terms))
    treatDF$R4 <- rep(NA, length(treatDF$terms))
    treatDF$R5 <- rep(NA, length(treatDF$terms))
    treatDF$R6 <- rep(NA, length(treatDF$terms))
    
    treatDF$aCO2 <- rep(NA, length(treatDF$terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$terms))
    treatDF$diff <- rep(NA, length(treatDF$terms))
    treatDF$percent_diff <- rep(NA, length(treatDF$terms))
    
    treatDF$year_start <- rep(NA, length(treatDF$terms))
    treatDF$year_end <- rep(NA, length(treatDF$terms))
    treatDF$timepoint <- rep(NA, length(treatDF$terms))
    treatDF$notes <- rep(NA, length(treatDF$terms))
    
    ### Canopy N 
    out <- summaryBy(leaf_n_pool~Ring,data=canopy_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Canopy N Pool", 2:7] <- out$leaf_n_pool
    treatDF$year_start[treatDF$terms == "Canopy N Pool"] <- min(year(canopy_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Canopy N Pool"] <- max(year(canopy_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Canopy N Pool"] <- length(unique(canopy_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Canopy N Pool"] <- "used monthly concentration values to extrapolate"

    ### Canopy Litter N 
    #out <- summaryBy(leaflitter_n_pool~Ring,data=leaflitter_n_pool,FUN=mean,keep.names=T,na.rm=T)
    #treatDF[treatDF$terms == "Canopy Litter N Pool", 2:7] <- out$leaflitter_n_pool
    #treatDF$year_start[treatDF$terms == "Canopy Litter N Pool"] <- min(year(leaflitter_n_pool$Date))    
    #treatDF$year_end[treatDF$terms == "Canopy Litter N Pool"] <- max(year(leaflitter_n_pool$Date))    
    #treatDF$timepoint[treatDF$terms == "Canopy Litter N Pool"] <- length(unique(leaflitter_n_pool$Date))  
    #treatDF$notes[treatDF$terms == "Canopy Litter N Pool"] <- "calculated based on leaflitter N concentration and leaflitter pool"
    
    ### Wood N 
    out <- summaryBy(wood_n_pool~Ring,data=wood_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Wood N Pool", 2:7] <- out$wood_n_pool
    treatDF$year_start[treatDF$terms == "Wood N Pool"] <- min(year(wood_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Wood N Pool"] <- max(year(wood_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Wood N Pool"] <- length(unique(wood_n_pool$Date)) 
    treatDF$notes[treatDF$terms == "Wood N Pool"] <- "Based on single time point concentration measurement"
    
    
    ### Fine root N pool
    out <- summaryBy(fineroot_n_pool~Ring,data=fineroot_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Fine Root N Pool", 2:7] <- out$fineroot_n_pool
    treatDF$year_start[treatDF$terms == "Fine Root N Pool"] <- min(year(fineroot_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Fine Root N Pool"] <- max(year(fineroot_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Fine Root N Pool"] <- length(unique(fineroot_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Fine Root N Pool"] <- "Top 30 cm"
    
    ### Coarse root N pool
    #out <- summaryBy(coarse_root_n_pool~Ring,data=coarse_root_n_pool,FUN=mean,keep.names=T,na.rm=T)
    #treatDF[treatDF$terms == "Coarse Root N Pool", 2:7] <- out$coarse_root_n_pool
    #treatDF$year_start[treatDF$terms == "Coarse Root N Pool"] <- min(year(coarse_root_n_pool$Date))    
    #treatDF$year_end[treatDF$terms == "Coarse Root N Pool"] <- max(year(coarse_root_n_pool$Date))    
    #treatDF$timepoint[treatDF$terms == "Coarse Root N Pool"] <- length(unique(coarse_root_n_pool$Date))  
    #treatDF$notes[treatDF$terms == "Coarse Root N Pool"] <- "Allometric rlt with DBH"
    
    ### Understorey N pool
    out <- summaryBy(understorey_n_pool~Ring,data=understorey_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Understorey N Pool", 2:7] <- out$understorey_n_pool
    treatDF$year_start[treatDF$terms == "Understorey N Pool"] <- min(year(understorey_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Understorey N Pool"] <- max(year(understorey_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey N Pool"] <- length(unique(understorey_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Understorey N Pool"] <- "Used harvest estimate of C pool"
    
    ### Understorey Litter N pool
    #out <- summaryBy(dead_n_pool~Ring,data=understorey_n_pool,FUN=mean,keep.names=T,na.rm=T)
    #treatDF[treatDF$terms == "Understorey Litter N Pool", 2:7] <- out$dead_n_pool
    #treatDF$year_start[treatDF$terms == "Understorey Litter N Pool"] <- min(year(understorey_n_pool$Date))    
    #treatDF$year_end[treatDF$terms == "Understorey Litter N Pool"] <- max(year(understorey_n_pool$Date))    
    #treatDF$timepoint[treatDF$terms == "Understorey Litter N Pool"] <- length(unique(understorey_n_pool$Date))  
    #treatDF$notes[treatDF$terms == "Understorey Litter N Pool"] <- "Used harvest estimate of C pool"

    
    ### Microbial N pool
    #out <- summaryBy(microbial_n_g_m2~Ring,data=microbial_n_pool,FUN=mean,keep.names=T,na.rm=T)
    #treatDF[treatDF$terms == "Microbial N Pool", 2:7] <- out$microbial_n_g_m2
    #treatDF$year_start[treatDF$terms == "Microbial N Pool"] <- min(year(microbial_n_pool$Date))    
    #treatDF$year_end[treatDF$terms == "Microbial N Pool"] <- max(year(microbial_n_pool$Date))    
    #treatDF$timepoint[treatDF$terms == "Microbial N Pool"] <- length(unique(microbial_n_pool$Date))  
    #treatDF$notes[treatDF$terms == "Microbial N Pool"] <- "Top 10 cm"
    

    ### Soil N pool
    out <- summaryBy(soil_n_g_m2~Ring,data=soil_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil N Pool", 2:7] <- out$soil_n_g_m2
    treatDF$year_start[treatDF$terms == "Soil N Pool"] <- min(year(soil_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Soil N Pool"] <- max(year(soil_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Soil N Pool"] <- length(unique(soil_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Soil N Pool"] <- "Averaged across all N forms"
    
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)
    
    treatDF$aCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R2, R3, R6))), na.rm=T)
    treatDF$eCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R1, R4, R5))), na.rm=T)
    
    ###### Diff (eCO2 - aCO2)
    treatDF$diff <- round(treatDF$eCO2 - treatDF$aCO2, 4)
    
    ###### percent differences (eCO2 - aCO2) / aCO2 * 100
    treatDF$percent_diff <- round((treatDF$eCO2 - treatDF$aCO2) / (treatDF$aCO2) * 100, 2)
    
    
    ##### output tables
    return(treatDF)
      
}

