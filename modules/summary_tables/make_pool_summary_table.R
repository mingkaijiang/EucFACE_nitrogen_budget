
#### To make EucFACE N summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for pools

make_pool_summary_table <- function(canopy_n_pool,
                                    wood_n_pool,
                                    fineroot_n_pool,
                                    coarseroot_n_pool,
                                    leaflitter_n_pool,
                                    understorey_n_pool,
                                    soil_n_pool,
                                    soil_inorganic_n_pool,
                                    microbial_n_pool) {
    
    ### Define pool variable names
    terms <- c("Canopy N Pool", 
               "Wood N Pool", 
               "Sapwood N Pool", 
               "Heartwood N Pool",
               "Coarse Root N Pool", 
               "Fine Root N Pool",
               "Understorey N Pool", 
               "Canopy Litter N Pool",
               "Microbial N Pool 0-10cm", 
               "Soil N Pool 0-10cm",
               "Soil N Pool 10-30cm",
               "Soil Inorg N Pool 0-10cm",
               "Soil NO3-N Pool 0-10cm",
               "Soil NH4-N Pool 0-10cm")
    
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
    out <- summaryBy(leaflitter_n_pool~Ring,data=leaflitter_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Canopy Litter N Pool", 2:7] <- out$leaflitter_n_pool
    treatDF$year_start[treatDF$terms == "Canopy Litter N Pool"] <- min(year(leaflitter_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Canopy Litter N Pool"] <- max(year(leaflitter_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Canopy Litter N Pool"] <- length(unique(leaflitter_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Canopy Litter N Pool"] <- "calculated based on leaflitter N concentration and leaflitter pool"
    
    ### Wood N 
    out <- summaryBy(wood_n_pool~Ring,data=wood_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Wood N Pool", 2:7] <- out$wood_n_pool
    treatDF$year_start[treatDF$terms == "Wood N Pool"] <- min(year(wood_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Wood N Pool"] <- max(year(wood_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Wood N Pool"] <- length(unique(wood_n_pool$Date)) 
    treatDF$notes[treatDF$terms == "Wood N Pool"] <- ""
    
    out <- summaryBy(sapwood_n_pool~Ring,data=wood_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Sapwood N Pool", 2:7] <- out$sapwood_n_pool
    treatDF$year_start[treatDF$terms == "Sapwood N Pool"] <- min(year(wood_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Sapwood N Pool"] <- max(year(wood_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Sapwood N Pool"] <- length(unique(wood_n_pool$Date)) 
    treatDF$notes[treatDF$terms == "Sapwood N Pool"] <- ""
    
    
    out <- summaryBy(heartwood_n_pool~Ring,data=wood_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Heartwood N Pool", 2:7] <- out$heartwood_n_pool
    treatDF$year_start[treatDF$terms == "Heartwood N Pool"] <- min(year(wood_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Heartwood N Pool"] <- max(year(wood_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Heartwood N Pool"] <- length(unique(wood_n_pool$Date)) 
    treatDF$notes[treatDF$terms == "Heartwood N Pool"] <- ""
    
    
    ### Fine root N pool
    out <- summaryBy(fineroot_n_pool~Ring,data=fineroot_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Fine Root N Pool", 2:7] <- out$fineroot_n_pool
    treatDF$year_start[treatDF$terms == "Fine Root N Pool"] <- min(year(fineroot_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Fine Root N Pool"] <- max(year(fineroot_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Fine Root N Pool"] <- length(unique(fineroot_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Fine Root N Pool"] <- "Top 30 cm"
    
    ### Coarse root N pool
    out <- summaryBy(coarseroot_n_pool~Ring,data=coarseroot_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Coarse Root N Pool", 2:7] <- out$coarseroot_n_pool
    treatDF$year_start[treatDF$terms == "Coarse Root N Pool"] <- min(year(coarseroot_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Coarse Root N Pool"] <- max(year(coarseroot_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Coarse Root N Pool"] <- length(unique(coarseroot_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Coarse Root N Pool"] <- "Allometric rlt with DBH"
    
    ### Understorey N pool
    out <- summaryBy(understorey_n_pool~Ring,data=understorey_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Understorey N Pool", 2:7] <- out$understorey_n_pool
    treatDF$year_start[treatDF$terms == "Understorey N Pool"] <- min(year(understorey_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Understorey N Pool"] <- max(year(understorey_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey N Pool"] <- length(unique(understorey_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Understorey N Pool"] <- "Used harvest estimate of C pool"
    
    
    ### Microbial N pool 0-10cm
    out <- summaryBy(microbial_n_g_m2~Ring,data=microbial_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial N Pool 0-10cm", 2:7] <- out$microbial_n_g_m2
    treatDF$year_start[treatDF$terms == "Microbial N Pool 0-10cm"] <- min(year(microbial_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Microbial N Pool 0-10cm"] <- max(year(microbial_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Microbial N Pool 0-10cm"] <- length(unique(microbial_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Microbial N Pool 0-10cm"] <- "Top 10 cm"
    

    ### Soil N pool 
    soil_n_pool1 <- soil_n_pool[soil_n_pool$Depth=="0_10",]
    soil_n_pool2 <- soil_n_pool[soil_n_pool$Depth=="10_30",]
    
    out <- summaryBy(soil_n_g_m2~Ring,data=soil_n_pool1,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil N Pool 0-10cm", 2:7] <- out$soil_n_g_m2
    treatDF$year_start[treatDF$terms == "Soil N Pool 0-10cm"] <- min(year(soil_n_pool1$Date))    
    treatDF$year_end[treatDF$terms == "Soil N Pool 0-10cm"] <- max(year(soil_n_pool1$Date))    
    treatDF$timepoint[treatDF$terms == "Soil N Pool 0-10cm"] <- length(unique(soil_n_pool1$Date))  
    treatDF$notes[treatDF$terms == "Soil N Pool 0-10cm"] <- "Averaged across all N forms"
    
    
    out <- summaryBy(soil_n_g_m2~Ring,data=soil_n_pool2,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil N Pool 10-30cm", 2:7] <- out$soil_n_g_m2
    treatDF$year_start[treatDF$terms == "Soil N Pool 10-30cm"] <- min(year(soil_n_pool2$Date))    
    treatDF$year_end[treatDF$terms == "Soil N Pool 10-30cm"] <- max(year(soil_n_pool2$Date))    
    treatDF$timepoint[treatDF$terms == "Soil N Pool 10-30cm"] <- length(unique(soil_n_pool2$Date))  
    treatDF$notes[treatDF$terms == "Soil N Pool 10-30cm"] <- "Averaged across all N forms"
    
    
    ### soil Inorg N
    out <- summaryBy(total_inorganic_pool~Ring,data=soil_inorganic_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil Inorg N Pool 0-10cm", 2:7] <- out$total_inorganic_pool
    treatDF$year_start[treatDF$terms == "Soil Inorg N Pool 0-10cm"] <- min(year(soil_inorganic_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Soil Inorg N Pool 0-10cm"] <- max(year(soil_inorganic_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Soil Inorg N Pool 0-10cm"] <- length(unique(soil_inorganic_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Soil Inorg N Pool 0-10cm"] <- "Averaged across all N forms"
    
    
    out <- summaryBy(nitrate_pool~Ring,data=soil_inorganic_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil NO3-N Pool 0-10cm", 2:7] <- out$nitrate_pool
    treatDF$year_start[treatDF$terms == "Soil NO3-N Pool 0-10cm"] <- min(year(soil_inorganic_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Soil NO3-N Pool 0-10cm"] <- max(year(soil_inorganic_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Soil NO3-N Pool 0-10cm"] <- length(unique(soil_inorganic_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Soil NO3-N Pool 0-10cm"] <- ""
    
    
    out <- summaryBy(ammonium_pool~Ring,data=soil_inorganic_n_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil NH4-N Pool 0-10cm", 2:7] <- out$ammonium_pool
    treatDF$year_start[treatDF$terms == "Soil NH4-N Pool 0-10cm"] <- min(year(soil_inorganic_n_pool$Date))    
    treatDF$year_end[treatDF$terms == "Soil NH4-N Pool 0-10cm"] <- max(year(soil_inorganic_n_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Soil NH4-N Pool 0-10cm"] <- length(unique(soil_inorganic_n_pool$Date))  
    treatDF$notes[treatDF$terms == "Soil NH4-N Pool 0-10cm"] <- ""
    
    
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

