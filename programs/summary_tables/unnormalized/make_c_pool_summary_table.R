
#### To make EucFACE C summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for pools

make_c_pool_summary_table <- function() {
    
    ### Define pool variable names
    terms <- c("Canopy C Pool", "Wood C Pool", "Coarse Root C Pool", "Fine Root C Pool",
               "Understorey C Pool", "Leaflitter C Pool",
               "Microbial C Pool", 
               "Soil C Pool")
    
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
    
    ### Canopy C 
    out <- summaryBy(leaf_pool~Ring,data=canopy_biomass_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Canopy C Pool", 2:7] <- out$leaf_pool
    treatDF$year_start[treatDF$terms == "Canopy C Pool"] <- min(year(canopy_biomass_pool$Date))    
    treatDF$year_end[treatDF$terms == "Canopy C Pool"] <- max(year(canopy_biomass_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Canopy C Pool"] <- length(unique(canopy_biomass_pool$Date))  
    treatDF$notes[treatDF$terms == "Canopy C Pool"] <- "Estimated based on LAI and SLA"

    
    ### Leaflitter C pool
    out <- summaryBy(leaflitter_pool~Ring,data=leaflitter_c_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Leaflitter C Pool", 2:7] <- out$leaflitter_pool
    treatDF$year_start[treatDF$terms == "Leaflitter C Pool"] <- min(year(leaflitter_c_pool$Date))    
    treatDF$year_end[treatDF$terms == "Leaflitter C Pool"] <- max(year(leaflitter_c_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Leaflitter C Pool"] <- length(unique(leaflitter_c_pool$Date))  
    treatDF$notes[treatDF$terms == "Leaflitter C Pool"] <- "based on decomposition data"
    
    
    ### Wood C 
    out <- summaryBy(wood_pool~Ring,data=wood_c_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Wood C Pool", 2:7] <- out$wood_pool
    treatDF$year_start[treatDF$terms == "Wood C Pool"] <- min(year(wood_c_pool$Date))    
    treatDF$year_end[treatDF$terms == "Wood C Pool"] <- max(year(wood_c_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Wood C Pool"] <- length(unique(wood_c_pool$Date)) 
    treatDF$notes[treatDF$terms == "Wood C Pool"] <- "Estimated based on allometric relationship, considers mortality"
    
    
    ### Fine root C pool
    out <- summaryBy(fineroot_pool~Ring,data=fineroot_c_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Fine Root C Pool", 2:7] <- out$fineroot_pool
    treatDF$year_start[treatDF$terms == "Fine Root C Pool"] <- min(year(fineroot_c_pool$Date))    
    treatDF$year_end[treatDF$terms == "Fine Root C Pool"] <- max(year(fineroot_c_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Fine Root C Pool"] <- length(unique(fineroot_c_pool$Date))  
    treatDF$notes[treatDF$terms == "Fine Root C Pool"] <- "Top 30 cm"
    
    ### Coarse root C pool
    out <- summaryBy(coarse_root_pool~Ring,data=coarse_root_c_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Coarse Root C Pool", 2:7] <- out$coarse_root_pool
    treatDF$year_start[treatDF$terms == "Coarse Root C Pool"] <- min(year(coarse_root_c_pool$Date))    
    treatDF$year_end[treatDF$terms == "Coarse Root C Pool"] <- max(year(coarse_root_c_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Coarse Root C Pool"] <- length(unique(coarse_root_c_pool$Date))  
    treatDF$notes[treatDF$terms == "Coarse Root C Pool"] <- "Allometric relationship with DBH"
    
    ### Understorey C pool
    out <- summaryBy(Total_g_C_m2~Ring,data=understorey_c_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Understorey C Pool", 2:7] <- out$Total_g_C_m2
    treatDF$year_start[treatDF$terms == "Understorey C Pool"] <- min(year(understorey_c_pool$Date))    
    treatDF$year_end[treatDF$terms == "Understorey C Pool"] <- max(year(understorey_c_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey C Pool"] <- length(unique(understorey_c_pool$Date))  
    treatDF$notes[treatDF$terms == "Understorey C Pool"] <- "harvest data"

    
    ### Microbial C pool
    out <- summaryBy(Cmic_g_m2~Ring,data=microbial_c_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Microbial C Pool", 2:7] <- out$Cmic_g_m2
    treatDF$year_start[treatDF$terms == "Microbial C Pool"] <- min(year(microbial_c_pool$date))    
    treatDF$year_end[treatDF$terms == "Microbial C Pool"] <- max(year(microbial_c_pool$date))    
    treatDF$timepoint[treatDF$terms == "Microbial C Pool"] <- length(unique(microbial_c_pool$date))  
    treatDF$notes[treatDF$terms == "Microbial C Pool"] <- "Top 10 cm, may have problem during extraction"
    
    
    ### Soil C pool
    out <- summaryBy(soil_carbon_pool~Ring,data=soil_c_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Soil C Pool", 2:7] <- out$soil_carbon_pool
    treatDF$year_start[treatDF$terms == "Soil C Pool"] <- min(year(soil_c_pool$Date))    
    treatDF$year_end[treatDF$terms == "Soil C Pool"] <- max(year(soil_c_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Soil C Pool"] <- length(unique(soil_c_pool$Date))  
    treatDF$notes[treatDF$terms == "Soil C Pool"] <- "Averaged across all C forms"
    
    
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

