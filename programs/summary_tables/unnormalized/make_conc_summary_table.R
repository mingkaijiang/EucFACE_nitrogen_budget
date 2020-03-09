#### To make EucFACE N summary table by CO2 treatment
#### Ignore time but produce time coverage information

make_conc_summary_table <- function() {
    
    ### Define concentration variable names
    conc.terms <- c("Canopy N Conc", "Wood N Conc", "Coarse Root N Conc", "Fine Root N Conc",
                    "Understorey N Conc", "Leaflitter N Conc", 
                    "Frass N Conc", "Microbial N Conc", "Soil N Conc")
    
    treatDF <- data.frame(conc.terms)
    treatDF$R1 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R3 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R4 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R5 <- rep(NA, length(treatDF$conc.terms))
    treatDF$R6 <- rep(NA, length(treatDF$conc.terms))
    
    treatDF$aCO2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$conc.terms))
    treatDF$diff <- rep(NA, length(treatDF$conc.terms))
    treatDF$percent_diff <- rep(NA, length(treatDF$conc.terms))
    
    treatDF$year_start <- rep(NA, length(treatDF$conc.terms))
    treatDF$year_end <- rep(NA, length(treatDF$conc.terms))
    treatDF$timepoint <- rep(NA, length(treatDF$conc.terms))
    treatDF$notes <- rep(NA, length(treatDF$conc.terms))
    
    ### Canopy N concentration
    out <- summaryBy(PercN~Ring,data=canopy_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Canopy N Conc", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Canopy N Conc"] <- min(year(canopy_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Canopy N Conc"] <- max(year(canopy_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Canopy N Conc"] <- length(unique(canopy_n_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Canopy N Conc"] <- "Only green leaf"
    
    
    ### Wood N concentration
    out <- summaryBy(PercN~Ring,data=wood_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Wood N Conc", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Wood N Conc"] <- min(year(wood_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Wood N Conc"] <- max(year(wood_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Wood N Conc"] <- length(unique(wood_n_concentration$Date)) 
    treatDF$notes[treatDF$conc.terms == "Wood N Conc"] <- "Only one data point per ring"
    
    ### Coarse root N concentration
    out <- summaryBy(PercN~Ring,data=wood_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Coarse Root N Conc", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Coarse Root N Conc"] <- min(year(wood_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Coarse Root N Conc"] <- max(year(wood_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Coarse Root N Conc"] <- length(unique(wood_n_concentration$Date)) 
    treatDF$notes[treatDF$conc.terms == "Coarse Root N Conc"] <- "Used wood N concentration"
    
    
    ### Fine root N concentration
    out <- summaryBy(PercN~Ring,data=fineroot_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Fine Root N Conc", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Fine Root N Conc"] <- min(year(fineroot_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Fine Root N Conc"] <- max(year(fineroot_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Fine Root N Conc"] <- length(unique(fineroot_n_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Fine Root N Conc"] <- "Depth 0 - 30 cm"
    
    
    ### Leaf litter N concentration
    out <- summaryBy(PercN~Ring,data=leaflitter_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Leaflitter N Conc", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Leaflitter N Conc"] <- min(year(leaflitter_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Leaflitter N Conc"] <- max(year(leaflitter_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Leaflitter N Conc"] <- length(unique(leaflitter_n_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Leaflitter N Conc"] <- "Considered both senecsed leaf and leaf litter"
    
    
    ### Understorey N concentration
    out <- summaryBy(PercN~Ring,data=understorey_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Understorey N Conc", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Understorey N Conc"] <- min(year(understorey_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Understorey N Conc"] <- max(year(understorey_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Understorey N Conc"] <- length(unique(understorey_n_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Understorey N Conc"] <- "Assumed Cymbopogon and Microlaena contributed equally"
    
    ### Understorey Litter N concentration
    # no data
    
    ### Frass N concentration
    out <- summaryBy(PercN~Ring,data=frass_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Frass N Conc", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Frass N Conc"] <- min(year(frass_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Frass N Conc"] <- max(year(frass_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Frass N Conc"] <- length(unique(frass_n_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Frass N Conc"] <- "Direct measurement"
    
    ### Microbial N concentration
    out <- summaryBy(PercN~Ring,data=microbial_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Microbial N Conc", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Microbial N Conc"] <- min(year(microbial_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Microbial N Conc"] <- max(year(microbial_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Microbial N Conc"] <- length(unique(microbial_n_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Microbial N Conc"] <- "Top 10 cm"
    
    ### Soil N concentration
    out <- summaryBy(PercN~Ring,data=soil_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil N Conc", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Soil N Conc"] <- min(year(soil_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Soil N Conc"] <- max(year(soil_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Soil N Conc"] <- length(unique(soil_n_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Soil N Conc"] <- "Averaged across all N forms"
    
    
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
