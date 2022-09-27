make_conc_summary_table <- function(canopy_n_concentration,
                                    wood_n_concentration,
                                    fineroot_n_concentration,
                                    understorey_n_concentration,
                                    frass_n_concentration,
                                    leaflitter_n_concentration,
                                    microbial_n_concentration,
                                    soil_n_concentration,
                                    soil_inorganic_n_concentration) {
    
    ### Define concentration variable names
    conc.terms <- c("Canopy N Conc", 
                    "Sapwood N Conc", 
                    #"Coarse Root N Conc", 
                    "Fine Root N Conc",
                    "Understorey N Conc",
                    "Leaflitter N Conc", 
                    "Frass N Conc", 
                    "Microbial N Conc 0-10cm",
                    #"Microbial N Conc 10-30cm",
                    "Soil N Conc 0-10cm",
                    "Soil N Conc 10-30cm",
                    "Soil Inorg N Conc 0-10cm",
                    #"Soil Inorg N Conc 10-30cm",
                    "Soil NO3-N Conc 0-10cm",
                    #"Soil NO3-N Conc 10-30cm",
                    "Soil NH4-N Conc 0-10cm")#,
                    #"Soil NH4-N Conc 10-30cm")
    
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
    treatDF[treatDF$conc.terms == "Sapwood N Conc", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Sapwood N Conc"] <- min(year(wood_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Sapwood N Conc"] <- max(year(wood_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Sapwood N Conc"] <- length(unique(wood_n_concentration$Date)) 
    treatDF$notes[treatDF$conc.terms == "Sapwood N Conc"] <- "Only one data point per ring"
    
    ### Coarse root N concentration
    #out <- summaryBy(PercN~Ring,data=wood_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    #treatDF[treatDF$conc.terms == "Coarse Root N Conc", 2:7] <- out$PercN
    #treatDF$year_start[treatDF$conc.terms == "Coarse Root N Conc"] <- min(year(wood_n_concentration$Date))    
    #treatDF$year_end[treatDF$conc.terms == "Coarse Root N Conc"] <- max(year(wood_n_concentration$Date))    
    #treatDF$timepoint[treatDF$conc.terms == "Coarse Root N Conc"] <- length(unique(wood_n_concentration$Date)) 
    #treatDF$notes[treatDF$conc.terms == "Coarse Root N Conc"] <- "Used wood N concentration"
    
    
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
    microbial_n_concentration1 <- microbial_n_concentration[microbial_n_concentration$Depth=="0_10",]
    out <- summaryBy(PercN~Ring,data=microbial_n_concentration1,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Microbial N Conc 0-10cm", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Microbial N Conc 0-10cm"] <- min(year(microbial_n_concentration1$Date))    
    treatDF$year_end[treatDF$conc.terms == "Microbial N Conc 0-10cm"] <- max(year(microbial_n_concentration1$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Microbial N Conc 0-10cm"] <- length(unique(microbial_n_concentration1$Date))  
    treatDF$notes[treatDF$conc.terms == "Microbial N Conc 0-10cm"] <- "Top 10 cm"
    
    ### Soil N concentration
    soil_n_concentration1 <- soil_n_concentration[soil_n_concentration$Depth=="0_10",]
    soil_n_concentration2 <- soil_n_concentration[soil_n_concentration$Depth=="0_10",]
    
    out <- summaryBy(PercN~Ring,data=soil_n_concentration1,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil N Conc 0-10cm", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Soil N Conc 0-10cm"] <- min(year(soil_n_concentration1$Date))    
    treatDF$year_end[treatDF$conc.terms == "Soil N Conc 0-10cm"] <- max(year(soil_n_concentration1$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Soil N Conc 0-10cm"] <- length(unique(soil_n_concentration1$Date))  
    treatDF$notes[treatDF$conc.terms == "Soil N Conc 0-10cm"] <- "Averaged across all N forms"
    
    
    out <- summaryBy(PercN~Ring,data=soil_n_concentration2,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil N Conc 10-30cm", 2:7] <- out$PercN
    treatDF$year_start[treatDF$conc.terms == "Soil N Conc 10-30cm"] <- min(year(soil_n_concentration2$Date))    
    treatDF$year_end[treatDF$conc.terms == "Soil N Conc 10-30cm"] <- max(year(soil_n_concentration2$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Soil N Conc 10-30cm"] <- length(unique(soil_n_concentration2$Date))  
    treatDF$notes[treatDF$conc.terms == "Soil N Conc 10-30cm"] <- "Averaged across all N forms"
    
    
    
    ### soil inorganic N
    out <- summaryBy(Total_PercN~Ring,data=soil_inorganic_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil Inorg N Conc 0-10cm", 2:7] <- out$Total_PercN
    treatDF$year_start[treatDF$conc.terms == "Soil Inorg N Conc 0-10cm"] <- min(year(soil_inorganic_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Soil Inorg N Conc 0-10cm"] <- max(year(soil_inorganic_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Soil Inorg N Conc 0-10cm"] <- length(unique(soil_inorganic_n_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Soil Inorg N Conc 0-10cm"] <- "Averaged across all N forms"
    
    
    ### NO3-N
    out <- summaryBy(Nitrate_PercN~Ring,data=soil_inorganic_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil NO3-N Conc 0-10cm", 2:7] <- out$Nitrate_PercN
    treatDF$year_start[treatDF$conc.terms == "Soil NO3-N Conc 0-10cm"] <- min(year(soil_inorganic_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Soil NO3-N Conc 0-10cm"] <- max(year(soil_inorganic_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Soil NO3-N Conc 0-10cm"] <- length(unique(soil_inorganic_n_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Soil NO3-N Conc 0-10cm"] <- ""
    
    
    ### NH4-N
    out <- summaryBy(Ammonium_PercN~Ring,data=soil_inorganic_n_concentration,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$conc.terms == "Soil NH4-N Conc 0-10cm", 2:7] <- out$Ammonium_PercN
    treatDF$year_start[treatDF$conc.terms == "Soil NH4-N Conc 0-10cm"] <- min(year(soil_inorganic_n_concentration$Date))    
    treatDF$year_end[treatDF$conc.terms == "Soil NH4-N Conc 0-10cm"] <- max(year(soil_inorganic_n_concentration$Date))    
    treatDF$timepoint[treatDF$conc.terms == "Soil NH4-N Conc 0-10cm"] <- length(unique(soil_inorganic_n_concentration$Date))  
    treatDF$notes[treatDF$conc.terms == "Soil NH4-N Conc 0-10cm"] <- ""
    
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)
    
    treatDF$aCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R2, R3, R6))), na.rm=T)
    treatDF$eCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R1, R4, R5))), na.rm=T)
    
    ###### Diff (eCO2 - aCO2)
    treatDF$diff <- round(treatDF$eCO2 - treatDF$aCO2, 4)
    
    ###### percent differences (eCO2 - aCO2) / aCO2 * 100
    treatDF$percent_diff <- round((treatDF$eCO2 - treatDF$aCO2) / (treatDF$aCO2) * 100, 2)
    
    
    ### outDF
    outDF <- treatDF[,c("conc.terms", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "aCO2_sd", "eCO2","eCO2_sd", "diff",
                        "percent_diff", "year_start", "year_end", "timepoint")]
    
    write.csv(outDF, paste0("output/n_budget/summary_table_n_concentration.csv"), row.names=F)
    
    
    
    ##### output tables
    return(outDF)
    
}
