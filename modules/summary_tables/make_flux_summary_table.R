
make_flux_summary_table <- function(soil_mineralization_n_flux,
                                    soil_nitrification_n_flux,
                                    canopy_n_flux,
                                    wood_n_production,
                                    fineroot_n_production,
                                    coarseroot_n_production,
                                    canopy_n_retranslocation_flux,
                                    sapwood_n_retranslocation_flux,
                                    fineroot_n_retranslocation_flux,
                                    understorey_n_retranslocation_flux,
                                    leaflitter_n_flux,
                                    bark_litter_n_flux,
                                    seed_litter_n_flux,
                                    twig_litter_n_flux,
                                    fineroot_litter_n_flux,
                                    atmospheric_n_deposition,
                                    leaching_n_flux,
                                    understorey_n_flux,
                                    understorey_litter_n_flux) {
    
    ### convert daily flux in mg N m2 d-1 to g N m-2 yr-1
    conv <- 365 / 1000
    
    ### Define production variable names
    terms <- c("Canopy N flux", 
               "Wood N flux", 
               "Coarse Root N flux", 
               "Fine Root N flux",
               "Leaflitter N flux", 
               "Fineroot Litter N flux",
               "Twig litter N flux", 
               "Bark litter N flux",
               "Seed litter N flux", 
               "Frass N flux",
               "Understorey N flux", 
               "Understorey Litter N flux", 
               "Mineralization N flux",
               "Leaching N flux", 
               "Nitrification N flux", 
               "Atmospheric deposition N flux",
               "Canopy retrans N flux",
               "Sapwood retrans N flux",
               "Fineroot retrans N flux",
               "Understorey retrans N flux")
    
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
    
    ### Canopy N flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Canopy N flux", i+1] <- with(canopy_n_flux[canopy_n_flux$Ring ==i,],
                                                              sum(canopy_n_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Canopy N flux"] <- min(year(canopy_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Canopy N flux"] <- max(year(canopy_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Canopy N flux"] <- length(unique(canopy_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Canopy N flux"] <- "need to consider leaf turnover"

    
    ### Wood N 
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Wood N flux", i+1] <- with(wood_n_production[wood_n_production$Ring ==i,],
                                                             sum(wood_n_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Wood N flux"] <- min(year(wood_n_production$Date))    
    treatDF$year_end[treatDF$terms == "Wood N flux"] <- max(year(wood_n_production$Date))    
    treatDF$timepoint[treatDF$terms == "Wood N flux"] <- length(unique(wood_n_production$Date)) 
    treatDF$notes[treatDF$terms == "Wood N flux"] <- "Based on single time point measurement"
    
    ### Fine root N flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fine Root N flux", i+1] <- with(fineroot_n_production[fineroot_n_production$Ring ==i,],
                                                             sum(fineroot_n_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Fine Root N flux"] <- min(year(fineroot_n_production$Date))    
    treatDF$year_end[treatDF$terms == "Fine Root N flux"] <- max(year(fineroot_n_production$Date))    
    treatDF$timepoint[treatDF$terms == "Fine Root N flux"] <- length(unique(fineroot_n_production$Date))  
    treatDF$notes[treatDF$terms == "Fine Root N flux"] <- "Top 30 cm"
    
    ### Coarse root N flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Coarse Root N flux", i+1] <- with(coarseroot_n_production[coarseroot_n_production$Ring ==i,],
                                                                  sum(coarseroot_n_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Coarse Root N flux"] <- min(year(coarseroot_n_production$Date))    
    treatDF$year_end[treatDF$terms == "Coarse Root N flux"] <- max(year(coarseroot_n_production$Date))    
    treatDF$timepoint[treatDF$terms == "Coarse Root N flux"] <- length(unique(coarseroot_n_production$Date))  
    treatDF$notes[treatDF$terms == "Coarse Root N flux"] <- "Allometric rlt with DBH"
    
    ### Understorey N flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Understorey N flux", i+1] <- with(understorey_n_flux[understorey_n_flux$Ring ==i,],
                                                                    sum(understorey_n_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Understorey N flux"] <- min(year(understorey_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Understorey N flux"] <- max(year(understorey_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey N flux"] <- length(unique(understorey_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Understorey N flux"] <- "Used Varsha's harvest data"
    
    ### Understorey Litter N flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Understorey Litter N flux", i+1] <- with(understorey_litter_n_flux[understorey_litter_n_flux$Ring ==i,],
                                                                    sum(understorey_litter_n_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Understorey Litter N flux"] <- min(year(understorey_litter_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Understorey Litter N flux"] <- max(year(understorey_litter_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey Litter N flux"] <- length(unique(understorey_litter_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Understorey Litter N flux"] <- "Used Varsha's harvest data"
    
    ### Frass production flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Frass N flux", i+1] <- with(frass_n_production[frass_n_production$Ring ==i,],
                                                                    sum(frass_n_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Frass N flux"] <- min(year(frass_n_production$Date))    
    treatDF$year_end[treatDF$terms == "Frass N flux"] <- max(year(frass_n_production$Date))    
    treatDF$timepoint[treatDF$terms == "Frass N flux"] <- length(unique(frass_n_production$Date))  
    treatDF$notes[treatDF$terms == "Frass N flux"] <- "NA"
    
    ### Leaf litter flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Leaflitter N flux", i+1] <- with(leaflitter_n_flux[leaflitter_n_flux$Ring ==i,],
                                                              sum(leaflitter_n_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Leaflitter N flux"] <- min(year(leaflitter_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Leaflitter N flux"] <- max(year(leaflitter_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Leaflitter N flux"] <- length(unique(leaflitter_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Leaflitter N flux"] <- "Only leaves, exclude twig, barks and seeds"
    
    ### Fine Root litter flux
    # assume it's the same as fine root production flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fineroot Litter N flux", i+1] <- with(fineroot_litter_n_flux[fineroot_litter_n_flux$Ring ==i,],
                                                                  sum(fineroot_litter_n_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Fineroot Litter N flux"] <- min(year(fineroot_litter_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Fineroot Litter N flux"] <- max(year(fineroot_litter_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Fineroot Litter N flux"] <- length(unique(fineroot_litter_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Fineroot Litter N flux"] <- "Assuming fineroot production = fineroot litter production"
    
    
    ### twig litter flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Twig litter N flux", i+1] <- with(twig_litter_n_flux[twig_litter_n_flux$Ring ==i,],
                                                                   sum(twiglitter_n_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Twig litter N flux"] <- min(year(twig_litter_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Twig litter N flux"] <- max(year(twig_litter_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Twig litter N flux"] <- length(unique(twig_litter_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Twig litter N flux"] <- "Assume wood N concentration"
    
    ### bark litter flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Bark litter N flux", i+1] <- with(bark_litter_n_flux[bark_litter_n_flux$Ring ==i,],
                                                                     sum(barklitter_n_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Bark litter N flux"] <- min(year(bark_litter_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Bark litter N flux"] <- max(year(bark_litter_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Bark litter N flux"] <- length(unique(bark_litter_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Bark litter N flux"] <- "Assume wood N concentration"
    
    ### seed litter flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Seed litter N flux", i+1] <- with(seed_litter_n_flux[seed_litter_n_flux$Ring ==i,],
                                                                     sum(seedlitter_n_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Seed litter N flux"] <- min(year(seed_litter_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Seed litter N flux"] <- max(year(seed_litter_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Seed litter N flux"] <- length(unique(seed_litter_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Seed litter N flux"] <- "Assume wood N concentration"
    
    ###  N mineralization flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Mineralization N flux", i+1] <- with(soil_mineralization_n_flux[soil_mineralization_n_flux$Ring ==i,],
                                                                       sum(soil_n_mineralization_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Mineralization N flux"] <- min(year(soil_mineralization_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Mineralization N flux"] <- max(year(soil_mineralization_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Mineralization N flux"] <- length(unique(soil_mineralization_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Mineralization N flux"] <- "data"
    
    
    ###  N nitrification
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Nitrification N flux", i+1] <- with(soil_nitrification_n_flux[soil_nitrification_n_flux$Ring ==i,],
                                                                    sum(soil_n_nitrification_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Nitrification N flux"] <- min(year(soil_nitrification_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Nitrification N flux"] <- max(year(soil_nitrification_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Nitrification N flux"] <- length(unique(soil_nitrification_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Nitrification N flux"] <- "data"
    
    
    
    ###  N leaching flux
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Leaching N flux", i+1] <- with(soil_leaching_n_flux[soil_leaching_n_flux$Ring ==i,],
                                                                     sum(nitrogen_leaching_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Leaching N flux"] <- min(year(soil_leaching_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Leaching N flux"] <- max(year(soil_leaching_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Leaching N flux"] <- length(unique(soil_leaching_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Leaching N flux"] <- "drainage 20 ml/d"
    
    
    ### atmospheric N deposition flux
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Atmospheric deposition N flux", i+1] <- with(atmospheric_deposition_n_flux[atmospheric_deposition_n_flux$Ring ==i,],
                                                                  sum(n_deposition_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Atmospheric deposition N flux"] <- min(year(atmospheric_deposition_n_flux$Date))    
    treatDF$year_end[treatDF$terms == "Atmospheric deposition N flux"] <- max(year(atmospheric_deposition_n_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Atmospheric deposition N flux"] <- length(unique(atmospheric_deposition_n_flux$Date))  
    treatDF$notes[treatDF$terms == "Atmospheric deposition N flux"] <- "Literature value"
    
    
    
    ###  Canopy N retrans
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Canopy retrans N flux", i+1] <- with(canopy_n_retranslocation_flux[canopy_n_retranslocation_flux$Ring ==i,],
                                                                    sum(canopy_n_retrans_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Canopy retrans N flux"] <- min(year(canopy_n_retranslocation_flux$Date))    
    treatDF$year_end[treatDF$terms == "Canopy retrans N flux"] <- max(year(canopy_n_retranslocation_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Canopy retrans N flux"] <- length(unique(canopy_n_retranslocation_flux$Date))  
    treatDF$notes[treatDF$terms == "Canopy retrans N flux"] <- "data"
    
    
    ###  Sapwood N retrans
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Sapwood retrans N flux", i+1] <- with(sapwood_n_retranslocation_flux[sapwood_n_retranslocation_flux$Ring ==i,],
                                                                     sum(sapwood_n_retrans_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Sapwood retrans N flux"] <- min(year(sapwood_n_retranslocation_flux$Date))    
    treatDF$year_end[treatDF$terms == "Sapwood retrans N flux"] <- max(year(sapwood_n_retranslocation_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Sapwood retrans N flux"] <- length(unique(sapwood_n_retranslocation_flux$Date))  
    treatDF$notes[treatDF$terms == "Sapwood retrans N flux"] <- "estimates"
    
    
    ###  Fineroot N retrans
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Fineroot retrans N flux", i+1] <- with(fineroot_n_retranslocation_flux[fineroot_n_retranslocation_flux$Ring ==i,],
                                                                      sum(fineroot_n_retranslocation_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Fineroot retrans N flux"] <- min(year(fineroot_n_retranslocation_flux$Date))    
    treatDF$year_end[treatDF$terms == "Fineroot retrans N flux"] <- max(year(fineroot_n_retranslocation_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Fineroot retrans N flux"] <- length(unique(fineroot_n_retranslocation_flux$Date))  
    treatDF$notes[treatDF$terms == "Fineroot retrans N flux"] <- "estimates"
    
    
    ### Understorey N retrans flux
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Understorey retrans N flux", i+1] <- with(understorey_n_retranslocation_flux[understorey_n_retranslocation_flux$Ring ==i,],
                                                                  sum(understorey_n_retranslocation_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Understorey retrans N flux"] <- min(year(understorey_n_retranslocation_flux$Date))    
    treatDF$year_end[treatDF$terms == "Understorey retrans N flux"] <- max(year(understorey_n_retranslocation_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey retrans N flux"] <- length(unique(understorey_n_retranslocation_flux$Date))  
    treatDF$notes[treatDF$terms == "Understorey retrans N flux"] <- "estimate"
    
    
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

