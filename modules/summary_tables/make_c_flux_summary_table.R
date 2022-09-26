
#### To make EucFACE C summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for fluxes

make_c_flux_summary_table <- function() {
    
    ### convert daily flux in mg C m2 d-1 to g C m-2 yr-1
    conv <- 365 / 1000
    
    ### Define production variable names
    terms <- c( "Canopy C flux", "Wood C flux", "Coarse Root C flux", "Fine Root C flux",
                "Understorey C flux",
               "Leaflitter C flux", "Fineroot Litter C flux", "Understorey Litter C flux",
               "Frass C flux", "Twig C flux", "Bark C flux", "Seed C flux")
    
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
    
    ### Canopy C flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Canopy C flux", i+1] <- with(canopy_c_production_flux[canopy_c_production_flux$Ring ==i,],
                                                              sum(leaf_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Canopy C flux"] <- min(year(canopy_c_production_flux$Date))    
    treatDF$year_end[treatDF$terms == "Canopy C flux"] <- max(year(canopy_c_production_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Canopy C flux"] <- length(unique(canopy_c_production_flux$Date))  
    treatDF$notes[treatDF$terms == "Canopy C flux"] <- "based on leaf litter data"

    
    ### Wood C 
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Wood C flux", i+1] <- with(wood_c_production[wood_c_production$Ring ==i,],
                                                             sum(wood_production_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Wood C flux"] <- min(year(wood_c_production$Date))    
    treatDF$year_end[treatDF$terms == "Wood C flux"] <- max(year(wood_c_production$Date))    
    treatDF$timepoint[treatDF$terms == "Wood C flux"] <- length(unique(wood_c_production$Date)) 
    treatDF$notes[treatDF$terms == "Wood C flux"] <- "Difference in measurement over periods"
    
    ### Fine root C flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fine Root C flux", i+1] <- with(fineroot_c_production_flux[fineroot_c_production_flux$Ring ==i,],
                                                             sum(fineroot_production_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Fine Root C flux"] <- min(year(fineroot_c_production_flux$Date))    
    treatDF$year_end[treatDF$terms == "Fine Root C flux"] <- max(year(fineroot_c_production_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Fine Root C flux"] <- length(unique(fineroot_c_production_flux$Date))  
    treatDF$notes[treatDF$terms == "Fine Root C flux"] <- "Top 30 cm"
    
    ### Coarse root C flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Coarse Root C flux", i+1] <- with(coarse_root_c_flux[coarse_root_c_flux$Ring ==i,],
                                                                  sum(coarse_root_production_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Coarse Root C flux"] <- min(year(coarse_root_c_flux$Date))    
    treatDF$year_end[treatDF$terms == "Coarse Root C flux"] <- max(year(coarse_root_c_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Coarse Root C flux"] <- length(unique(coarse_root_c_flux$Date))  
    treatDF$notes[treatDF$terms == "Coarse Root C flux"] <- "Allometric rlt with DBH"
    
    ### Understorey C flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Understorey C flux", i+1] <- with(understorey_c_flux[understorey_c_flux$Ring ==i,],
                                                                    sum(understorey_production_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Understorey C flux"] <- min(year(understorey_c_flux$Date))    
    treatDF$year_end[treatDF$terms == "Understorey C flux"] <- max(year(understorey_c_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey C flux"] <- length(unique(understorey_c_flux$Date))  
    treatDF$notes[treatDF$terms == "Understorey C flux"] <- "Harvest data"
    
    
    ### Understorey litter C flux
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Understorey Litter C flux", i+1] <- with(understorey_litter_c_flux[understorey_litter_c_flux$Ring ==i,],
                                                                  sum(understorey_litter_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Understorey Litter C flux"] <- min(year(understorey_litter_c_flux$Date))    
    treatDF$year_end[treatDF$terms == "Understorey Litter C flux"] <- max(year(understorey_litter_c_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey Litter C flux"] <- length(unique(understorey_litter_c_flux$Date))  
    treatDF$notes[treatDF$terms == "Understorey Litter C flux"] <- "50% retrans"
    
    ### Frass production flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Frass C flux", i+1] <- with(frass_c_production_flux[frass_c_production_flux$Ring ==i,],
                                                                    sum(frass_production_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Frass C flux"] <- min(year(frass_c_production_flux$Date))    
    treatDF$year_end[treatDF$terms == "Frass C flux"] <- max(year(frass_c_production_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Frass C flux"] <- length(unique(frass_c_production_flux$Date))  
    treatDF$notes[treatDF$terms == "Frass C flux"] <- "NA"
    
    ### Leaf litter flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Leaflitter C flux", i+1] <- with(leaflitter_c_production_flux[leaflitter_c_production_flux$Ring ==i,],
                                                              sum(leaf_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Leaflitter C flux"] <- min(year(leaflitter_c_production_flux$Date))    
    treatDF$year_end[treatDF$terms == "Leaflitter C flux"] <- max(year(leaflitter_c_production_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Leaflitter C flux"] <- length(unique(leaflitter_c_production_flux$Date))  
    treatDF$notes[treatDF$terms == "Leaflitter C flux"] <- "Only leaves, exclude twig, barks and seeds"
    
    ### Fine Root litter flux
    # assume it's the same as fine root production flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fineroot Litter C flux", i+1] <- with(fineroot_c_production_flux[fineroot_c_production_flux$Ring ==i,],
                                                                  sum(fineroot_production_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Fineroot Litter C flux"] <- min(year(fineroot_c_production_flux$Date))    
    treatDF$year_end[treatDF$terms == "Fineroot Litter C flux"] <- max(year(fineroot_c_production_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Fineroot Litter C flux"] <- length(unique(fineroot_c_production_flux$Date))  
    treatDF$notes[treatDF$terms == "Fineroot Litter C flux"] <- "Assuming fineroot production = fineroot litter production"
    
    
    
    ### Twig litter flux
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Twig C flux", i+1] <- with(twiglitter_c_production_flux[twiglitter_c_production_flux$Ring ==i,],
                                                                 sum(twig_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Twig C flux"] <- min(year(twiglitter_c_production_flux$Date))    
    treatDF$year_end[treatDF$terms == "Twig C flux"] <- max(year(twiglitter_c_production_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Twig C flux"] <- length(unique(twiglitter_c_production_flux$Date))  
    treatDF$notes[treatDF$terms == "Twig C flux"] <- "twig litter"
    
    ### Bark litter flux
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Bark C flux", i+1] <- with(barklitter_c_production_flux[barklitter_c_production_flux$Ring ==i,],
                                                           sum(bark_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Bark C flux"] <- min(year(barklitter_c_production_flux$Date))    
    treatDF$year_end[treatDF$terms == "Bark C flux"] <- max(year(barklitter_c_production_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Bark C flux"] <- length(unique(barklitter_c_production_flux$Date))  
    treatDF$notes[treatDF$terms == "Bark C flux"] <- "bark litter"
    
    
    ### Seed litter flux
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Seed C flux", i+1] <- with(seedlitter_c_production_flux[seedlitter_c_production_flux$Ring ==i,],
                                                           sum(seed_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Seed C flux"] <- min(year(seedlitter_c_production_flux$Date))    
    treatDF$year_end[treatDF$terms == "Seed C flux"] <- max(year(seedlitter_c_production_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Seed C flux"] <- length(unique(seedlitter_c_production_flux$Date))  
    treatDF$notes[treatDF$terms == "Seed C flux"] <- "twig litter"
    
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

