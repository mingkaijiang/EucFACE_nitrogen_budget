
#### To make EucFACE P summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for fluxes

make_flux_summary_table_by_treatment <- function() {
    
    ### convert daily flux in mg P m2 d-1 to g P m-2 yr-1
    conv <- 365 / 1000
    
    ### Define production variable names
    terms <- c("Wood P flux", "Canopy P flux", "Fine Root P flux",
               "Coarse Root P flux","Leaflitter P flux", "Fineroot Litter P flux",
               "Twig litter P flux", "Bark litter P flux","Seed litter P flux", "Frass P flux",
               "Understorey P flux", "Understorey Litter P flux", "Mineralization P flux",
               "Leaching P flux")
    
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
    
    ### Canopy P flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Canopy P flux", i+1] <- with(canopy_p_flux[canopy_p_flux$Ring ==i,],
                                                              sum(canopy_p_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Canopy P flux"] <- min(year(canopy_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Canopy P flux"] <- max(year(canopy_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Canopy P flux"] <- length(unique(canopy_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Canopy P flux"] <- "need to consider leaf turnover"

    
    ### Wood P 
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Wood P flux", i+1] <- with(wood_p_flux[wood_p_flux$Ring ==i,],
                                                             sum(wood_p_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Wood P flux"] <- min(year(wood_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Wood P flux"] <- max(year(wood_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Wood P flux"] <- length(unique(wood_p_flux$Date)) 
    treatDF$notes[treatDF$terms == "Wood P flux"] <- "Based on single time point measurement"
    
    ### Fine root P flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fine Root P flux", i+1] <- with(fineroot_p_production[fineroot_p_production$Ring ==i,],
                                                             sum(fineroot_p_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Fine Root P flux"] <- min(year(fineroot_p_production$Date))    
    treatDF$year_end[treatDF$terms == "Fine Root P flux"] <- max(year(fineroot_p_production$Date))    
    treatDF$timepoint[treatDF$terms == "Fine Root P flux"] <- length(unique(fineroot_p_production$Date))  
    treatDF$notes[treatDF$terms == "Fine Root P flux"] <- "Top 30 cm"
    
    ### Coarse root P flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Coarse Root P flux", i+1] <- with(coarse_root_p_flux[coarse_root_p_flux$Ring ==i,],
                                                                  sum(coarse_root_p_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Coarse Root P flux"] <- min(year(coarse_root_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Coarse Root P flux"] <- max(year(coarse_root_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Coarse Root P flux"] <- length(unique(coarse_root_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Coarse Root P flux"] <- "Allometric rlt with DBH"
    
    ### Understorey P flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Understorey P flux", i+1] <- with(understorey_p_flux[understorey_p_flux$Ring ==i,],
                                                                    sum(understorey_p_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Understorey P flux"] <- min(year(understorey_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Understorey P flux"] <- max(year(understorey_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey P flux"] <- length(unique(understorey_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Understorey P flux"] <- "Used Varsha's harvest data"
    
    ### Understorey Litter P flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Understorey Litter P flux", i+1] <- with(understorey_litter_p_flux[understorey_litter_p_flux$Ring ==i,],
                                                                    sum(understorey_litter_p_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Understorey Litter P flux"] <- min(year(understorey_litter_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Understorey Litter P flux"] <- max(year(understorey_litter_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Understorey Litter P flux"] <- length(unique(understorey_litter_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Understorey Litter P flux"] <- "Used Varsha's harvest data"
    
    ### Frass production flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Frass P flux", i+1] <- with(frass_p_production[frass_p_production$Ring ==i,],
                                                                    sum(frass_p_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Frass P flux"] <- min(year(frass_p_production$Date))    
    treatDF$year_end[treatDF$terms == "Frass P flux"] <- max(year(frass_p_production$Date))    
    treatDF$timepoint[treatDF$terms == "Frass P flux"] <- length(unique(frass_p_production$Date))  
    treatDF$notes[treatDF$terms == "Frass P flux"] <- "NA"
    
    ### Leaf litter flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Leaflitter P flux", i+1] <- with(leaflitter_p_flux[leaflitter_p_flux$Ring ==i,],
                                                              sum(leaflitter_p_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Leaflitter P flux"] <- min(year(leaflitter_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Leaflitter P flux"] <- max(year(leaflitter_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Leaflitter P flux"] <- length(unique(leaflitter_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Leaflitter P flux"] <- "Only leaves, exclude twig, barks and seeds"
    
    ### Fine Root litter flux
    # assume it's the same as fine root production flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Fineroot Litter P flux", i+1] <- with(fineroot_litter_p_flux[fineroot_litter_p_flux$Ring ==i,],
                                                                  sum(fineroot_litter_p_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Fineroot Litter P flux"] <- min(year(fineroot_litter_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Fineroot Litter P flux"] <- max(year(fineroot_litter_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Fineroot Litter P flux"] <- length(unique(fineroot_litter_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Fineroot Litter P flux"] <- "Assuming fineroot production = fineroot litter production"
    
    
    ### twig litter flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Twig litter P flux", i+1] <- with(twig_litter_p_flux[twig_litter_p_flux$Ring ==i,],
                                                                   sum(twiglitter_p_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Twig litter P flux"] <- min(year(twig_litter_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Twig litter P flux"] <- max(year(twig_litter_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Twig litter P flux"] <- length(unique(twig_litter_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Twig litter P flux"] <- "Assume wood P concentration"
    
    ### bark litter flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Bark litter P flux", i+1] <- with(bark_litter_p_flux[bark_litter_p_flux$Ring ==i,],
                                                                     sum(barklitter_p_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Bark litter P flux"] <- min(year(bark_litter_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Bark litter P flux"] <- max(year(bark_litter_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Bark litter P flux"] <- length(unique(bark_litter_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Bark litter P flux"] <- "Assume wood P concentration"
    
    ### seed litter flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Seed litter P flux", i+1] <- with(seed_litter_p_flux[seed_litter_p_flux$Ring ==i,],
                                                                     sum(seedlitter_p_flux_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Seed litter P flux"] <- min(year(seed_litter_p_flux$Date))    
    treatDF$year_end[treatDF$terms == "Seed litter P flux"] <- max(year(seed_litter_p_flux$Date))    
    treatDF$timepoint[treatDF$terms == "Seed litter P flux"] <- length(unique(seed_litter_p_flux$Date))  
    treatDF$notes[treatDF$terms == "Seed litter P flux"] <- "Assume wood P concentration"
    
    ###  P mineralization flux
    for (i in c(1:6)) {
        treatDF[treatDF$terms == "Mineralization P flux", i+1] <- with(soil_p_mineralization[soil_p_mineralization$Ring ==i,],
                                                                    sum(p_mineralization_mg_m2_d*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Mineralization P flux"] <- min(year(soil_p_mineralization$Date))    
    treatDF$year_end[treatDF$terms == "Mineralization P flux"] <- max(year(soil_p_mineralization$Date))    
    treatDF$timepoint[treatDF$terms == "Mineralization P flux"] <- length(unique(soil_p_mineralization$Date))  
    treatDF$notes[treatDF$terms == "Mineralization P flux"] <- "obtained"
    
    ###  P leaching flux
    for (i in c(1:6)) {
      treatDF[treatDF$terms == "Leaching P flux", i+1] <- with(soil_p_leaching[soil_p_leaching$Ring ==i,],
                                                                     sum(phosphate_leaching_flux*Days)/sum(Days)) * conv
    }
    treatDF$year_start[treatDF$terms == "Leaching P flux"] <- min(year(soil_p_leaching$Date))    
    treatDF$year_end[treatDF$terms == "Leaching P flux"] <- max(year(soil_p_leaching$Date))    
    treatDF$timepoint[treatDF$terms == "Leaching P flux"] <- length(unique(soil_p_leaching$Date))  
    treatDF$notes[treatDF$terms == "Leaching P flux"] <- "drainage 20 ml/d"
    
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

