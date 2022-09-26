
#### To make EucFACE C summary table by CO2 treatment
#### Ignore time but produce time coverage information
#### This is for pools

make_c_pool_summary_table <- function(norm,
                                      canopy_c_pool,
                                      wood_c_pool,
                                      fineroot_c_pool,
                                      coarse_root_c_pool,
                                      understorey_c_pool,
                                      soil_c_pool,
                                      microbial_c_pool,
                                      leaflitter_c_pool) {
  
  ### Define pool variable names
  terms <- c("Canopy C Pool", 
             "Wood C Pool", 
             "Sapwood C Pool", 
             "Heartwood C Pool",
             "Fine Root C Pool",
             "Coarse Root C Pool", 
             "Understorey C Pool", 
             "Microbial C Pool 0-10cm",
             "Microbial C Pool 10-30cm",
             "Microbial C Pool 30-60cm",
             "Microbial C Pool",
             "Leaflitter C Pool",
             "Soil C Pool 0-10cm",
             "Soil C Pool 10-30cm",
             "Soil C Pool 30-60cm",
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
  out <- summaryBy(leaf_pool~Ring,data=canopy_c_pool,FUN=mean,keep.names=T,na.rm=T)
  treatDF[treatDF$terms == "Canopy C Pool", 2:7] <- out$leaf_pool
  treatDF$year_start[treatDF$terms == "Canopy C Pool"] <- min(year(canopy_c_pool$Date))    
  treatDF$year_end[treatDF$terms == "Canopy C Pool"] <- max(year(canopy_c_pool$Date))    
  treatDF$timepoint[treatDF$terms == "Canopy C Pool"] <- length(unique(canopy_c_pool$Date))  
  treatDF$notes[treatDF$terms == "Canopy C Pool"] <- "Estimated based on LAI and SLA"
  
  ### Leaflitter C 
  out <- summaryBy(leaflitter_pool~Ring,data=leaflitter_c_pool,FUN=mean,keep.names=T,na.rm=T)
  treatDF[treatDF$terms == "Leaflitter C Pool", 2:7] <- out$leaflitter_pool
  treatDF$year_start[treatDF$terms == "Leaflitter C Pool"] <- min(year(leaflitter_c_pool$Date))    
  treatDF$year_end[treatDF$terms == "Leaflitter C Pool"] <- max(year(leaflitter_c_pool$Date))    
  treatDF$timepoint[treatDF$terms == "Leaflitter C Pool"] <- length(unique(leaflitter_c_pool$Date))  
  treatDF$notes[treatDF$terms == "Leaflitter C Pool"] <- "decomposition"
  
  
  ### Wood C 
  out <- summaryBy(wood_pool~Ring,data=wood_c_pool,FUN=mean,keep.names=T,na.rm=T)
  treatDF[treatDF$terms == "Wood C Pool", 2:7] <- out$wood_pool
  treatDF$year_start[treatDF$terms == "Wood C Pool"] <- min(year(wood_c_pool$Date))    
  treatDF$year_end[treatDF$terms == "Wood C Pool"] <- max(year(wood_c_pool$Date))    
  treatDF$timepoint[treatDF$terms == "Wood C Pool"] <- length(unique(wood_c_pool$Date)) 
  treatDF$notes[treatDF$terms == "Wood C Pool"] <- "Estimated based on allometric relationship, considers mortality"
  
  if (norm == "unnormalized") {
    ### Sapwood C 
    out <- summaryBy(sap_pool~Ring,data=wood_c_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Sapwood C Pool", 2:7] <- out$sap_pool
    treatDF$year_start[treatDF$terms == "Sapwood C Pool"] <- min(year(wood_c_pool$Date))    
    treatDF$year_end[treatDF$terms == "Sapwood C Pool"] <- max(year(wood_c_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Sapwood C Pool"] <- length(unique(wood_c_pool$Date)) 
    treatDF$notes[treatDF$terms == "Sapwood C Pool"] <- "Estimated based on allometric relationship, considers mortality"
    
    ### Heartwood C 
    out <- summaryBy(heart_pool~Ring,data=wood_c_pool,FUN=mean,keep.names=T,na.rm=T)
    treatDF[treatDF$terms == "Heartwood C Pool", 2:7] <- out$heart_pool
    treatDF$year_start[treatDF$terms == "Heartwood C Pool"] <- min(year(wood_c_pool$Date))    
    treatDF$year_end[treatDF$terms == "Heartwood C Pool"] <- max(year(wood_c_pool$Date))    
    treatDF$timepoint[treatDF$terms == "Heartwood C Pool"] <- length(unique(wood_c_pool$Date)) 
    treatDF$notes[treatDF$terms == "Heartwood C Pool"] <- "Estimated based on allometric relationship, considers mortality"
    
  }
  

  
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
  out1 <- summaryBy(Cmic_g_m2~Ring+Depth,data=microbial_c_pool,FUN=mean,keep.names=T,na.rm=T)
  out2 <- summaryBy(Cmic_g_m2~Ring,data=out1,FUN=sum,keep.names=T,na.rm=T)
  
  treatDF[treatDF$terms == "Microbial C Pool", 2:7] <- out2$Cmic_g_m2
  treatDF$year_start[treatDF$terms == "Microbial C Pool"] <- min(year(microbial_c_pool$Date))    
  treatDF$year_end[treatDF$terms == "Microbial C Pool"] <- max(year(microbial_c_pool$Date))    
  treatDF$timepoint[treatDF$terms == "Microbial C Pool"] <- length(unique(microbial_c_pool$Date))  
  treatDF$notes[treatDF$terms == "Microbial C Pool"] <- "all depth"
  
  
  microbial_c_pool1 <- subset(microbial_c_pool, Depth=="0_10")
  out1 <- summaryBy(Cmic_g_m2~Ring,data=microbial_c_pool1,FUN=mean,keep.names=T,na.rm=T)
  
  treatDF[treatDF$terms == "Microbial C Pool 0-10cm", 2:7] <- out1$Cmic_g_m2
  treatDF$year_start[treatDF$terms == "Microbial C Pool 0-10cm"] <- min(year(microbial_c_pool1$Date))    
  treatDF$year_end[treatDF$terms == "Microbial C Pool 0-10cm"] <- max(year(microbial_c_pool1$Date))    
  treatDF$timepoint[treatDF$terms == "Microbial C Pool 0-10cm"] <- length(unique(microbial_c_pool1$Date))  
  treatDF$notes[treatDF$terms == "Microbial C Pool 0-10cm"] <- ""
  
  
  microbial_c_pool1 <- subset(microbial_c_pool, Depth=="10_30")
  out1 <- summaryBy(Cmic_g_m2~Ring,data=microbial_c_pool1,FUN=mean,keep.names=T,na.rm=T)
  
  treatDF[treatDF$terms == "Microbial C Pool 10-30cm", 2:7] <- out1$Cmic_g_m2
  treatDF$year_start[treatDF$terms == "Microbial C Pool 10-30cm"] <- min(year(microbial_c_pool1$Date))    
  treatDF$year_end[treatDF$terms == "Microbial C Pool 10-30cm"] <- max(year(microbial_c_pool1$Date))    
  treatDF$timepoint[treatDF$terms == "Microbial C Pool 10-30cm"] <- length(unique(microbial_c_pool1$Date))  
  treatDF$notes[treatDF$terms == "Microbial C Pool 10-30cm"] <- ""
  
  
  
  microbial_c_pool1 <- subset(microbial_c_pool, Depth=="transition")
  out1 <- summaryBy(Cmic_g_m2~Ring,data=microbial_c_pool1,FUN=mean,keep.names=T,na.rm=T)
  
  treatDF[treatDF$terms == "Microbial C Pool 30_60cm", 2:7] <- out1$Cmic_g_m2
  treatDF$year_start[treatDF$terms == "Microbial C Pool 30_60cm"] <- min(year(microbial_c_pool1$Date))    
  treatDF$year_end[treatDF$terms == "Microbial C Pool 30_60cm"] <- max(year(microbial_c_pool1$Date))    
  treatDF$timepoint[treatDF$terms == "Microbial C Pool 30_60cm"] <- length(unique(microbial_c_pool1$Date))  
  treatDF$notes[treatDF$terms == "Microbial C Pool 30_60cm"] <- ""
  
  
  ### Soil C pool
  out1 <- summaryBy(soil_carbon_pool~Ring+Depth,data=soil_c_pool,FUN=mean,keep.names=T,na.rm=T)
  out2 <- summaryBy(soil_carbon_pool~Ring,data=out1,FUN=sum,keep.names=T,na.rm=T)
  
  treatDF[treatDF$terms == "Soil C Pool", 2:7] <- out2$soil_carbon_pool
  treatDF$year_start[treatDF$terms == "Soil C Pool"] <- min(year(soil_c_pool$Date))    
  treatDF$year_end[treatDF$terms == "Soil C Pool"] <- max(year(soil_c_pool$Date))    
  treatDF$timepoint[treatDF$terms == "Soil C Pool"] <- length(unique(soil_c_pool$Date))  
  treatDF$notes[treatDF$terms == "Soil C Pool"] <- "Averaged across all C forms"
  
  
  soil_c_pool1 <- subset(soil_c_pool, Depth=="0_10")
  out2 <- summaryBy(soil_carbon_pool~Ring,data=soil_c_pool1,FUN=mean,keep.names=T,na.rm=T)
  
  treatDF[treatDF$terms == "Soil C Pool 0-10cm", 2:7] <- out2$soil_carbon_pool
  treatDF$year_start[treatDF$terms == "Soil C Pool 0-10cm"] <- min(year(soil_c_pool1$Date))    
  treatDF$year_end[treatDF$terms == "Soil C Pool 0-10cm"] <- max(year(soil_c_pool1$Date))    
  treatDF$timepoint[treatDF$terms == "Soil C Pool 0-10cm"] <- length(unique(soil_c_pool1$Date))  
  treatDF$notes[treatDF$terms == "Soil C Pool 0-10cm"] <- ""
  
  
  soil_c_pool1 <- subset(soil_c_pool, Depth=="10_30")
  out2 <- summaryBy(soil_carbon_pool~Ring,data=soil_c_pool1,FUN=mean,keep.names=T,na.rm=T)
  
  treatDF[treatDF$terms == "Soil C Pool 10-30cm", 2:7] <- out2$soil_carbon_pool
  treatDF$year_start[treatDF$terms == "Soil C Pool 10-30cm"] <- min(year(soil_c_pool1$Date))    
  treatDF$year_end[treatDF$terms == "Soil C Pool 10-30cm"] <- max(year(soil_c_pool1$Date))    
  treatDF$timepoint[treatDF$terms == "Soil C Pool 10-30cm"] <- length(unique(soil_c_pool1$Date))  
  treatDF$notes[treatDF$terms == "Soil C Pool 10-30cm"] <- ""
  
  
  soil_c_pool1 <- subset(soil_c_pool, Depth=="transition")
  out2 <- summaryBy(soil_carbon_pool~Ring,data=soil_c_pool1,FUN=mean,keep.names=T,na.rm=T)
  
  treatDF[treatDF$terms == "Soil C Pool 30-60cm", 2:7] <- out2$soil_carbon_pool
  treatDF$year_start[treatDF$terms == "Soil C Pool 30-60cm"] <- min(year(soil_c_pool1$Date))    
  treatDF$year_end[treatDF$terms == "Soil C Pool 30-60cm"] <- max(year(soil_c_pool1$Date))    
  treatDF$timepoint[treatDF$terms == "Soil C Pool 30-60cm"] <- length(unique(soil_c_pool1$Date))  
  treatDF$notes[treatDF$terms == "Soil C Pool 30-60cm"] <- ""

  
  ### calculate treatment averages
  treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 5)
  treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 5)
  
  treatDF$aCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R2, R3, R6))), na.rm=T)
  treatDF$eCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R1, R4, R5))), na.rm=T)
  
  ###### Diff (eCO2 - aCO2)
  treatDF$diff <- round(treatDF$eCO2 - treatDF$aCO2, 4)
  
  ###### percent differences (eCO2 - aCO2) / aCO2 * 100
  treatDF$percent_diff <- round((treatDF$eCO2 - treatDF$aCO2) / (treatDF$aCO2) * 100, 2)
  
  write.csv(treatDF, 
            paste0("plots_tables/summary_tables/", norm, 
                   "/summary_table_C_pool_", norm, ".csv"), row.names=F)
  
  
  ### plot
  #tmpDF1 <- treatDF[,c("terms", "aCO2", "eCO2")]
  #tmpDF2 <- treatDF[,c("terms", "aCO2_sd", "eCO2_sd")]
  #
  #plotDF1 <- reshape::melt(tmpDF1, id.var="terms")
  #plotDF2 <- reshape::melt(tmpDF2, id.var="terms")
  #colnames(plotDF2) <- c("terms", "variable", "value_sd")
  #plotDF2$variable <- gsub("_sd", "", plotDF2$variable)
  #
  #plotDF <- merge(plotDF1, plotDF2, by=c("terms", "variable"))
  #plotDF$terms <- gsub(" C Pool", "", plotDF$terms)
  #
  #p1 <- ggplot(plotDF, aes(terms, value, group=variable))+
  #  geom_errorbar(aes(ymin=value-value_sd, ymax=value+value_sd),
  #                position=position_dodge2(), width=0.3)+
  #  geom_point(aes(fill=variable), pch=21, stat = "identity", size=2,
  #             position=position_dodge2(width=0.3)) +
  #  coord_flip()+
  #  scale_fill_manual(name="",
  #                    values=c("aCO2"="blue3",
  #                             "eCO2"="red2"))
  #
  #pdf(paste0("plots_tables/summary_tables/", norm, "/C_pool_comparison.pdf"))
  #plot(p1)
  #dev.off()
  
  ##### output tables
  return(treatDF)
  
}
