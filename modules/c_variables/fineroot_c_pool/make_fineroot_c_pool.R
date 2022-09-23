make_fineroot_c_pool <- function(back.calculate,
                                 soil_bulk_density,
                                 root.size){
    
    
    ### there are three datasets with different structures, dates and fineroot data
    ### Most importantly, one dataset contains the depth profile,
    ### and one contains the different root size profile.
    ### The fineroot (< 2mm in diameter) is tiny compared to the intermediate size (2-3mm),
    ### so it's important to include the intermediate root
    
    ### download data
    download_fineroot_c_data()
    
    ### depth profile
    dDF <- read.csv("download/FACE_PILOTROOT_RA_FINEROOTS_L1_20131201-20131231.csv")
    colnames(dDF) <- c("Number", "RingID", "Replicate1", "Replicate2", "Depth", "Date",
                       "BulkDensity", "SoilpH", "Moisture", "FRB", "Cpercent", "FRB_0_50",
                       "Abv_biomass", "Root_shoot", "DistanceNT", "DBH_of_NT")
    dDF$Date <- as.Date(as.character(paste0("01-", dDF$Date)), "%d-%b-%y")
    
    dDF <- dDF[,c("Number", "RingID", "Depth", "Date", "FRB", "Cpercent", "BulkDensity")]
    dDF$Depth <- gsub("Oct-20", "10 - 20", dDF$Depth)
    dDF$RingID[160] <- "R4"
    
    names(dDF)[names(dDF)=="RingID"] <- "Ring"
    dDF$Ring <- gsub("R", "", dDF$Ring)
    dDF$Ring <- as.numeric(dDF$Ring)
    
    ### calculate FRB based on revised bulk density
    if (back.calculate==T) {
        tmpDF <- dDF
        tmpDF$FRB1 <- ifelse(tmpDF$Depth%in%c("0 - 10", "10 - 20", "20 - 30"),
                             tmpDF$FRB/0.1, 
                             tmpDF$FRB/0.2)
        
        tmpDF$FRB2 <- tmpDF$FRB1 / tmpDF$BulkDensity
        
        ### add new bulk density
        for (i in c(1:6)) {
            tmpDF$BDrev[tmpDF$Ring==i&tmpDF$Depth=="0 - 10"] <- soil_bulk_density$bulk_density_kg_m3[soil_bulk_density$Ring==i&soil_bulk_density$Depth=="0_10"]
            tmpDF$BDrev[tmpDF$Ring==i&tmpDF$Depth=="10 - 20"] <- soil_bulk_density$bulk_density_kg_m3[soil_bulk_density$Ring==i&soil_bulk_density$Depth=="10_30"]
            tmpDF$BDrev[tmpDF$Ring==i&tmpDF$Depth=="20 - 30"] <- soil_bulk_density$bulk_density_kg_m3[soil_bulk_density$Ring==i&soil_bulk_density$Depth=="10_30"]
            tmpDF$BDrev[tmpDF$Ring==i&tmpDF$Depth=="30 - 50"] <- soil_bulk_density$bulk_density_kg_m3[soil_bulk_density$Ring==i&soil_bulk_density$Depth=="transition"]
        }
        
        tmpDF$BDrev <- tmpDF$BDrev / 1000.0
        
        ### recalculate FRB
        tmpDF$FRB3 <- tmpDF$FRB2 * tmpDF$BDrev
        
        ### last to consider depth
        tmpDF$FRB4 <- ifelse(tmpDF$Depth%in%c("0 - 10", "10 - 20", "20 - 30"),
                             tmpDF$FRB3*0.1, 
                             tmpDF$FRB3*0.2)
        
        ### prepare out
        dDF <- tmpDF[,c("Number", "Ring", "Depth", "Date", "FRB4", "Cpercent")]
        names(dDF)[names(dDF)=="FRB4"] <- "FRB"
        
    }
    
    ### convert from fresh to dry
    dDF$FRB <- dDF$FRB * 0.5
    
    
    ### estimate 50-60 profile, assuming 1/24
    dDF2 <- dDF[dDF$Depth == "30 - 50",]
    dDF2$Depth <- "50 - 60"
    dDF2$FRB <- dDF2$FRB / 2
    
    ## merge
    myDF <- rbind(dDF, dDF2)
    
    ## calculate C content
    myDF$FRC <- myDF$FRB * myDF$Cpercent / 100.0
    
    ### re-categorize, 0-10, 10-30, 30-60
    reDF1 <- myDF[myDF$Depth == "0 - 10", c("Number", "Ring", "Date", "FRC", "Depth")]
    
    reDF1$Depth <- gsub("0 - 10", "0_10", reDF1$Depth)
    
    reDF2 <- myDF[myDF$Depth %in%c("10 - 20", "20 - 30"), ]
    reDF3 <- myDF[myDF$Depth %in%c("30 - 50", "50 - 60"), ]
    
    ### calculate total for 10 - 30
    reDF4 <- summaryBy(FRC~Number+Ring+Date, FUN=sum,
                       data=reDF2, na.rm=T, keep.names=T)
    
    reDF4$Depth <- "10_30"
    
    reDF5 <- summaryBy(FRC~Number+Ring+Date, FUN=sum,
                       data=reDF3, na.rm=T, keep.names=T)
    
    reDF5$Depth <- "transition"
    
    depthDF <- rbind(reDF1, rbind(reDF4, reDF5))
    
    ### calculate total C
    totDF <- summaryBy(FRC~Number+Ring+Date, FUN=sum, 
                       data=depthDF, na.rm=T, keep.names=T)
    colnames(totDF) <- c("Number", "Ring", "Date", "FRC_0_60")
    
    depthDF <- merge(depthDF, totDF, by=c("Number", "Ring", "Date"))
    
    depthDF$pct <- depthDF$FRC / depthDF$FRC_0_60
    
    sumDF <- summaryBy(pct~Ring+Depth, FUN=c(mean,sd),
                       data=depthDF, keep.names=T, na.rm=T)
    
    transDF <- sumDF[sumDF$Depth=="transition",]
    
    
    
    # read in the csv
    frb1 <- read.csv("temp_files/EucFACERootsRingDateDepth.csv")
    frb1$Date <- as.Date(frb1$Dateform, format="%d-%m-%Y")
    
    ### back calculate based on bulk density
    if (back.calculate==T) {
        
        ### assign a temporary DF
        tmpDF <- frb1
        
        ### removing depth
        tmpDF$FRB_0.10cm2 <- tmpDF$FRB_0.10cm / 0.1
        tmpDF$FRB_10.30cm2 <- tmpDF$FRB_10.30cm / 0.2
        
        ### back calculating biomass in g biomass g-1 soil
        tmpDF$FRB_0.10cm3 <- tmpDF$FRB_0.10cm2 / 1.39
        tmpDF$FRB_10.30cm3 <- tmpDF$FRB_10.30cm2 / 1.39
        
        ### assign new bulk density
        for (i in 1:6) {
            tmpDF$BK0_10[tmpDF$Ring==i] <- soil_bulk_density$bulk_density_kg_m3[soil_bulk_density$Ring==i&soil_bulk_density$Depth=="0_10"]
            tmpDF$BK10_30[tmpDF$Ring==i] <- soil_bulk_density$bulk_density_kg_m3[soil_bulk_density$Ring==i&soil_bulk_density$Depth=="10_30"]
            
        }
        
        ### calculate new frb
        tmpDF$FRB_0.10cm4 <- tmpDF$FRB_0.10cm3 * tmpDF$BK0_10 /1000.0 * 0.1
        tmpDF$FRB_10.30cm4 <- tmpDF$FRB_10.30cm3 * tmpDF$BK10_30 /1000.0 * 0.2
        
        ### prepare output
        frb1 <- tmpDF[,c("CO2", "Ring", "Date", "FRB_0.10cm4", "FRB_10.30cm4",
                         "C0_0.10cm", "C0_10.30cm")]
        names(frb1)[names(frb1)=="FRB_0.10cm4"] <- "FRB_0.10cm"
        names(frb1)[names(frb1)=="FRB_10.30cm4"] <- "FRB_10.30cm"
        
        
    }
    
    
    # fineroot C pool
    frb1$frb_top <- with(frb1, FRB_0.10cm * C0_0.10cm / 100)
    frb1$frb_bot <- with(frb1, FRB_10.30cm * C0_10.30cm / 100)
    frb1$frb_tot <- with(frb1, frb_top + frb_bot)
    
    # average across rings and dates
    outDF <- summaryBy(frb_tot+frb_top+frb_bot~Date+Ring,
                       data=frb1,FUN=mean,keep.names=T)
    
    ## colnames
    colnames(outDF) <- c("Date","Ring","fineroot_pool_0_30cm", 
                         "fineroot_0_10_cm", "fineroot_10_30_cm")
    
    outDF <- merge(outDF, transDF, by=c("Ring"))
    
    ### calculate total and 30-60cm
    outDF$fineroot_pool <- with(outDF, fineroot_pool_0_30cm/(1-pct.mean))
    outDF$fineroot_30_60_cm <- with(outDF, fineroot_pool-fineroot_pool_0_30cm)
    
    
    ### clean
    outDF <- outDF[,c("Ring", "Date", "fineroot_pool", 
                      "fineroot_0_10_cm",
                      "fineroot_10_30_cm", 
                      "fineroot_30_60_cm")]
    
    
    ### add the 2013 data
    depthDF <- depthDF[,c("Number", "Ring", "Depth", "Date", "FRC")]
    
    
    sumDF <- summaryBy(FRC~Ring+Depth+Date, FUN=mean,
                       data=depthDF, na.rm=T, keep.names=T)
    
    outDF2 <- dcast(setDT(sumDF), Ring~Depth, value.var="FRC")
    outDF2 <- as.data.frame(outDF2)
    colnames(outDF2) <- c("Ring", "fineroot_0_10_cm", "fineroot_10_30_cm",
                          "fineroot_30_60_cm")
    outDF2$Date <- "2013-12-01"
    outDF2$fineroot_pool <- outDF2$fineroot_0_10_cm + outDF2$fineroot_10_30_cm + outDF2$fineroot_30_60_cm
    outDF2 <- outDF2[,c("Ring", "Date", "fineroot_pool",
                        "fineroot_0_10_cm", "fineroot_10_30_cm",
                        "fineroot_30_60_cm")]
    
    out <- rbind(outDF, outDF2)
    
    ### order
    out <- out[order(out$Ring, out$Date),]
    
    
    
    ### the most recent dataset contains root size information
    newDF <- estimate_fineroot_fractions(bkDF=soil_bulk_density)
    
    ### get fineroot first
    subDF1 <- newDF[,c("Date", "Ring", "Depth", "FRB_small")]
    
    ### convert from long to wide
    outDF1 <- tidyr::spread(subDF1, Depth, FRB_small)
    colnames(outDF1) <- c("Date", "Ring", "fineroot_0_10_cm", "fineroot_10_30_cm",
                          "fineroot_30_60_cm")
    outDF1$fineroot_pool <- rowSums(data.frame(outDF1$fineroot_0_10_cm, outDF1$fineroot_10_30_cm,
                                               outDF1$fineroot_30_60_cm))
    
    outDF1 <- outDF1[,c("Date", "Ring", "fineroot_pool",
                        "fineroot_0_10_cm", "fineroot_10_30_cm",
                        "fineroot_30_60_cm")]
    
    ### combine all data together
    out <- rbind(out, outDF1)
    
    
    ### check root size profile
    if (root.size == "small") {
        outDF <- out
    } else if (root.size == "intermediate") {
        
        ### get the fraction data
        subDF2 <- newDF[,c("Ring", "Depth", "frac_intermediate",
                           "frac_large")]
        
        for (i in 1:6) {
            out$intermediateroot_0_10_cm[out$Ring==i] <- out$fineroot_0_10_cm[out$Ring==i] * subDF2$frac_intermediate[subDF2$Ring==i&subDF2$Depth=="0_10"]
            out$intermediateroot_10_30_cm[out$Ring==i] <- out$fineroot_10_30_cm[out$Ring==i] * subDF2$frac_intermediate[subDF2$Ring==i&subDF2$Depth=="10_30"]
            out$intermediateroot_30_60_cm[out$Ring==i] <- out$fineroot_30_60_cm[out$Ring==i] * subDF2$frac_intermediate[subDF2$Ring==i&subDF2$Depth=="transition"]
            
            out$largeroot_0_10_cm[out$Ring==i] <- out$fineroot_0_10_cm[out$Ring==i] * subDF2$frac_large[subDF2$Ring==i&subDF2$Depth=="0_10"]
            out$largeroot_10_30_cm[out$Ring==i] <- out$fineroot_10_30_cm[out$Ring==i] * subDF2$frac_large[subDF2$Ring==i&subDF2$Depth=="10_30"]
            out$largeroot_30_60_cm[out$Ring==i] <- out$fineroot_30_60_cm[out$Ring==i] * subDF2$frac_large[subDF2$Ring==i&subDF2$Depth=="transition"]
        }
        
        
        ### update the original variables
        out$fineroot_0_10_cm <- out$fineroot_0_10_cm + out$intermediateroot_0_10_cm
        out$fineroot_10_30_cm <- out$fineroot_10_30_cm + out$intermediateroot_10_30_cm
        out$fineroot_30_60_cm <- out$fineroot_30_60_cm + out$intermediateroot_30_60_cm
        
        out$fineroot_pool <- rowSums(data.frame(out$fineroot_0_10_cm, out$fineroot_10_30_cm,
                                                out$fineroot_30_60_cm))
        
        outDF <- out[,c("Ring", "Date", "fineroot_pool", "fineroot_0_10_cm",
                        "fineroot_10_30_cm", "fineroot_30_60_cm")]
        
        
    } else if (root.size == "large") {
        
        ### get the fraction data
        subDF2 <- newDF[,c("Ring", "Depth", "frac_intermediate",
                           "frac_large")]
        
        for (i in 1:6) {
            out$intermediateroot_0_10_cm[out$Ring==i] <- out$fineroot_0_10_cm[out$Ring==i] * subDF2$frac_intermediate[subDF2$Ring==i&subDF2$Depth=="0_10"]
            out$intermediateroot_10_30_cm[out$Ring==i] <- out$fineroot_10_30_cm[out$Ring==i] * subDF2$frac_intermediate[subDF2$Ring==i&subDF2$Depth=="10_30"]
            out$intermediateroot_30_60_cm[out$Ring==i] <- out$fineroot_30_60_cm[out$Ring==i] * subDF2$frac_intermediate[subDF2$Ring==i&subDF2$Depth=="transition"]
            
            out$largeroot_0_10_cm[out$Ring==i] <- out$fineroot_0_10_cm[out$Ring==i] * subDF2$frac_large[subDF2$Ring==i&subDF2$Depth=="0_10"]
            out$largeroot_10_30_cm[out$Ring==i] <- out$fineroot_10_30_cm[out$Ring==i] * subDF2$frac_large[subDF2$Ring==i&subDF2$Depth=="10_30"]
            out$largeroot_30_60_cm[out$Ring==i] <- out$fineroot_30_60_cm[out$Ring==i] * subDF2$frac_large[subDF2$Ring==i&subDF2$Depth=="transition"]
        }
        
        
        ### update the original variables
        out$fineroot_0_10_cm <- out$fineroot_0_10_cm + out$intermediateroot_0_10_cm + out$largeroot_0_10_cm
        out$fineroot_10_30_cm <- out$fineroot_10_30_cm + out$intermediateroot_10_30_cm + out$largeroot_10_30_cm
        out$fineroot_30_60_cm <- out$fineroot_30_60_cm + out$intermediateroot_30_60_cm + out$largeroot_30_60_cm
        
        out$fineroot_pool <- rowSums(data.frame(out$fineroot_0_10_cm, out$fineroot_10_30_cm,
                                                out$fineroot_30_60_cm))
        
        outDF <- out[,c("Ring", "Date", "fineroot_pool", "fineroot_0_10_cm",
                        "fineroot_10_30_cm", "fineroot_30_60_cm")]
        
    } else {
        print("no root size available")
    }
    
    
    ### return
    return(outDF)
}