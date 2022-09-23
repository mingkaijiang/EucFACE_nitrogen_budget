#- Make the soil C pool
make_soil_c_pool <- function(bk_density){
    
    ### download the data
    infile <- "FACE_P0014_ALL_BasicSoilProperties_L1_2012.csv"
    if(!file.exists(paste0("download/", infile))) {
        download_soil_p_data()
    }
    
    ### read in data - soil property data
    myDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2012.csv"))
    
    myDF3 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2013.csv"))   
    
    myDF4 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2014.csv"))
    
    myDF5 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2015.csv"))
    
    myDF2 <- myDF2[,1:10]
    myDF3 <- myDF3[,1:10]
    myDF4 <- myDF4[,1:10]
    myDF5 <- myDF5[,1:10]
    
    colnames(myDF2) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF3) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF4) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF5) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    
    myDF <- rbind(myDF2, myDF3, myDF4, myDF5)
    myDF$Date <- dmy(myDF$Date)
    
    ### get rid of spaces in the variable "Depth"
    myDF$depth <- as.character(myDF$depth)
    myDF$depth <- gsub(" ", "", myDF$depth, fixed = TRUE)
    myDF$depth <- gsub("-", "_", myDF$depth, fixed = TRUE)
    myDF$depth <- gsub("cm", "", myDF$depth, fixed = TRUE)
    myDF$depth <- gsub("10_20", "10_30", myDF$depth, fixed = TRUE)
    myDF$depth <- gsub("20_30", "10_30", myDF$depth, fixed = TRUE)
    
    ### note that all data in 2015 are missing. Remove them.
    myDF <- subset(myDF,Date<as.Date("2015-01-01"))
    
    myDF <- myDF[,c("Date", "ring", "depth", "totC")]
    colnames(myDF) <- c("Date", "Ring", "Depth", "totC")
    
    # update earlier datasets
    myDF$Trt[myDF$Ring%in%c(1,4,5)] <- "eCO2"
    myDF$Trt[myDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    
    ### we will need more recent soil C data in deeper depths
    # read in the Oct 2018 Johanna data at deeper depths
    tmpDF <- read.csv("temp_files/belowground_P_working_sheet.csv")
    
    tmpDF <- tmpDF[,c("Date", "Ring", "Depth", "SoilC")]
    colnames(tmpDF) <- c("Date", "Ring", "Depth", "totC")
    tmpDF$Date <- as.Date(tmpDF$Date, format="%d/%m/%y")
    
    ### assign treatment
    tmpDF$Trt[tmpDF$Ring%in%c(1,4,5)] <- "eCO2"
    tmpDF$Trt[tmpDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    ### rbind and expand
    totDF <- rbind(myDF, tmpDF)
    totDF.avg <- summaryBy(totC~Trt+Date+Ring+Depth, data=totDF, 
                           FUN=mean, keep.names=T, na.rm=T)
    
    exDF <- expand.grid("Date"=unique(totDF$Date), 
                        "Ring"=unique(totDF$Ring),
                        "Depth"=unique(totDF$Depth))
    
    exDF <- merge(exDF, totDF.avg, by=c("Date", "Ring", "Depth"),
                  all.x=T)
    
    
    ### assign treatment
    exDF$Trt[exDF$Ring%in%c(1,4,5)] <- "eCO2"
    exDF$Trt[exDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    
    ###calculate trt-averaged depth reduction
    #mpDF2 <- summaryBy(totC~Depth+Trt, FUN=mean, data=tmpDF, keep.names=T)
    
    #mpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="10_30"] <- tmpDF2$totC[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="10_30"]/tmpDF2$totC[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="0_10"]
    #mpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="10_30"] <- tmpDF2$totC[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="10_30"]/tmpDF2$totC[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="0_10"]
    
    #mpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="transition"] <- tmpDF2$totC[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="transition"]/tmpDF2$totC[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="0_10"]
    #mpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="transition"] <- tmpDF2$totC[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="transition"]/tmpDF2$totC[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="0_10"]
    
    
    tmpDF2 <- summaryBy(totC~Depth+Ring, FUN=mean, data=tmpDF, keep.names=T)
    
    for (i in 1:6) {
        tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="10_30"] <- tmpDF2$totC[tmpDF2$Ring==i&tmpDF2$Depth=="10_30"]/tmpDF2$totC[tmpDF2$Ring==i&tmpDF2$Depth=="0_10"]
        tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="transition"] <- tmpDF2$totC[tmpDF2$Ring==i&tmpDF2$Depth=="transition"]/tmpDF2$totC[tmpDF2$Ring==i&tmpDF2$Depth=="0_10"]
        
    }
    
    outDF <- c()
    
    #for (i in unique(exDF$Date)) {
    #    for (j in c("aCO2", "eCO2")) {
    #        
    #        subDF1 <- subset(exDF, Date==i & Trt==j)
    #        
    #        subDF1$totC[subDF1$Depth=="10_30"&subDF1$Date==i&subDF1$Trt==j] <- ifelse(is.na(subDF1$totC[subDF1$Depth=="10_30"&subDF1$Date==i&subDF1$Trt==j]),
    #                                                                                  subDF1$totC[subDF1$Depth=="0_10"&subDF1$Date==i&subDF1$Trt==j]*tmpDF2$Red[tmpDF2$Trt==j&tmpDF2$Depth=="10_30"],
    #                                                                                  subDF1$totC[subDF1$Depth=="10_30"&subDF1$Date==i&subDF1$Trt==j])
    #        
    #        
    #        subDF1$totC[subDF1$Depth=="transition"&subDF1$Date==i&subDF1$Trt==j] <- ifelse(is.na(subDF1$totC[subDF1$Depth=="transition"&subDF1$Date==i&subDF1$Trt==j]),
    #                                                                                  subDF1$totC[subDF1$Depth=="0_10"&subDF1$Date==i&subDF1$Trt==j]*tmpDF2$Red[tmpDF2$Trt==j&tmpDF2$Depth=="transition"],
    #                                                                                  subDF1$totC[subDF1$Depth=="transition"&subDF1$Date==i&subDF1$Trt==j])
    #    
    #        
    #        outDF <- rbind(outDF, subDF1)
    #        
    #    }
    #}
    
    
    for (i in unique(exDF$Date)) {
        for (j in c(1:6)) {
            
            subDF1 <- subset(exDF, Date==i & Ring==j)
            
            subDF1$totC[subDF1$Depth=="10_30"&subDF1$Date==i&subDF1$Ring==j] <- ifelse(is.na(subDF1$totC[subDF1$Depth=="10_30"&subDF1$Date==i&subDF1$Ring==j]),
                                                                                       subDF1$totC[subDF1$Depth=="0_10"&subDF1$Date==i&subDF1$Ring==j]*tmpDF2$Red[tmpDF2$Ring==j&tmpDF2$Depth=="10_30"],
                                                                                       subDF1$totC[subDF1$Depth=="10_30"&subDF1$Date==i&subDF1$Ring==j])
            
            
            subDF1$totC[subDF1$Depth=="transition"&subDF1$Date==i&subDF1$Ring==j] <- ifelse(is.na(subDF1$totC[subDF1$Depth=="transition"&subDF1$Date==i&subDF1$Ring==j]),
                                                                                            subDF1$totC[subDF1$Depth=="0_10"&subDF1$Date==i&subDF1$Ring==j]*tmpDF2$Red[tmpDF2$Ring==j&tmpDF2$Depth=="transition"],
                                                                                            subDF1$totC[subDF1$Depth=="transition"&subDF1$Date==i&subDF1$Ring==j])
            
            
            outDF <- rbind(outDF, subDF1)
            
        }
    }
    
    outDF <- outDF[complete.cases(outDF$totC),]
    
    
    
    ### merge soil C with bulk density
    mydat <- merge(outDF,bk_density,by.x=c("Depth", "Ring"), by.y=c("Depth", "Ring"))
    
    
    ### calculate soil C content of each layer. Units of g C m-2 for each 10-cm long depth increment
    ###  Note that the 10-20cm and 20-30cm layers were only measured on 3 of the 15 dates.
    ###   These deeper layers have less C than the 0-10cm layer.
    ### convert to gC m-2
    mydat$totC_g_m2 <- ifelse(mydat$Depth=="0_10", mydat$totC/100*mydat$bulk_density_kg_m3*0.1*1000,
                              ifelse(mydat$Depth=="10_30", mydat$totC/100*mydat$bulk_density_kg_m3*0.2*1000,
                                     ifelse(mydat$Depth=="transition", mydat$totC/100*mydat$bulk_density_kg_m3*0.3*1000, NA)))
    
    
    outDF <- summaryBy(totC_g_m2~Date+Ring+Depth, data=mydat,
                       FUN=mean, keep.names=T, na.rm=T)
    
    colnames(outDF) <- c("Date", "Ring", "Depth", "soil_carbon_pool")
    
    
    
    return(outDF)
    
}
