#- Make the microbial C pool
make_microbial_c_pool <- function(bk_density) {
    
    # download the data
    download_microbial_data()
    
    myDF <- read.csv(file.path(getToPath(), 
                               "FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130.csv"))
    
    myDF <- myDF[,c("date", "ring", "depth", "Cmic")]
    colnames(myDF) <- c("Date", "Ring", "Depth", "Cmic")
    myDF$Date <- as.Date(myDF$Date, "%d/%m/%Y")
    myDF$Depth <- "0_10"
    
    
    # read in the Oct 2018 Johanna data at deeper depths
    tmpDF <- read.csv("temp_files/belowground_P_working_sheet.csv")
    
    tmpDF <- tmpDF[,c("Date", "Ring", "Depth", "Cmic")]
    colnames(tmpDF) <- c("Date", "Ring", "Depth", "Cmic")
    tmpDF$Date <- as.Date(tmpDF$Date, format="%d/%m/%y")
    
    ### calculate trt-averaged depth reduction
    tmpDF$Trt[tmpDF$Ring%in%c(1,4,5)] <- "eCO2"
    tmpDF$Trt[tmpDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    #tmpDF2 <- summaryBy(Cmic~Depth+Trt, FUN=mean, data=tmpDF, keep.names=T, na.rm=T)
    #
    #tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="10_30"] <- tmpDF2$Cmic[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="10_30"]/tmpDF2$Cmic[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="0_10"]
    #tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="10_30"] <- tmpDF2$Cmic[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="10_30"]/tmpDF2$Cmic[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="0_10"]
    #
    #tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="transition"] <- tmpDF2$Cmic[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="transition"]/tmpDF2$Cmic[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="0_10"]
    #tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="transition"] <- tmpDF2$Cmic[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="transition"]/tmpDF2$Cmic[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="0_10"]
    
    
    tmpDF2 <- summaryBy(Cmic~Depth+Ring, FUN=mean, data=tmpDF, keep.names=T)
    
    for (i in 1:6) {
        tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="10_30"] <- tmpDF2$Cmic[tmpDF2$Ring==i&tmpDF2$Depth=="10_30"]/tmpDF2$Cmic[tmpDF2$Ring==i&tmpDF2$Depth=="0_10"]
        tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="transition"] <- tmpDF2$Cmic[tmpDF2$Ring==i&tmpDF2$Depth=="transition"]/tmpDF2$Cmic[tmpDF2$Ring==i&tmpDF2$Depth=="0_10"]
        
    }
    
    # update earlier datasets
    myDF$Trt[myDF$Ring%in%c(1,4,5)] <- "eCO2"
    myDF$Trt[myDF$Ring%in%c(2,3,6)] <- "aCO2"
    
    
    #test <- rbind(myDF, tmpDF)
    
    #p1 <- ggplot(test, aes(Date, Cmic))+
    #    geom_point(aes(pch=Depth)); p1
    
    
    myDF2 <- myDF3 <- myDF
    myDF2$Depth <- "10_30"
    myDF3$Depth <- "transition"
    
    #myDF2$Rev[myDF2$Trt=="aCO2"] <- myDF2$Cmic[myDF2$Trt=="aCO2"] * tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="10_30"]
    #myDF2$Rev[myDF2$Trt=="eCO2"] <- myDF2$Cmic[myDF2$Trt=="eCO2"] * tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="10_30"]
    #
    #myDF3$Rev[myDF3$Trt=="aCO2"] <- myDF3$Cmic[myDF3$Trt=="aCO2"] * tmpDF2$Red[tmpDF2$Trt=="aCO2"&tmpDF2$Depth=="transition"]
    #myDF3$Rev[myDF3$Trt=="eCO2"] <- myDF3$Cmic[myDF3$Trt=="eCO2"] * tmpDF2$Red[tmpDF2$Trt=="eCO2"&tmpDF2$Depth=="transition"]
    
    
    for (i in 1:6) {
        myDF2$Rev[myDF2$Ring==i] <- myDF2$Cmic[myDF2$Ring==i] * tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="10_30"]
        myDF3$Rev[myDF3$Ring==i] <- myDF3$Cmic[myDF3$Ring==i] * tmpDF2$Red[tmpDF2$Ring==i&tmpDF2$Depth=="transition"]
        
    }
    
    myDF2 <- myDF2[,c("Date", "Ring", "Depth", "Rev")]
    myDF3 <- myDF3[,c("Date", "Ring", "Depth", "Rev")]
    colnames(myDF2) <- colnames(myDF3) <- c("Date", "Ring", "Depth", "Cmic")
    
    myDF <- myDF[,c("Date", "Ring", "Depth", "Cmic")]
    
    updDF <- rbind(myDF, rbind(myDF2, myDF3))
    
    tmpDF <- tmpDF[,c("Date", "Ring", "Depth", "Cmic")]
    
    myDF <- rbind(updDF, tmpDF)
    
    
    
    
    # average across rings and depths, unit: mg/kg
    df.m <- summaryBy(Cmic~Ring+Date+Depth,
                      data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    calcDF <- merge(df.m, bk_density, by=c("Ring", "Depth"))
    
    
    # unit conversion: mg/kg to g/m2 for the top 0-10 cm only
    calcDF$Cmic_g_m2 <- ifelse(calcDF$Depth=="0_10", calcDF$bulk_density_kg_m3 * calcDF$Cmic * 0.1 / g_to_mg, 
                               ifelse(calcDF$Depth=="10_30", calcDF$bulk_density_kg_m3 * calcDF$Cmic * 0.2 / g_to_mg,
                                      ifelse(calcDF$Depth=="transition", calcDF$bulk_density_kg_m3 * calcDF$Cmic * 0.3 / g_to_mg, NA)))
    
    
    # update variables to output Pmic in unit g m-2
    df.out <- calcDF[,c("Date", "Ring", "Depth", "Cmic_g_m2")]
    
    df.out <- df.out[complete.cases(df.out),]
    
    
    return(df.out)
    
}
