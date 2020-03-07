#- Make the microbial P concentration
make_microbial_p_concentration <- function() {
    # return ring-specific, continuous microbial P concentration

    #Pmic,ug g-1,"Fumigation extraction with Bray P AQ2 determination of PO4.
    
    # download the data
    download_microbial_p_data()
    
    df <- read.csv(file.path(getToPath(), 
                             "FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130.csv"))
    df$Pmic <- as.numeric(as.character(df$Pmic))
    df <- df[complete.cases(df$ring),]
    
    df$Date <- as.character(df$date)
    df$date <- as.Date(df$Date, format="%d/%m/%Y")    
        
    # first, averaged across depths, unit: ug g-1
    df2 <- summaryBy(Pmic~ring+date+plot,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    # now, mean/min/max across rings and date
    df.m <- summaryBy(Pmic~ring+date,
                     data=df2,FUN=mean,keep.names=T,na.rm=T)
    
    df.m$PercP <- df.m$Pmic * 10^-4
    
    df.m <- df.m[!is.infinite(df.m$Pmic),] 
    df.m <- df.m[complete.cases(df.m),]
    
    df.out <- df.m[,c("date", "ring", "PercP")]
    colnames(df.out) <- c("Date", "Ring", "PercP")
    
    
    ### this is august/sep 2017 data
    newdf <- read.csv("temp_files/MASTER_Mic_P_biomass_P0091.csv")
    newdf$Date <- "2017-09-01"
    
    # to account for the original conversion factor of 0.4
    newdf$mic_P_mg.kg <- newdf$mic_P_mg.kg/0.4
    
    ### 
    newdf.m <- summaryBy(mic_P_mg.kg~ring+depth+Date, data=newdf, 
                         FUN=mean, na.rm=T, keep.names=T)
    newdf.s <- subset(newdf.m, depth == "0-10")
    newdf.s$PercP <- newdf.s$mic_P_mg.kg * 10^-4
    newdf.out <- newdf.s[,c("Date", "ring", "PercP")]
    colnames(newdf.out) <- c("Date", "Ring", "PercP")
    
    df.out <- rbind(df.out, newdf.out)
    
    return(df.out)
    
}
