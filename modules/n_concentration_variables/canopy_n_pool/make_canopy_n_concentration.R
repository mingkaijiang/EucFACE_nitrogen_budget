#- Make the canopy N concentration
make_canopy_n_concentration <- function() {
    ### return ring-specific canopy n data (mg/kg)
    
    ### download the data
    download_canopy_n_data()

    ### new file code
    df <- read.csv("download/FACE_P0020_RA_NPleaf_2012-2018-L2.csv")
    
    df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
    
    ## correct unit
    df$PercP <- df$Pm / 10
    df$PercN <- df$Nm / 10
    
    ### only include green leaf
    df.green <- subset(df, Age == "old")
    
    ### green leaf p, average across rings and date, unit = %
    df.green.n <- summaryBy(PercN~Ring+Date,
                            data=df.green,FUN=mean,keep.names=T,na.rm=T)
    df.green.n$month <- month(df.green.n$Date)
    df.green.n$year <- year(df.green.n$Date)
    
    return(df.green.n[,1:3])
}


