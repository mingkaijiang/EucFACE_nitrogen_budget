make_microbial_n_concentration <- function() {
    # return ring-specific, continuous microbial N concentration

    download_microbial_n_data()

    # download the data
    #df <- read.csv(file.path(getToPath(), 
    #                         "FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130.csv"))
    df <- read.csv("temp_files/FACE_P0014_RA_MicrobialBiomassCNP_L1_20120613-20151130_V2.csv")
    df$Nmic <- as.numeric(as.character(df$Nmic))
    df <- df[complete.cases(df$ring),]
    
    df$Date <- as.character(df$date)
    df$date <- as.Date(df$Date, format="%d/%m/%Y")    
        
    # first, averaged across depths, unit: ug g-1  - only one depth
    df.m <- summaryBy(Nmic~ring+date,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    df.m$PercN <- df.m$Nmic * 10^-4
    
    df.m <- df.m[!is.infinite(df.m$Nmic),] 
    df.m <- df.m[complete.cases(df.m),]
    
    df.out <- df.m[,c("date", "ring", "PercN")]
    colnames(df.out) <- c("Date", "Ring", "PercN")
    df.out$Depth <- "0_10"
    
    return(df.out)
    
}
