#- Make the canopy P concentration
make_canopy_p_concentration <- function(func) {
    ### return ring-specific canopy P data (mg/kg)

    ### download the data
    download_canopy_p_data()
    
    ### this file only exists in local directory now
    #df <- read.csv("download/FACE_P0020_RA_leafP-Eter_20130201-20151115_L1.csv")
    #
    #### setting up the date
    #df$Date <- paste0("1-", as.character(df$Campaign))
    #df$Date <- as.Date(df$Date, "%d-%b-%y")
    #
    #### only include green leaf
    #df.green <- subset(df, Type == "green leaf")
    #
    #### green leaf p, average across rings and date, unit = %
    #df.green.p <- summaryBy(PercP~Ring+Date,
    #                        data=df.green,FUN=func,keep.names=T,na.rm=T)
    #df.green.p$month <- month(df.green.p$Date)
    #df.green.p$year <- year(df.green.p$Date)
    #
    #return(df.green.p[,1:3])
    
    ### new file code
    df <- read.csv("download/FACE_P0020_RA_NPleaf_2012-2018-L2.csv")
    
    df$Date <- as.Date(df$Date, format = "%d/%m/%Y")
    
    ## correct unit
    df$PercP <- df$Pm / 10
    df$PercN <- df$Nm / 10
    
    ### only include green leaf
    df.green <- subset(df, Age == "old")

    ### green leaf p, average across rings and date, unit = %
    df.green.p <- summaryBy(PercP~Ring+Date,
                      data=df.green,FUN=func,keep.names=T,na.rm=T)
    df.green.p$month <- month(df.green.p$Date)
    df.green.p$year <- year(df.green.p$Date)
    
    return(df.green.p[,1:3])

}


