make_wood_p_concentration <- function(func) {
    
    #### New file
    infile <- "FACE_P0020_RA_NPwood_2015-L2.csv"
    
    if(!file.exists(paste0("download/", infile))) {
        downloadHIEv(hiev=searchHIEv(infile))
    }
    
    df <- read.csv("download/FACE_P0020_RA_NPwood_2015-L2.csv")
    
    df$Date <- paste0("1-", as.character(df$Campaign))
    df$Date <- gsub("_", "-", df$Date)
    df$Date <- as.Date(df$Date, "%d-%b-%Y")
    
    
    ### out
    df.out <- df[,c("Date", "Ring", "Perc.P.mean", "Perc.P.std.error")]
    colnames(df.out) <- c("Date", "Ring", "PercP", "PercP.sd")
    
    ### file no longer exist on HIEv
    ### download the data
    #infile <- "download/FACE_P0020_RA_leafP-Eter_20130201-20151115_L1.csv"
    #if(!file.exists(infile)) {
    #    download_canopy_p_data()
    #}
    #
    #df <- read.csv(infile)
    #
    #### setting up the date
    #df$Date <- paste0("1-", as.character(df$Campaign))
    #df$Date <- as.Date(df$Date, "%d-%b-%y")
    #
    #df.wood <- subset(df, Type == "wood")
    #
    #
    #### Wood p, average across rings and date, unit = %
    #df.wood.p <- summaryBy(PercP~Ring+Date,
    #                       data=df.wood,FUN=func,keep.names=T,na.rm=T)
    #df.wood.p$month <- month(df.wood.p$Date)
    #df.wood.p$year <- year(df.wood.p$Date)
    
    return(df.out)
    

}