#- Make the understorey N concentration
make_understorey_n_concentration <- function(){
    ### download the data 
    download_understorey_n_data()
    
    ### read in the file
    df <- read.csv("download/FACE_P0061_RA_C4-GRASSES-GASEXCHANGE-NITROGEN_L2_20130201-20160430.csv")
    
    ### Assign date format
    df$Date <- paste0(as.character(df$campaign), "-01")
    df$Date <- sub("_", "-", df$Date)
    df$Date <- as.Date(df$Date, "%Y-%m-%d")
    
    ### There is a typo in date, correct it
    df[df$Date == "3013-10-04", "Date"] <- "2013-10-04"
    
    ### ring and date specific data
    outDF <- summaryBy(nitrogen_mg_per_g~ring+Date,
                             data=df,FUN=mean,keep.names=T,na.rm=T)

    ### return % N
    outDF$PercN <- with(outDF, nitrogen_mg_per_g / 10)
    out <- outDF[,c("Date", "ring", "PercN")]
    colnames(out) <- c("Date", "Ring", "PercN")
    
    return(out)
    
}
