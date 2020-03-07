make_leaflitter_n_concentration <- function() {
    
    ### new file code
    infile <- "download/FACE_P0020_RA_NPsenesced_2013-2018-L2.csv"
    
    if(!file.exists(infile)) {
        download_leaflitter()
    }
    
    df <- read.csv("download/FACE_P0020_RA_NPsenesced_2013-2018-L2.csv")
    
    df$Date <- paste0("1-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    ### lit leaf n, average across rings and date, unit = %
    df.lit.n <- summaryBy(Perc.N~Ring+Date,
                            data=df,FUN=mean,keep.names=T,na.rm=T)
    df.lit.n$month <- month(df.lit.n$Date)
    df.lit.n$year <- year(df.lit.n$Date)
    
    colnames(df.lit.n) <- c("Ring", "Date", "PercN", "month", "year")
    

    return(df.lit.n[,1:3])
}