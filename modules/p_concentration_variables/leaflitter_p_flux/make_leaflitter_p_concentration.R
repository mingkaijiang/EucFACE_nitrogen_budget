make_leaflitter_p_concentration <- function(func) {
    
    ### new file code
    infile <- "download/FACE_P0020_RA_NPsenesced_2013-2018-L2.csv"
    
    if(!file.exists(infile)) {
        download_leaflitter()
    }
    
    df <- read.csv("download/FACE_P0020_RA_NPsenesced_2013-2018-L2.csv")
    
    df$Date <- paste0("1-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    ### lit leaf p, average across rings and date, unit = %
    df.lit.p <- summaryBy(Perc.P~Ring+Date,
                            data=df,FUN=func,keep.names=T,na.rm=T)
    df.lit.p$month <- month(df.lit.p$Date)
    df.lit.p$year <- year(df.lit.p$Date)
    
    colnames(df.lit.p) <- c("Ring", "Date", "PercP", "month", "year")
    
    
    #### this dataset has been updated
    #infile <- "download/FACE_P0020_RA_leafP-Eter_20130201-20151115_L1.csv"
    #
    #if(!file.exists(infile)) {
    #    download_leaflitter()
    #}
    #
    #df <- read.csv("download/FACE_P0020_RA_leafP-Eter_20130201-20151115_L1.csv")
    #
    #### setting up the date
    #df$Date <- paste0("1-", as.character(df$Campaign))
    #df$Date <- as.Date(df$Date, "%d-%b-%y")
    #df.litter <- subset(df, Type == "Leaf litter")
    #df.dead <- subset(df, Type == "sceneced leaf")
    #myDF <- rbind(df.litter, df.dead)
    #
    #### Leaf litter p, average across rings and date, unit = %
    #df.litter.p <- summaryBy(PercP~Ring+Date,
    #                         data=myDF,FUN=func,keep.names=T,na.rm=T)
    #df.litter.p$month <- month(df.litter.p$Date)
    #df.litter.p$year <- year(df.litter.p$Date)
    
    
    #### This dataset has been updated
    #df2 <- read.csv("temp_files/Litter_Data_Mingkai.csv")
    #
    #### setting up the date
    #df2$Date <- gsub("-Feb", "-02", df2$Campaign)
    #df2$Date <- paste0(as.character(df2$Date), "-01")
    #df2$Date <- as.Date(df2$Date, "%Y-%m-%d")
    #
    ##### Leaf litter p, average across rings and date, unit = %
    #df.litter.p <- summaryBy(Perc.P~Ring+Date,
    #                         data=df2,FUN=func,keep.names=T,na.rm=T)
    #df.litter.p$month <- month(df.litter.p$Date)
    #df.litter.p$year <- year(df.litter.p$Date)

    return(df.lit.p[,1:3])
}