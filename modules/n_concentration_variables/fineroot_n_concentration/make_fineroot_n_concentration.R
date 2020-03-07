make_fineroot_n_concentration <- function(){
    
    ### read in the csv
    myDF <- read.csv("temp_files/EucFACE_FineRootData.csv")

    ### setting up the date
    myDF$Date <- paste0("1-", as.character(myDF$Date))
    myDF$date <- as.Date(myDF$Date, "%d-%b-%y")
    
    ### average across rings and dates, for each depth
    frp.1 <- summaryBy(X.N.0~Ring.ID+date,data=myDF,FUN=mean,keep.names=T, na.rm=T)
    frp.2 <- summaryBy(X.N.30~Ring.ID+date,data=myDF,FUN=mean,keep.names=T, na.rm=T)
 
    ### convert date to character
    frp.1$date <- as.character(frp.1$date)
    frp.2$date <- as.character(frp.2$date)

    ### averaging concentration across depth
    ### asign weight by depth
    frp.1$frp_10_30cm <- frp.2$X.N.30
    frp.1$frp_0_30cm <- (frp.1$frp_10_30cm * 2/3) + (frp.1$X.N.0 * 1/3)
    
    
    ### format dataframe to return
    frp.out <- frp.1[,c("date","Ring.ID","frp_0_30cm")]
    names(frp.out) <- c("Date", "Ring", "PercN")
    
    return(frp.out)
}