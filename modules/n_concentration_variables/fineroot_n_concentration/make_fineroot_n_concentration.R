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
    
    colnames(frp.1) <- colnames(frp.2) <- c("Ring", "Date", "PercN")
    frp.1$Depth <- "0_10"
    frp.2$Depth <- "10_30"
    
    frp <- rbind(frp.1, frp.2)
    
    ### format dataframe to return
    frp <- frp[,c("Ring", "Date", "Depth", "PercN")]
    
    return(frp)
}