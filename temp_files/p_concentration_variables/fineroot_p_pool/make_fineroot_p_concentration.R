#### Make the fine root P concentration
make_fineroot_p_concentration <- function(func){
    
    ### download the data
    download_fineroot_p_data()

    ### read in the csv
    myDF <- read.csv("temp_files/EucFACE_FineRootData.csv")

    ### setting up the date
    myDF$Date <- paste0("1-", as.character(myDF$Date))
    myDF$date <- as.Date(myDF$Date, "%d-%b-%y")
    
    ### average across rings and dates, for each depth
    frp.1 <- summaryBy(P.ppm.0~Ring.ID+date,data=myDF,FUN=func,keep.names=T, na.rm=T)
    frp.2 <- summaryBy(P.ppm30~Ring.ID+date,data=myDF,FUN=func,keep.names=T, na.rm=T)
 
    ### convert date to character
    frp.1$date <- as.character(frp.1$date)
    frp.2$date <- as.character(frp.2$date)

    ### averaging concentration across depth
    ### asign weight by depth
    frp.1$frp_10_30cm <- frp.2$P.ppm30
    frp.1$frp_0_30cm <- (frp.1$frp_10_30cm * 2/3) + (frp.1$P.ppm.0 * 1/3)
    
    ### converting from ppm to %
    frp.1$frp_0_30cm_percent <- frp.1$frp_0_30cm * 10^-4
    
    #- format dataframe to return
    frp.out <- frp.1[,c("date","Ring.ID","frp_0_30cm_percent")]
    names(frp.out) <- c("Date", "Ring", "PercP")
    
    return(frp.out)
}