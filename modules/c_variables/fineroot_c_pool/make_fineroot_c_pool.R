#- Make the fine root C pool
make_fineroot_c_pool <- function(){
    
    # read in the csv
    frb1 <- read.csv("temp_files/EucFACERootsRingDateDepth.csv")
    frb1$Date <- as.Date(frb1$Dateform, format="%d-%m-%Y")
    
    # fineroot C pool
    frb1$frb_top <- with(frb1, FRB_0.10cm * C0_0.10cm / 100)
    frb1$frb_bot <- with(frb1, FRB_10.30cm * C0_10.30cm / 100)
    frb1$frb_tot <- with(frb1, frb_top + frb_bot)
    
    # average across rings and dates
    frb.m <- summaryBy(frb_tot+frb_top+frb_bot~Date+Ring,data=frb1,FUN=mean,keep.names=T)
    
    ## colnames
    colnames(frb.m) <- c("Date","Ring","fineroot_pool", "fineroot_0_10_cm", "fineroot_10_30_cm")
    
    ### return
    return(frb.m)
}