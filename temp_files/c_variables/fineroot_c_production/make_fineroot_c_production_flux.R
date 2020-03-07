#- Make the fineroot c production flux
make_fineroot_c_production_flux <- function(){
    
    
    # read in the csv
    frb1 <- read.csv("temp_files/EucFACERootsRingDateDepth.csv")
    frb1$Date <- as.Date(frb1$Dateform, format="%d-%m-%Y")
    
    # fineroot C pool
    frb1$frb_top <- with(frb1, FRP0d_0.10cm * C0_0.10cm / 100, na.rm=T)
    frb1$frb_bot <- with(frb1, FRP0d_10.30cm * C0_10.30cm / 100, na.rm=T)
    frb1$frb_tot <- with(frb1, frb_top + frb_bot, na.rm=T)
    
    # average across rings and dates
    frb.m <- summaryBy(frb_tot+frb_top+frb_bot~Date+Ring,data=frb1,FUN=mean,keep.names=T)
    
    ## colnames
    colnames(frb.m) <- c("Date","Ring","fineroot_production_flux", 
                         "fineroot_production_flux_0_10_cm", 
                         "fineroot_production_flux_10_30_cm")
    
    s.date <- unique(frb.m$Date)
    s.date <- s.date[-7]
    
    ### prepare output
    outDF <- subset(frb.m, Date > "2014-02-18")
    
    outDF$Start_date <- rep(s.date, each=6)
    outDF$End_date <- outDF$Date
    outDF$Days <- as.numeric(outDF$End_date - outDF$Start_date) + 1
    
    # format dataframe to return
    outDF <- outDF[,c("Date","Start_date", "End_date", "Ring","fineroot_production_flux", "Days")]
    
    # convert to mg C m-2 yr-1
    outDF$fineroot_production_flux <- outDF$fineroot_production_flux * 1000
    
    ### return
    return(outDF)
    
}