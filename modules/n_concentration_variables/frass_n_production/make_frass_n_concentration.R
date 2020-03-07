#- Make the frass N concentration
make_frass_n_concentration <- function(){
    
    # download the data. 
    download_frass_data()
    
    # read in the data - frass chemistry data (for N, in unit of %)
    inDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0017_RA_FRASSCHEMISTRY_L2_20121112-20141016.csv"))
    inDF2$DATE <- as.Date(inDF2$DATE)

    # average across rings and dates
    outDF2 <- summaryBy(NITROGEN~DATE+RING,data=inDF2,FUN=mean,keep.names=T)
 
    # format dataframe to return
    out <- outDF2[,c("DATE", "RING", "NITROGEN")]
    colnames(out) <- c("Date", "Ring", "PercN")

    return(out)
}

