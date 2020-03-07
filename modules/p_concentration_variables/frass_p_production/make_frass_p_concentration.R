#- Make the frass P concentration
make_frass_p_concentration <- function(func){
    
    # download the data. 
    download_frass_data()
    
    # read in the data - frass chemistry data (for P, in unit of mg/g)
    inDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0017_RA_FRASSCHEMISTRY_L2_20121112-20141016.csv"))
    inDF2$DATE <- as.Date(inDF2$DATE)

    # average across rings and dates
    outDF2 <- summaryBy(PHOSPHORUS~DATE+RING,data=inDF2,FUN=func,keep.names=T)
    
    # convert unit from mg/g to %
    outDF2$PercP <- outDF2$PHOSPHORUS / 1000
 
    #- format dataframe to return
    out <- outDF2[,c("DATE", "RING", "PercP")]
    colnames(out) <- c("Date", "Ring", "PercP")

    return(out)
}

