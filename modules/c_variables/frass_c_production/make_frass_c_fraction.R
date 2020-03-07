#- Make the frass C fraction per ring
make_frass_c_fraction <- function() {
    
    #- read in the data - frass chemistry data (for C, in unit of %)
    inDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0017_RA_FRASSCHEMISTRY_L2_20121112-20141016.csv"))
    inDF2$DATE <- as.Date(inDF2$DATE)
    
    out <- summaryBy(CARBON~DATE+RING, data = inDF2, FUN=mean, keep.names=T, ra.rm=T)
    
    #- drop NA rows
    out <- out[complete.cases(out),]
    
    out$CARBON <- out$CARBON/100
    
    #- format dataframe to return
    out <- out[,c("DATE", "RING","CARBON")]
    colnames(out) <- c("Date", "Ring", "frass_c_fraction")
    
    return(out)
}