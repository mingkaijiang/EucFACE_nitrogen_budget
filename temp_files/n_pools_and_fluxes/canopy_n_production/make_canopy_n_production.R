##### Make the canopy N production
make_canopy_n_production <- function(n_conc,
                                     c_flux,
                                     c_frac){
    
    ### prepare output df
    out <- c_flux
    
    ### averaging p concentration for each ring
    n_avg <- summaryBy(PercN~Ring, data=n_conc, FUN=mean, keep.names=T, na.rm=T)
    
    ### find the common month and year
    for (i in c(1:6)) {
        out[out$Ring == i, "PercN"] <- n_avg[n_avg$Ring == i, "PercN"]
    }
    
    outDF <- out
    
    # calculate n flux
    outDF$canopy_n_flux <- outDF$leaf_flux / c_frac * outDF$PercN / 100
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "canopy_n_flux", "Days")]
    
    return(outDF)
}