make_leaflitter_n_flux <- function(n_conc,
                                   c_flux,c_frac) {
    
    
    ### prepare output df
    out <- c_flux
    
    ### average N concentration
    n_avg <- summaryBy(PercN~Ring, data=n_conc, FUN=mean, keep.names=T, na.rm=T)
    
    ### find the common month and year
    for (i in c(1:6)) {
        out[out$Ring == i, "PercN"] <- n_avg[n_avg$Ring == i, "PercN"]
    }
    
    outDF <- out
    
    ### calculate leaflitter n flux mg n m-2 d-1
    outDF$leaflitter_n_flux_mg_m2_d <- outDF$leaf_flux/c_frac*outDF$PercN/100
    
    outDF$Days <- as.numeric(with(outDF, End_date - Start_date))
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "leaflitter_n_flux_mg_m2_d", "Days")]
    
    return(outDF)
}