#- Make the understorey N production flux
make_understorey_n_flux <- function(n_conc, c_flux, c_frac){
    
    ### obtaining month and year information 
    n_avg <- summaryBy(PercN~Ring, data=n_conc, FUN=mean, na.rm=T, keep.names=T)
    
    for (i in c(1:6)) {
        c_flux[c_flux$Ring == i, "PercN"] <- n_avg[n_avg$Ring == i, "PercN"]
    }
    
    ### calculate N flux mg N m-2 d-1
    c_flux$understorey_n_flux <- c_flux$understorey_production_flux / c_frac * c_flux$PercN / 100
    
    outDF <- c_flux[,c("Date", "Start_date", "End_date", "Ring", "understorey_n_flux", "Days")]
    
    
    return(outDF)
    
}
