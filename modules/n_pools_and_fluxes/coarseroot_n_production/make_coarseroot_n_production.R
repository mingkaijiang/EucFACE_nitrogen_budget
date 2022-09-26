
make_coarseroot_n_production <- function(n_conc, c_flux, n_retrans) {
    ### apply all concentration to different timepoints
    
    out <- merge(c_flux, n_conc, by="Ring")
    out <- merge(out, n_retrans, by="Ring")
    
    # calculate n flux
    out$coarseroot_n_flux <- out$coarse_root_production_flux / c_fraction * out$PercN * (1-out$retrans_coef) / 100
    
    outDF <- out[complete.cases(out),]

    outDF <- outDF[, c("Date.x", "Start_date", "End_date", "Ring", "coarseroot_n_flux", "Days")]
    names(outDF) <- c("Date", "Start_date", "End_date", "Ring", "coarseroot_n_flux", "Days") 
    
    return(outDF)
}