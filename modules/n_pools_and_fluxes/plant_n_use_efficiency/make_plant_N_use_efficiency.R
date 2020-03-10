make_plant_N_use_efficiency <- function(c_flux, n_flux) {
    
 
    ### NPP df
    nppDF <- c_flux[c_flux$terms%in%c("Canopy C flux", "Wood C flux", "Coarse Root C flux",
                                      "Fine Root C flux", "Understorey C flux", "Frass C flux",
                                      "Twig C flux", "Bark C flux", "Seed C flux"),]
    
    tot <- colSums(nppDF[,2:7])
    
    out <- data.frame(n_flux$Ring, n_flux$Trt, tot, n_flux$Total_plant_uptake_N_flux)
    colnames(out) <- c("Ring", "Trt", "NPP", "Total_plant_uptake_N_flux")
    
    ### NUE
    out$NUE <- out$NPP/out$Total_plant_uptake_N_flux   
    
    return(out)
}