make_plant_N_mean_residence_time <- function(n_stand, n_flux) {
    
    
    out <- data.frame(n_stand$Ring, n_stand$Trt, n_stand$total, n_flux$Total_plant_uptake_N_flux)
    colnames(out) <- c("Ring", "Trt", "Total_standing_N_stock", "Total_plant_N_uptake_flux")
    
    out$plant_N_MRT <- with(out, Total_standing_N_stock / Total_plant_N_uptake_flux)
    
    return(out)
}