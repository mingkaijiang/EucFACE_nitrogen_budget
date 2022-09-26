make_cn_ratios <- function(c_pool, 
                           n_pool, 
                           c_flux, 
                           n_flux) {
    
    out <- data.frame(c(1:6), NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("Ring", "canopy", "leaflitter", "understorey", "wood",
                       "fineroot", "frass", "soil_0_10", "soil_10_30","microbe")
    
    ### Compute CN ratio for major pools
    out$canopy <- as.numeric(c_pool[c_pool$terms == "Canopy C Pool", 2:7]/n_pool[n_pool$terms == "Canopy N Pool",
                                                                                 2:7])
    
    out$leaflitter <- as.numeric(c_pool[c_pool$terms == "Leaflitter C Pool", 2:7]/n_pool[n_pool$terms == "Canopy Litter N Pool",
                                                                                 2:7])
    
    out$understorey <- as.numeric(c_pool[c_pool$terms == "Understorey C Pool", 2:7]/n_pool[n_pool$terms == "Understorey N Pool",
                                                                                 2:7])
    
    out$wood <- as.numeric(c_pool[c_pool$terms == "Wood C Pool", 2:7]/n_pool[n_pool$terms == "Wood N Pool",
                                                                                 2:7])
    
    out$fineroot <- as.numeric(c_pool[c_pool$terms == "Fine Root C Pool", 2:7]/n_pool[n_pool$terms == "Fine Root N Pool",
                                                                                 2:7])
    
    out$frass <- as.numeric(c_flux[c_flux$terms == "Frass C flux", 2:7]/n_flux[n_flux$terms == "Frass N flux",
                                                                                 2:7])
    
    out$soil_0_10 <- as.numeric(c_pool[c_pool$terms == "Soil C Pool 0-10cm", 2:7]/n_pool[n_pool$terms == "Soil N Pool 0-10cm",
                                                                                 2:7])
    
    out$soil_10_30 <- as.numeric(c_pool[c_pool$terms == "Soil C Pool 10-30cm", 2:7]/n_pool[n_pool$terms == "Soil N Pool 10-30cm",
                                                                                         2:7])
    
    out$microbe <- as.numeric(c_pool[c_pool$terms == "Microbial C Pool 0-10cm", 2:7]/n_pool[n_pool$terms == "Microbial N Pool 0-10cm",
                                                                                 2:7])
    
    write.csv(out, "plots_tables/summary_cn_ratios.csv", row.names=F)
    
    return(out)
    
}