make_summary_table_np_ratios <- function(n_conc,
                                         n_flux,
                                         n_pool) {
    
    ### read in P concentration, P fluxes and P pools
    p_conc <- read.csv("output/p_budget/summary_table_P_concentration_unnormalized.csv")
    p_flux <- read.csv("output/p_budget/summary_table_P_flux_unnormalized.csv")
    p_pool <- read.csv("output/p_budget/summary_table_P_pool_unnormalized.csv")
    
    
    ### prepare output data frame
    out <- data.frame(c(1:6), NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("Ring", "canopy", "leaflitter", "understorey", "sapwood",
                       "fineroot", "frass", "soil_0_10", "soil_10_30","microbe")
    
    
    ### Compute CN ratio for major pools
    out$canopy <- as.numeric(n_conc[n_conc$conc.terms == "Canopy N Conc", 2:7]/p_conc[p_conc$conc.terms == "Canopy P Conc",
                                                                                 2:7])
    
    out$leaflitter <- as.numeric(n_conc[n_conc$conc.terms == "Leaflitter N Conc", 2:7]/p_conc[p_conc$conc.terms == "Leaflitter P Conc",
                                                                                      2:7])
    
    out$understorey <- as.numeric(n_conc[n_conc$conc.terms == "Understorey N Conc", 2:7]/p_conc[p_conc$conc.terms == "Understorey P Conc",
                                                                                      2:7])
    
    out$sapwood <- as.numeric(n_conc[n_conc$conc.terms == "Sapwood N Conc", 2:7]/p_conc[p_conc$conc.terms == "Sapwood P Conc",
                                                                                      2:7])
    
    out$fineroot <- as.numeric(n_conc[n_conc$conc.terms == "Fine Root N Conc", 2:7]/p_conc[p_conc$conc.terms == "Fine Root P Conc",
                                                                                      2:7])
    
    out$frass <- as.numeric(n_conc[n_conc$conc.terms == "Frass N Conc", 2:7]/p_conc[p_conc$conc.terms == "Frass P Conc",
                                                                                      2:7])
    
    out$soil_0_10 <- as.numeric(n_conc[n_conc$conc.terms == "Soil N Conc 0-10cm", 2:7]/p_conc[p_conc$conc.terms == "Soil P Conc 0-10cm",
                                                                                      2:7])
    
    out$soil_10_30 <- as.numeric(n_conc[n_conc$conc.terms == "Soil N Conc 10-30cm", 2:7]/p_conc[p_conc$conc.terms == "Soil P Conc 10-30cm",
                                                                                      2:7])
    
    out$microbe <- as.numeric(n_conc[n_conc$conc.terms == "Microbial N Conc 0-10cm", 2:7]/p_conc[p_conc$conc.terms == "Microbial P Conc 0-10cm",
                                                                                      2:7])
    
    

    write.csv(out, "output/n_budget/summary_np_ratios.csv", row.names=F)
     
     return(out)
}