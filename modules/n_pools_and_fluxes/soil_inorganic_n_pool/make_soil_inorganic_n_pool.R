
make_soil_inorganic_n_pool <- function(n_conc,
                                       bk_density) {
 
    # obtain ring averaged soil bulk density (0 - 10 cm only)
    bk_density <- subset(bk_density, Depth == "0-10cm")
    
    # assign bulk density onto each ring and each depth
    for (i in 1:6) {
        n_conc[n_conc$Ring == i, "bk_kg_m3"] <- bk_density[bk_density$ring == i, "bulk_density_kg_m3"] 
    }
    
    # calculate total N in top 10cm of soil (hence the * 0.1), unit kg m-2
    n_conc$total_kg_m2 <- n_conc$Total_PercN * n_conc$bk_kg_m3 * 0.1 / 100
    n_conc$nitr_kg_m2 <- n_conc$Nitrate_PercN * n_conc$bk_kg_m3 * 0.1 / 100
    n_conc$ammo_kg_m2 <- n_conc$Ammonium_PercN * n_conc$bk_kg_m3 * 0.1 / 100
    
    # return in unit of g/m2
    n_conc$nitr_g_m2 <-n_conc$nitr_kg_m2 * 10^3
    n_conc$ammo_g_m2 <-n_conc$ammo_kg_m2 * 10^3
    n_conc$total_g_m2 <-n_conc$total_kg_m2 * 10^3
    
    myDF.out <- n_conc[,c("Date", "Ring", "nitr_g_m2", "ammo_g_m2", "total_g_m2")]
    colnames(myDF.out) <- c("Date", "Ring", "nitrate_pool", "ammonium_pool", "total_inorganic_pool")
    
    return(myDF.out)
}