make_soil_phosphate_pool <- function(p_conc,
                                     bk_density) {
    
    # obtain ring averaged soil bulk density (0 - 10 cm only)
    bk_density <- subset(bk_density, Depth == "0-10cm")
    
    # assign bulk density onto each ring and each depth
    for (i in 1:6) {
        p_conc[p_conc$Ring == i, "bk_kg_m3"] <- bk_density[bk_density$ring == i, "bulk_density_kg_m3"] 
    }
    
    # calculate total P in top 10cm of soil (hence the * 0.1), unit kg m-2
    p_conc$soil_p_kg_m2 <- p_conc$PercP * p_conc$bk_kg_m3 * 0.1 / 100
    
    # return in unit of g/m2
    p_conc$soil_phosphate_p_g_m2 <-p_conc$soil_p_kg_m2 * 10^3
    
    myDF.out <- p_conc[,c("Date", "Ring", "soil_phosphate_p_g_m2")]
    
    return(myDF.out)
}