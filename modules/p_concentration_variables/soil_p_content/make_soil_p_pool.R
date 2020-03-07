#- Make the soil P content pool
make_soil_p_pool <- function(p_conc, bk_density){
    # return ring-specific, time series data of soil P content 
    # p_conc: soil p concentration variable
    # bk_density: ring-specific soil density data (kg/m3) across depths
    
    ### pre-processing input dfs
    p_conc$Date <- as.Date(p_conc$Date)
    
    ### averaging bulk density across depths
    #bk <- summaryBy(bulk_density_kg_m3~ring, data=bk_density, FUN=mean,
    #                keep.names=T, na.rm=T)
    bk <- subset(bk_density, Depth == "0-10cm")
    
    # assign bulk density onto each ring and each depth
    for (i in 1:6) {
        p_conc[p_conc$Ring == i, "bk_kg_m3"] <- bk[bk$ring == i, "bulk_density_kg_m3"] 
    }
    
    # calculate total P in top 30cm of soil (hence the * 0.1), unit kg m-2
    p_conc$soil_p_kg_m2 <- p_conc$PercP * p_conc$bk_kg_m3 * 0.1 / 100
    
    # return in unit of g/m2
    p_conc$soil_p_g_m2 <-p_conc$soil_p_kg_m2 * 1000.0
    
    myDF.out <- p_conc[,c("Date", "Ring", "soil_p_g_m2")]
    
    myDF.out <- myDF.out[complete.cases(myDF.out),]
    
    return(myDF.out)
    
}
