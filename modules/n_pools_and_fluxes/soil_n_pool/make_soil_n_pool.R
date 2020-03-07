#- Make the soil N content pool
make_soil_n_pool <- function(n_conc, bk_density){
    # return ring-specific, time series data of soil P content 
    # n_conc: soil n concentration variable
    # bk_density: ring-specific soil density data (kg/m3) across depths
    
    ### pre-processing input dfs
    n_conc$Date <- as.Date(n_conc$Date)
    
    ### averaging bulk density across depths
    bk <- summaryBy(bulk_density_kg_m3~ring, data=bk_density, FUN=mean,
                    keep.names=T, na.rm=T)
    
    # assign bulk density onto each ring and each depth
    for (i in 1:6) {
        n_conc[n_conc$Ring == i, "bk_kg_m3"] <- bk[bk$ring == i, "bulk_density_kg_m3"] 
    }
    
    # calculate total N in top 30cm of soil (hence the * 0.3), unit kg m-2
    n_conc$soil_n_kg_m2 <- n_conc$PercN * n_conc$bk_kg_m3 * 0.3 / 100
    
    # return in unit of g/m2
    n_conc$soil_n_g_m2 <-n_conc$soil_n_kg_m2 * 1000.0
    
    myDF.out <- n_conc[,c("Date", "Ring", "soil_n_g_m2")]
    
    myDF.out <- myDF.out[complete.cases(myDF.out),]

    return(myDF.out)
    
}
