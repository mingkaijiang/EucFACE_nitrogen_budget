#- Make the soil N content pool
make_soil_n_pool <- function(n_conc, bk_density){
    # return ring-specific, time series data of soil P content 
    # n_conc: soil n concentration variable
    # bk_density: ring-specific soil density data (kg/m3) across depths
    
    ### pre-processing input dfs
    n_conc$Date <- as.Date(n_conc$Date)
    
    ### merge
    n_conc <- merge(n_conc, bk_density, by=c("Ring", "Depth"))
    
    
    # calculate total N in top 60cm of soil (hence the * 0.1), unit kg m-2
    n_conc$soil_n_kg_m2 <- ifelse(n_conc$Depth=="0_10", n_conc$PercN * n_conc$bulk_density_kg_m3 * 0.1 / 100, 
                                  ifelse(n_conc$Depth=="10_30", n_conc$PercN * n_conc$bulk_density_kg_m3 * 0.2 / 100,
                                         ifelse(n_conc$Depth=="transition", n_conc$PercN * n_conc$bulk_density_kg_m3 * 0.3 / 100, NA)))
    
    # return in unit of g/m2
    n_conc$soil_n_g_m2 <-n_conc$soil_n_kg_m2 * 1000.0
    
    myDF.out <- n_conc[,c("Date", "Ring", "Depth", "soil_n_g_m2")]
    
    myDF.out <- myDF.out[complete.cases(myDF.out),]
    
    return(myDF.out)
    
}
