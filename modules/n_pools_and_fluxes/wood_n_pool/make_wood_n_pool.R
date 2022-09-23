
make_wood_n_pool <- function(n_conc, c_pool, case_consideration) {
    ### apply all concentration to different timepoints
    
    out <- merge(c_pool, n_conc, by="Ring")
    
    # calculate p pool
    if (case_consideration == "total") {
        ### data from Kristine: sapwood N conc, no data on heartwood N, assuming 50% resorption
        out$wood_n_pool <- (out$sap_pool / c_fraction * out$PercN / 100) + (out$heart_pool / c_fraction * out$PercN / 2 / 100)
        
        #out$wood_p_pool <- (out$wood_pool / c_fraction * out$PercP / 100) 
    } else if (case_consideration == "sapwood") {
        out$wood_n_pool <- out$sap_pool / c_fraction * out$PercN / 100
    } else if (case_consideration == "heartwood") {
        out$wood_n_pool <- (out$heart_pool / c_fraction * out$PercN / 2 / 100)
    }
    
    outDF <- out[complete.cases(out),]
    
    outDF <- outDF[, c("Date.x", "Ring", "wood_n_pool")]
    names(outDF) <- c("Date", "Ring", "wood_n_pool") 
    
    return(outDF)
}