
make_wood_n_pool <- function(n_conc, c_pool, case_consideration) {
    ### apply all concentration to different timepoints
    
    out <- merge(c_pool, n_conc, by="Ring")
    
    # calculate n pool
    if (case_consideration == "total") {
        out$wood_n_pool <- (out$wood_pool / c_fraction * out$PercN / 100) 
        
    } else if (case_consideration == "sapwood") {
        out$wood_n_pool <- out$sap_pool / c_fraction * out$PercN / 100
    }

    outDF <- out[complete.cases(out),]
    
    outDF <- outDF[, c("Date.x", "Ring", "wood_n_pool")]
    names(outDF) <- c("Date", "Ring", "wood_n_pool") 
    
    return(outDF)
}