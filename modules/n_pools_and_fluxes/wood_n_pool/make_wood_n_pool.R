
make_wood_n_pool <- function(n_conc, c_pool, n_retrans) {
    ### apply all concentration to different timepoints
    
    out <- merge(c_pool, n_conc, by="Ring")
    
    out <- merge(out, n_retrans, by="Ring")
    
    ### data from Kristine: sapwood N conc, no data on heartwood N
    out$sapwood_n_pool <- out$sap_pool / c_fraction * out$PercN / 100
    
    out$heartwood_n_pool <- (out$heart_pool / c_fraction * out$PercN * (1-out$retrans_coef) / 100)
    
    out$wood_n_pool <- with(out, sapwood_n_pool+heartwood_n_pool)
    
    outDF <- out[complete.cases(out),]
    
    outDF <- outDF[, c("Date.x", "Ring", "wood_n_pool", "sapwood_n_pool", "heartwood_n_pool")]
    names(outDF) <- c("Date", "Ring", "wood_n_pool", "sapwood_n_pool", "heartwood_n_pool") 
    
    return(outDF)
}