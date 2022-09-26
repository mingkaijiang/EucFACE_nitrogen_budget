make_coarseroot_n_pool <- function(n_conc, c_pool, n_retrans) {
    ### apply all concentration to different timepoints
    
    out <- merge(c_pool, n_conc, by="Ring")
    out <- merge(out, n_retrans, by="Ring")
    
    out$coarseroot_n_pool <- (out$coarse_root_pool / c_fraction * out$PercN * (1-out$retrans_coef) / 100)
    
    
    outDF <- out[complete.cases(out),]
    
    outDF <- outDF[, c("Date.x", "Ring", "coarseroot_n_pool")]
    names(outDF) <- c("Date", "Ring", "coarseroot_n_pool") 
    
    return(outDF)
}