make_canopy_np_ratios <- function(n_pool, p_pool) {
    
    # ring averages
    n_avg <- summaryBy(leaf_n_pool~Ring, data=n_pool, FUN=mean, keep.names=T)
    p_avg <- summaryBy(leaf_p_pool~Ring, data=p_pool, FUN=mean, keep.names=T)
    
    # out
    out <- cbind(n_avg, p_avg$leaf_p_pool)
    colnames(out) <- c("Ring", "N", "P")
    out$np_ratio <- out$N/out$P
    
    outDF <- out[,c("Ring", "np_ratio")]
    
    return(outDF)
}