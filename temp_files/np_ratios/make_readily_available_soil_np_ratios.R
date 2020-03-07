make_readily_available_soil_np_ratios <- function(n_pool, p_pool) {
    
    # ring averages
    n_avg <- summaryBy(total_inorganic_pool~Ring, data=n_pool, FUN=mean, keep.names=T)
    p_avg <- summaryBy(soil_phosphate_p_g_m2~Ring, data=p_pool, FUN=mean, keep.names=T)
    
    # out
    out <- cbind(n_avg, p_avg$soil_phosphate_p_g_m2)
    colnames(out) <- c("Ring", "N", "P")
    out$np_ratio <- out$N/out$P
    
    outDF <- out[,c("Ring", "np_ratio")]
    
    return(outDF)
}