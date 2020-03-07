make_fineroot_np_ratios <- function(n_conc, p_conc) {
    
    # ring averages
    n_avg <- summaryBy(PercN~Ring, data=n_conc, FUN=mean, keep.names=T)
    p_avg <- summaryBy(PercP~Ring, data=p_conc, FUN=mean, keep.names=T)
    
    # out
    out <- cbind(n_avg, p_avg$PercP)
    colnames(out) <- c("Ring", "N", "P")
    out$np_ratio <- out$N/out$P
    
    outDF <- out[,c("Ring", "np_ratio")]
    
    return(outDF)
}