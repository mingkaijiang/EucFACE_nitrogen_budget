make_vegetation_standing_n_stock <- function(leaf, wood, understorey, fineroot, coarseroot) {
    
    ### ignores bark and twigs
    
    ### summary dfs by year & ring
    leaf.y <- summaryBy(leaf_n_pool~Ring, data=leaf, FUN=mean, na.rm=T, keep.names=T)
    wood.y <- summaryBy(wood_n_pool~Ring, data=wood, FUN=mean, na.rm=T, keep.names=T)
    froot.y <- summaryBy(fineroot_n_pool~Ring, data=fineroot, FUN=mean, na.rm=T, keep.names=T)
    croot.y <- summaryBy(coarseroot_n_pool~Ring, data=coarseroot, FUN=mean, na.rm=T, keep.names=T)
    ua.y <- summaryBy(understorey_n_pool~Ring, data=understorey, FUN=mean, na.rm=T, keep.names=T)
    
    ### compute annual averages for each pool and ring
    out <- cbind(leaf.y, wood.y$wood_n_pool, 
                 froot.y$fineroot_n_pool,croot.y$coarseroot_n_pool,ua.y$understorey_n_pool)
    colnames(out) <- c("Ring", "leaf", "wood", "fineroot", "coarseroot", "understorey")
    
    ### Calculate total
    out$total <- with(out, (leaf+wood+fineroot+coarseroot+understorey))
    
    ### calculate oa
    out$oa <- with(out, (leaf+wood))
    
    ### calculate belowground
    out$belowground <- with(out, (fineroot+coarseroot))
    
    
    ### assign aCO2 and eCO2
    out$Trt <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
    
    return(out)
}