make_summary_table_np_ratios <- function() {
    
    ### canopy
    outDF <- canopy_np_ratio
    colnames(outDF) <- c("Ring", "canopy")
    
    ### leaflitter
    outDF$leafliter <- leaflitter_np_ratio$np_ratio
    
    ### understorey
    outDF$understorey <- understorey_np_ratio$np_ratio
    
    ### wood
    outDF$wood <- wood_np_ratio$np_ratio
    
    ### fineroot
    outDF$fineroot <- fineroot_np_ratio$np_ratio
    
    ### frass
    outDF$frass <- frass_np_ratio$np_ratio
    
    ### soil
    outDF$soil <- soil_np_ratio$np_ratio
    
    ### readily available nutrients
    outDF$readily_available_soil <- readily_available_soil_np_ratio$np_ratio
    
    ### microbe
    outDF$microbe <- microbial_np_ratio$np_ratio
    

   write.csv(outDF, "plots_tables/summary_np_ratios.csv", row.names=F)
}