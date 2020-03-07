make_canopy_c_production_flux_new <- function(inDF) {
    
    inDF$total <- inDF$dLEAF + inDF$leaflit
    
    # convert unit from g C m-2 period-1 to mg C m-2 d-1
    inDF$total_mg_c_m2_d <- inDF$total * 1000 / inDF$Days
    
    out <- inDF[,c("Date", "Ring", "total_mg_c_m2_d", "Start_date", "End_date", "Days")]
    colnames(out) <- c("Date", "Ring", "leaf_flux", "Start_date", "End_date", "Days")
    
    return(out)
}