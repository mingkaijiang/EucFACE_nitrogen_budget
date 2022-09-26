make_fineroot_retranslocation_n_flux <- function (df1,
                                                  df2) {
    
    
    myDF <- merge(df1, df2, by=c("Date", "Start_date", "End_date", "Ring", "Days"))
    
    myDF$fineroot_n_retranslocation_flux <- with(myDF, fineroot_n_flux_mg_m2_d - fineroot_litter_n_flux)
    
    out <- myDF[,c("Date", "Start_date", "End_date", "Ring", "Days", "fineroot_n_retranslocation_flux")]
    
    
    return(out)
}