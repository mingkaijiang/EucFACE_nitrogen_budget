make_understorey_retranslocation_n_flux <- function (df1,
                                                  df2) {
    
    
    myDF <- merge(df1, df2, by=c("Date", "Start_date", "End_date", "Ring", "Days"))
    
    myDF$understorey_n_retranslocation_flux <- with(myDF, understorey_n_flux - understorey_litter_n_flux)
    
    out <- myDF[,c("Date", "Start_date", "End_date", "Ring", "Days", "understorey_n_retranslocation_flux")]
    
    
    return(out)
}