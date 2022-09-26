calculate_sapwood_n_retranslocation_flux <- function (tflux,
                                                      retransDF) {
    
    ### merge the two
    myDF <- merge(tflux, retransDF, by="Ring")
    
    myDF$sapwood_n_retrans_flux <- with(myDF, wood_n_flux * retrans_coef)
    
    
    ### prepare retranslocation flux
    outDF <- myDF[,c("Date", "Ring", "Start_date", "End_date",
                     "sapwood_n_retrans_flux", "Days")]
    
    return(outDF)
    
}