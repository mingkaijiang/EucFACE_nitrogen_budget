make_barklitter_n_flux <- function(n_conc, litter_flux) {
    

    ### prepare output df
    out <- litter_flux
    
    ### prepare out df dates
    out$s.diff <- difftime(out$Start_date, "2010-01-01", units="days")
    out$e.diff <- difftime(out$End_date, "2010-01-01", units="days")
    n_conc$numd <- difftime(n_conc$Date, "2010-01-01", units="days")
    
    
    ### find the common month and year
    for (i in c(1:6)) {
        out[out$Ring == i, "PercN"] <- n_conc$PercN[n_conc$Ring==i]
    }
    
    
    outDF <- out[complete.cases(out),]
    
    ### calculate twiglitter N flux mg N m-2 d-1
    outDF$barklitter_n_flux_mg_m2_d <- outDF$bark_flux*outDF$PercN/100
    
    outDF$Days <- as.numeric(with(outDF, End_date - Start_date))
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "barklitter_n_flux_mg_m2_d", "Days")]
    
    return(outDF)
}