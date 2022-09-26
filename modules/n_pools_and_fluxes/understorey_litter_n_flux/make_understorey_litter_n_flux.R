make_understorey_litter_n_flux <- function(n_conc,
                                           c_flux,
                                           n_retrans){
    
    ### prepare output df
    out <- merge(c_flux, n_retrans, by="Ring")
    
    ### prepare out df dates
    out$s.diff <- difftime(out$Start_date, "2010-01-01", units="days")
    out$e.diff <- difftime(out$End_date, "2010-01-01", units="days")
    n_conc$numd <- difftime(n_conc$Date, "2010-01-01", units="days")
    
    
    ### find the common month and year
    for (i in c(1:6)) {
        mydf1 <- subset(n_conc, Ring == i)
        
        for (j in mydf1$numd) {
            
            mydf2 <- subset(mydf1, numd == j)
            
            out[out$Ring == i & out$s.diff <= j & out$e.diff >= j, "PercN"] <- mydf2$PercN
            out[out$Ring == i & out$s.diff <= j & out$e.diff >= j, "date"] <- difftime(mydf2$Date, "2010-01-01", units="days")
            
        }
    }
    
    out$PercN <- out$PercN * (1 - out$retrans_coef)
    
    outDF <- out[complete.cases(out),]
    
    # calculate n flux
    outDF$understorey_litter_n_flux <- outDF$understorey_production_flux / c_fraction_ud * outDF$PercN / 100
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "understorey_litter_n_flux", "Days")]
    
    return(outDF)
}


