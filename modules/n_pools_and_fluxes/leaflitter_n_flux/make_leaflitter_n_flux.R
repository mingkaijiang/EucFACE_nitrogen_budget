make_leaflitter_n_flux <- function(n_conc) {
    
    ### Leaf litter flux in unit of mg m-2 d-1 of leaf, not C!!!
    litter_flux <- make_leaflitter_flux()
    
    ### prepare output df
    out <- litter_flux
    
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
    
    outDF <- out[complete.cases(out),]
    
    ### calculate leaflitter N flux mg N m-2 d-1
    outDF$leaflitter_n_flux_mg_m2_d <- outDF$leaf_flux*outDF$PercN/100
    
    outDF$Days <- as.numeric(with(outDF, End_date - Start_date))
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "leaflitter_n_flux_mg_m2_d", "Days")]
    
    return(outDF)
}