#- Make the frass N flux
make_frass_n_production_flux <- function(n_conc,
                                         c_flux,
                                         c_frac){
    
    # Fo use frassfall data and frass carbon content data to obtain frass production flux.
    # Frassfall data has longer temporal coverage,
    # frass carbon content is quite constant over time and across rings.
    # Currently only time points that have both frassfall and frass C data are included.
    # May need to consider just using one frass C content coefficient across time,
    # so that more temporal coverage can be provided. 
    # returns: frass production per day (mg/d), averaged across start and date dates
    
 
    ### prepare output df
    out <- c_flux
    
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
    
    #- drop NA rows
    outDF <- out[complete.cases(out),]
    
    # averaging c_fraction by ring
    c_frac_df <- summaryBy(frass_c_fraction~Ring, data=c_frac, FUN=mean, keep.names=T, na.rm=T)
    
    ### calculate frass N flux mg N m-2 d-1
    for (i in c(1:6)) {
        outDF[outDF$Ring == i, "c_frac"] <- c_frac_df[c_frac_df$Ring == i, "frass_c_fraction"]
        
    }
    
    outDF$frass_n_flux_mg_m2_d <- outDF$frass_production_flux/outDF$c_frac*outDF$PercN/100
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "frass_n_flux_mg_m2_d", "Days")]

    return(outDF)
}

