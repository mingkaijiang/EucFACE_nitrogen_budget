#### Make the fine root litter N production
make_fineroot_litter_n_flux <- function(n_conc,
                                        c_flux,
                                        n_retrans){
    
    ### prepare output df
    out <- c_flux
    
    n_conc <- n_conc[n_conc$Depth=="0_10",]
    
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
    
    out$PercN <- out$PercN * (1 - n_retrans)
    
    outDF <- out[complete.cases(out),]
    
    # calculate litter n flux
    outDF$fineroot_litter_n_flux <- outDF$fineroot_production_flux / c_fraction_fr * outDF$PercN / 100
    
    outDF <- outDF[,c("Date", "Start_date", "End_date", "Ring", "fineroot_litter_n_flux", "Days")]
    
    return(outDF)
}


