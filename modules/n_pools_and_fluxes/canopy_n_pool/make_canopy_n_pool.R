#- Make the canopy N pool 
make_canopy_n_pool <- function(n_conc,
                               biom,
                               c_frac) {
    ### return ring-specific canopy N data (mg/kg)
    ### n_conc: N concentration in %
    ### biom: canopy biomass pool (dry weight)
    

    ### obtaining month and year information 
    n_conc$month <- month(n_conc$Date)
    n_conc$year <- year(n_conc$Date)

    ### obtaining month and year information 
    biom$month <- month(biom$Date)
    biom$year <- year(biom$Date)
    
    ### prepare output df
    out <- biom

    ### find the common month and year 
    for (i in c(1:6)) {
        mydf1 <- subset(n_conc, Ring == i)
        
        for (j in unique(mydf1$year)) {
            mydf2 <- subset(mydf1, year == j)

            for (k in unique(mydf2$month)) {
                mydf3 <- subset(mydf2, month == k)
                avg.value <- mean(mydf3$PercN)
                out[out$Ring == i & out$year == j & out$month == k, "PercN"] <- avg.value
            }
        }
    }

    outDF <- out[complete.cases(out),]
    

    ### calculate leaf N pool g N m-2
    outDF$leaf_n_pool <- outDF$leaf_pool/c_frac*outDF$PercN/100
    
    outDF <- outDF[,c("Date", "Ring", "leaf_n_pool")]

    return(outDF)

}


