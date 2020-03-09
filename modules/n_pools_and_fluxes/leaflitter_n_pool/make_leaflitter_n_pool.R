make_leaflitter_n_pool <- function(n_conc, c_pool, c_frac) {
    
    ### convert c_pool to biomass
    c_pool$leaflitter_biomass <- c_pool$leaflitter_pool / c_frac
    
    c_pool$Year <- year(c_pool$Date)
    n_conc$Year <- year(n_conc$Date)
    
    ### assign n concentration
    for (i in 1:6) {
        for (j in 2013:2016) {
            c_pool[c_pool$Ring==i&c_pool$Year==j, "n_conc"] <- n_conc[n_conc$Ring==i&n_conc$Year==j, "PercN"]
        }
    }
    
    ### calculate n pool
    c_pool$leaflitter_n_pool <- c_pool$n_conc / 100 * c_pool$leaflitter_biomass
    
    out <- c_pool[,c("Date", "Ring", "leaflitter_n_pool")]
    
    return(out)
    
}

