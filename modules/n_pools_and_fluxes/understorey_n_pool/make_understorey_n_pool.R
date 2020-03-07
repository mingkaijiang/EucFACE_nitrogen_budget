#- Make the understorey N pool
make_understorey_n_pool <- function(n_conc, c_pool, c_frac,
                                    live_or_total){
    
    ### obtaining month and year information 
    n_avg <- summaryBy(PercN~Ring, data=n_conc, FUN=mean, na.rm=T, keep.names=T)
    
    for (i in c(1:6)) {
        c_pool[c_pool$Ring == i, "PercN"] <- n_avg[n_avg$Ring == i, "PercN"]
    }
    
    if (live_or_total == "Live") {
        ### calculate N pool g N m-2
        c_pool$understorey_n_pool <- c_pool$Live_g_C_m2 / c_frac * c_pool$PercN / 100
    } else if (live_or_total == "Total") {
        ### calculate N pool g N m-2
        c_pool$understorey_n_pool <- c_pool$Total_g_C_m2 / c_frac * c_pool$PercN / 100
    }

    
    outDF <- c_pool[,c("Date", "Ring", "understorey_n_pool")]
    
    
    return(outDF)
    
}
