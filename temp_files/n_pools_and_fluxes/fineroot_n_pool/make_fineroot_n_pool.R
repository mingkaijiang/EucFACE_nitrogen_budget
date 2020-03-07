make_fineroot_n_pool <- function(n_conc,
                                 c_pool){
    
    ### obtaining month and year information 
    n_conc$month <- month(n_conc$Date)
    n_conc$year <- year(n_conc$Date)
    
    ### obtaining month and year information 
    c_pool$month <- month(c_pool$Date)
    c_pool$year <- year(c_pool$Date)
    
    ### prepare output df
    out <- c_pool
    
    ### find the common month and year 
    for (i in c(1:6)) {
        mydf1 <- subset(n_conc, Ring == i)
        
        for (j in unique(mydf1$year)) {
            mydf2 <- subset(mydf1, year == j)
            
            for (k in unique(mydf2$month)) {
                mydf3 <- subset(mydf2, month == k)
                
                out[out$Ring == i & out$year == j & out$month == k, "PercN"] <- mydf3$PercN
            }
        }
    }
    
    # calculate n pool
    out$fineroot_n_pool <- out$fineroot_pool / c_fraction_fr * out$PercN / 100
    
    outDF <- out[complete.cases(out),]
    
    outDF <- outDF[, c("Date", "Ring", "fineroot_n_pool")]
        
    return(outDF)
}