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
    
    ### split n_conc into two depths
    n_conc1 <- n_conc[n_conc$Depth=="0_10",]
    n_conc2 <- n_conc[n_conc$Depth=="10_30",]
    
    ### find the common month and year 
    ## 0 - 10 cm
    for (i in c(1:6)) {
        mydf1 <- subset(n_conc1, Ring == i)
        
        for (j in unique(mydf1$year)) {
            mydf2 <- subset(mydf1, year == j)
            
            for (k in unique(mydf2$month)) {
                mydf3 <- subset(mydf2, month == k)
                
                out[out$Ring == i & out$year == j & out$month == k, "PercN_0_10"] <- mydf3$PercN
            }
        }
    }
    
    ## 10 - 30cm
    for (i in c(1:6)) {
        mydf1 <- subset(n_conc2, Ring == i)
        
        for (j in unique(mydf1$year)) {
            mydf2 <- subset(mydf1, year == j)
            
            for (k in unique(mydf2$month)) {
                mydf3 <- subset(mydf2, month == k)
                
                out[out$Ring == i & out$year == j & out$month == k, "PercN_10_30"] <- mydf3$PercN
            }
        }
    }
    
    ### calculate N concentration average
    out$PercN_avg <- with(out, PercN_0_10 * (1/3) + PercN_10_30 * (2/3))
    
    # calculate n pool
    out$fineroot_n_pool_0_10 <- out$fineroot_0_10_cm / c_fraction_fr * out$PercN_0_10 / 100
    out$fineroot_n_pool_10_30 <- out$fineroot_10_30_cm / c_fraction_fr * out$PercN_10_30 / 100
    out$fineroot_n_pool_30_60 <- out$fineroot_30_60_cm / c_fraction_fr * out$PercN_avg / 100
    
    out$fineroot_n_pool <- with(out, fineroot_n_pool_0_10+fineroot_n_pool_10_30+fineroot_n_pool_30_60)
    
    
    outDF <- out[complete.cases(out),]
    
    outDF <- outDF[, c("Date", "Ring", "fineroot_n_pool", "fineroot_n_pool_0_10", "fineroot_n_pool_10_30", "fineroot_n_pool_30_60")]
        
    return(outDF)
}