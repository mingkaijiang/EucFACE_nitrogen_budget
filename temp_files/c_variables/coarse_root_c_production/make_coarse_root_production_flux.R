make_coarse_root_production_flux <- function(cr_pool) {
    
    dates <- unique(cr_pool$Date)
    dates <- dates[order(dates)]
    
    prod <- subset(cr_pool, Date != dates[1])
    prod$Start_date <- prod$Date  # just to make this a date format! 
    for (i in 1:length(prod$Date)) {
        prod$Start_date[i] <- dates[which(dates == prod$Date[i]) - 1]
        prod$prev_biom[i] <- cr_pool$coarse_root_pool[cr_pool$Ring == prod$Ring[i] &
                                                          as.numeric(cr_pool$Date-prod$Start_date[i])==0]
    }
    
    # Length of period
    prod$length <- as.numeric(prod$Date - prod$Start_date)
    
    # C increment in mg C d-1
    prod$cr_production_flux <- (prod$coarse_root_pool - prod$prev_biom) * 1000/prod$length
    
    # format dataframe to return
    cr.out <- prod[,c("Start_date", "Date", "Date", "Ring", "cr_production_flux", "length")]
    
    names(cr.out) <- c("Start_date", "End_date", "Date", "Ring", "coarse_root_production_flux", "Days")
    
    return(cr.out)
    
}