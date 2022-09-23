# Make leaf mass consumption by herbivory flux
make_herbivory_leaf_consumption_flux <- function(sla,frass_flux) {
    
    
    # read in the consumption relationship data
    inDF1 <- read.csv("temp_files/GHS36_AG-THESIS_CA_FRASS-LEAFAREA_L2_20110101-20151231.csv")
    
    # update column names
    colnames(inDF1) <- c("leaf_area_consumed_cm2", "weight_of_frass_g", "insect_species", "tree_species", "temperature", "co2")
    
    # update units - g to mg
    inDF1$weight_of_frass_mg <- inDF1$weight_of_frass_g * g_to_mg
    
    # average sla for all rings   unit: cm2/g
    sla_avg <- mean(sla$sla_variable, na.rm=T)
    
    # derive herbivory consumption of leaf mass based on leaf area lost and sla
    inDF1$leaf_mass_consumed_mg <- inDF1$leaf_area_consumed_cm2 / sla_avg * g_to_mg
    
    # obtain leaf consumption ~ frass weight relationship
    # currently assuming one relationship for all tree species, all insect species and all co2 levels
    # force intercept to zero
    lrtn <- lm(inDF1$leaf_mass_consumed_mg~-1+inDF1$weight_of_frass_mg)
    
    # extract coefficients 
    slp <- coefficients(lrtn)[[1]]
    
    # generate leaf consumption mass df
    out <- frass_flux
    out$herbivory_leaf_consumption_flux <- out$frass_production_flux * slp
    
    out$frass_production_flux <- NULL
    
    outDF <- out[,c("Start_date", "End_date", "Date", "Ring", "herbivory_leaf_consumption_flux")]
    
    outDF$ndays <- as.numeric(outDF$End_date - outDF$Start_date) + 1
    
    # Only use data period 2012-2016
    #outDF <- outDF[outDF$Date<="2016-12-31",]
    
    return(outDF)
    
}