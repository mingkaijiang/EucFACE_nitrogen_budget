make_total_n_budget <- function() {
    #### This function calculates all N budgeting variables
    ### out df
    terms <- c("Total plant N stock", 
               "Total plant N requirement flux", 
               "Total plant N retranslocation flux", 
               "Plant N uptake flux", 
               "Soil N mineralization flux",
               "Soil N nitrification flux",
               "Atmospheric N deposition flux",
               "Plant N uptake over requirement",
               "Plant N MRT", 
               "Plant NUE",
               "Overstorey aboveground N stock",
               "Understorey aboveground N stock",
               "Belowground N stock")
    
    out <- data.frame(terms, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("terms", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2")
    
    ### assign values
    out[out$terms == "Total plant N stock", 2:7] <- round(vegetation_standing_n_stock$total,2)
    
    out[out$terms == "Total plant N requirement flux", 2:7] <- round(total_plant_n_fluxes$Total_plant_N_requirement_flux,2)
    
    out[out$terms == "Total plant N retranslocation flux", 2:7] <- round(total_plant_n_fluxes$Total_plant_retranslocation_N_flux,2)
    
    out[out$terms == "Plant N uptake flux", 2:7] <- round(total_plant_n_fluxes$Total_plant_uptake_N_flux,2)
    
    out[out$terms == "Soil N mineralization flux", 2:7] <- round(summary_table_flux[summary_table_flux$terms=="Mineralization N flux",2:7],2)
    
    out[out$terms == "Soil N nitrification flux", 2:7] <- round(summary_table_flux[summary_table_flux$terms=="Nitrification N flux",2:7],2)
    
    out[out$terms == "Atmospheric N deposition flux", 2:7] <- round(summary_table_flux[summary_table_flux$terms=="Atmospheric deposition N flux",2:7],2)
    
    out[out$terms == "Plant N uptake over requirement", 2:7] <- round(total_plant_n_fluxes$Total_uptake_over_requirement_ratio,2)
    
    out[out$terms == "Plant N MRT", 2:7] <- round(plant_n_MRT$plant_N_MRT,2)
    
    out[out$terms == "Plant NUE", 2:7] <- round(plant_n_use_efficiency$NUE,2)
    
    out[out$terms == "Overstorey aboveground N stock", 2:7] <- round(vegetation_standing_n_stock$oa,2)
    
    out[out$terms == "Understorey aboveground N stock", 2:7] <- round(vegetation_standing_n_stock$understorey,2)
    
    out[out$terms == "Belowground N stock", 2:7] <- round(vegetation_standing_n_stock$belowground,2)
    
    
    ### aCO2 and eCO2 averages
    out$aCO2 <- round(rowMeans(data.frame(out$R2, out$R3, out$R6)), 4)
    out$eCO2 <- round(rowMeans(data.frame(out$R1, out$R4, out$R5)) , 4)
    
    
    ### sd
    out$aCO2_sd <- rowSds(as.matrix(subset(out, select=c(R2, R3, R6))), na.rm=T)
    out$eCO2_sd <- rowSds(as.matrix(subset(out, select=c(R1, R4, R5))), na.rm=T)
    
    ### save
    write.csv(out, "output/n_budget/total_n_budget.csv", row.names=F)
    
    
    return(out)
    
}