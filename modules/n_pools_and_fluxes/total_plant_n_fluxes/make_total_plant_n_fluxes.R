make_total_plant_n_fluxes <- function(sumDF) {

    #### Calculate total plant N requirement flux
    myDF1 <- sumDF[sumDF$terms%in%c("Canopy N flux", "Wood N flux", "Fine Root N flux",
                                   "Coarse Root N flux", "Twig litter N flux", "Bark litter N flux", 
                                   "Seed litter N flux", "Understorey N flux"),]
    
    tot1 <- colSums(myDF1[,2:7])
    
    out <- data.frame(c(1:6), tot1, c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2"))
    colnames(out) <- c("Ring", "Total_plant_N_requirement_flux", "Trt")
    
    
    #### Calculate total plant litter N flux
    ### litter fluxes, we do not have wood and coarseroot
    myDF2 <- sumDF[sumDF$terms%in%c("Leaflitter N flux", "Fineroot Litter N flux", "Understorey Litter N flux",
                                    "Twig litter N flux", "Bark litter N flux", "Seed litter N flux"),]
    
    tot2 <- colSums(myDF2[,2:7])
    
    out$Total_plant_litter_N_flux <- tot2
    
    
    ### total retrans
    myDF2 <- sumDF[sumDF$terms%in%c("Canopy retrans N flux", "Sapwood retrans N flux", "Understorey retrans N flux",
                                    "Fineroot retrans N flux"),]
    
    tot2 <- colSums(myDF2[,2:7])
    
    out$Total_plant_retranslocation_N_flux2 <- tot2
    
    
    #### Calculate total plant retranslocation N flux
    out$Total_plant_retranslocation_N_flux <- with(out, (Total_plant_N_requirement_flux - Total_plant_litter_N_flux))
    
    
    #### Calculate total plant uptake N flux
    out$Total_plant_uptake_N_flux <- with(out, Total_plant_N_requirement_flux - Total_plant_retranslocation_N_flux)
    
    #### Calculate total uptake over requirement ratio
    out$Total_uptake_over_requirement_ratio <- round(with(out, Total_plant_uptake_N_flux / Total_plant_N_requirement_flux),2)
    
    #### Calculate total retranslocation over requirement ratio
    out$Total_retranslocation_over_requirement_ratio <- with(out, 1 - Total_uptake_over_requirement_ratio)
    
    return(out)
    
}