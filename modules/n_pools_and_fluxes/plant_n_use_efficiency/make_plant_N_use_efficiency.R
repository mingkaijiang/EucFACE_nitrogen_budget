make_plant_N_use_efficiency <- function(c_flux, n_flux) {
    
    ### convert unit from mg C m-2 d-1 to g C m-2 yr-1
    conv <- 365/1000
    
    ### calculating NPP
    rings <- c(1:6)
    nppDF <- data.frame(rings, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(nppDF) <- c("Ring", "leaf", "wood", "fine root", "coarse root",
                         "frass", "twig", "bark", "seed", "understorey", "total")
    
    for (i in rings) {
        nppDF[nppDF$Ring == i, "leaf"] <- with(canopy_c_production_flux[canopy_c_production_flux$Ring == i, ],
                                               sum(leaf_flux*Days)/sum(Days)) * conv
        
        nppDF[nppDF$Ring == i, "wood"] <- with(wood_c_production[wood_c_production$Ring == i, ],
                                               sum(wood_production_flux*Days)/sum(Days)) * conv
        
        nppDF[nppDF$Ring == i, "fine root"] <- with(fineroot_c_production_flux[fineroot_c_production_flux$Ring == i, ],
                                                    sum(fineroot_production_flux*Days)/sum(Days)) * conv
        
        nppDF[nppDF$Ring == i, "coarse root"] <- with(coarse_root_c_flux[coarse_root_c_flux$Ring == i, ],
                                                      sum(coarse_root_production_flux*Days)/sum(Days)) * conv
        
        nppDF[nppDF$Ring == i, "frass"] <- with(frass_c_production_flux[frass_c_production_flux$Ring == i, ],
                                                sum(frass_production_flux*Days)/sum(Days)) * conv
        
        nppDF[nppDF$Ring == i, "twig"] <- with(litter_c_production_flux[litter_c_production_flux$Ring == i, ],
                                               sum(twig_flux*Days)/sum(Days)) * conv
        
        nppDF[nppDF$Ring == i, "bark"] <- with(litter_c_production_flux[litter_c_production_flux$Ring == i, ],
                                               sum(bark_flux*Days)/sum(Days)) * conv
        
        nppDF[nppDF$Ring == i, "seed"] <- with(litter_c_production_flux[litter_c_production_flux$Ring == i, ],
                                               sum(seed_flux*Days)/sum(Days)) * conv
        
        nppDF[nppDF$Ring == i, "understorey"] <- with(understorey_c_flux[understorey_c_flux$Ring == i, ],
                                                      sum(understorey_production_flux*Days)/sum(Days)) * conv
        
    }
    
    nppDF$total <- rowSums(nppDF[,2:10])
    
    
    ### calculate pup / npp per ring
    out <- nppDF[,c("Ring", "total")]
    for (i in 1:6) {
        out[i, "pup"] <- p_up[,i]
        
    }
    colnames(out) <- c("Ring", "NPP", "PUP")    
    out$NPP_by_PUP <- out$NPP/out$PUP   
    
    return(out)
}