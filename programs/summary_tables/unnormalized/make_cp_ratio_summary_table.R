make_cp_ratios <- function(c_pool, p_pool) {
    ### Compute CP ratio for major pools
    
    terms <- c("Wood CP", "Canopy CP", "Fine Root CP",
               "Coarse Root CP", "Understorey CP",
               "Microbial CP", "Soil CP", "Mycorrhizal CP")
    
    treatDF <- data.frame(terms)
    treatDF$R1 <- rep(NA, length(treatDF$terms))
    treatDF$R2 <- rep(NA, length(treatDF$terms))
    treatDF$R3 <- rep(NA, length(treatDF$terms))
    treatDF$R4 <- rep(NA, length(treatDF$terms))
    treatDF$R5 <- rep(NA, length(treatDF$terms))
    treatDF$R6 <- rep(NA, length(treatDF$terms))
    
    treatDF$aCO2 <- rep(NA, length(treatDF$terms))
    treatDF$eCO2 <- rep(NA, length(treatDF$terms))
    treatDF$notes <- rep(NA, length(treatDF$terms))
    
    ### Canopy CP
    treatDF[treatDF$terms == "Canopy CP", 2:7] <- c_pool[c_pool$terms == "Canopy C Pool", 2:7]/p_pool[p_pool$terms == "Canopy P Pool",
                                                                                                      2:7]
    treatDF$notes[treatDF$terms == "Canopy CP"] <- ""
    
    ### Wood CP
    treatDF[treatDF$terms == "Wood CP", 2:7] <- c_pool[c_pool$terms == "Wood C Pool", 2:7]/p_pool[p_pool$terms == "Wood P Pool", 
                                                                                                  2:7]
    treatDF$notes[treatDF$terms == "Wood CP"] <- ""
    
    ### Fine root CP
    treatDF[treatDF$terms == "Fine Root CP", 2:7] <- c_pool[c_pool$terms == "Fine Root C Pool", 2:7]/p_pool[p_pool$terms == "Fine Root P Pool",
                                                                                                            2:7]
    treatDF$notes[treatDF$terms == "Fine Root CP"] <- ""
    
    ### Coarse root CP
    treatDF[treatDF$terms == "Coarse Root CP", 2:7] <- c_pool[c_pool$terms == "Coarse Root C Pool", 2:7]/p_pool[p_pool$terms == "Coarse Root P Pool", 
                                                                                                  2:7]
    treatDF$notes[treatDF$terms == "Coarse Root CP"] <- ""
    
    ### Underunderstorey CP
    treatDF[treatDF$terms == "Understorey CP", 2:7] <- c_pool[c_pool$terms == "Understorey C Pool", 2:7]/p_pool[p_pool$terms == "Understorey P Pool", 
                                                                                                  2:7]
    treatDF$notes[treatDF$terms == "Understorey CP"] <- ""
    
    ### Microbial CP
    treatDF[treatDF$terms == "Microbial CP", 2:7] <- c_pool[c_pool$terms == "Microbial C Pool", 2:7]/p_pool[p_pool$terms == "Microbial P Pool", 
                                                                                                  2:7]
    treatDF$notes[treatDF$terms == "Microbial CP"] <- ""
    
    ### Soil CP
    treatDF[treatDF$terms == "Soil CP", 2:7] <- c_pool[c_pool$terms == "Soil C Pool", 2:7]/p_pool[p_pool$terms == "Soil P Pool", 
                                                                                                  2:7]
    treatDF$notes[treatDF$terms == "Soil CP"] <- ""
    
    ### Mycorrhizal CP
    treatDF$notes[treatDF$terms == "Mycorrhizal CP"] <- "No P data"
    
    ### calculate treatment averages
    treatDF$aCO2 <- round(rowMeans(subset(treatDF, select=c(R2, R3, R6)), na.rm=T), 2)
    treatDF$eCO2 <- round(rowMeans(subset(treatDF, select=c(R1, R4, R5)), na.rm=T), 2)
    
    treatDF$aCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R2, R3, R6))), na.rm=T)
    treatDF$eCO2_sd <- rowSds(as.matrix(subset(treatDF, select=c(R1, R4, R5))), na.rm=T)

    treatDF[,2:7] <- round(treatDF[,2:7], 2)
    
    return(treatDF)
    
}