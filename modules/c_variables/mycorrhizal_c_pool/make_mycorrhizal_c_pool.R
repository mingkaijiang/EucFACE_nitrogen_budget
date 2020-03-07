make_mycorrhizal_c_pool <- function(micDF) {
    
    ### calculate mycorrhizal biomass
    micDF$mycorrhizal_c_pool[micDF$ring==1] <- micDF$Cmic_g_m2[micDF$ring==1] * 0.105
    micDF$mycorrhizal_c_pool[micDF$ring==2] <- micDF$Cmic_g_m2[micDF$ring==2] * 0.115
    micDF$mycorrhizal_c_pool[micDF$ring==3] <- micDF$Cmic_g_m2[micDF$ring==3] * 0.101
    micDF$mycorrhizal_c_pool[micDF$ring==4] <- micDF$Cmic_g_m2[micDF$ring==4] * 0.11
    micDF$mycorrhizal_c_pool[micDF$ring==5] <- micDF$Cmic_g_m2[micDF$ring==5] * 0.088
    micDF$mycorrhizal_c_pool[micDF$ring==6] <- micDF$Cmic_g_m2[micDF$ring==6] * 0.128
    
    outDF <- micDF[,c("date", "ring", "mycorrhizal_c_pool")]
    colnames(outDF) <- c("Date", "Ring", "mycorrhizal_c_pool")
    
    # Only use data period 2012-2016
    outDF <- outDF[outDF$Date<="2016-12-31",]
    
    return(outDF)
    
}