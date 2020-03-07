#- Make the microbial C pool
make_microbial_pool2 <- function(bk_density){
    # return ring-specific, continuous microbial C pool
    
    ### read in data
    df <- read.csv("~/Documents/Research/Projects/EucFACE_C_Balance/R_repo/data/Microbial_Data_Yolima.csv")
    
    # average across rings, unit: mg/kg soil
    df.m <- summaryBy(Microbial.Biomass.C+Microbial.Biomass.N~Ring,
                      data=df,FUN=mean,keep.names=T,na.rm=T)
    
    bk.r <- summaryBy(bulk_density_kg_m3~ring, data=bk_density, FUN=mean, keep.names=T, na.rm=T)
    
    for (i in 1:6) {
        df.m[df.m$Ring == i, "bk_density"] <- bk.r[bk.r$ring == i, "bulk_density_kg_m3"]
    }
    
    # unit conversion: mg/kg to g/m2 
    df.m$Cmic <- df.m$bk_density * df.m$Microbial.Biomass.C * 0.3 / g_to_mg
    df.m$Nmic <- df.m$bk_density * df.m$Microbial.Biomass.N * 0.3 / g_to_mg
    
    # update variables to output Pmic in unit g m-2
    df.out <- df.m[,c("Ring", "Cmic", "Nmic")]
    
    colnames(df.out) <- c("Ring", "microbial_c_pool", "microbial_n_pool")
    
    return(df.out)
    
}
