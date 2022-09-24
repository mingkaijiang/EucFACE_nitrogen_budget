
make_soil_n_mineralization_flux <- function(bk_density) {
    
    ## read in data - extractable NP data
    myDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILMINERALISATION_L3_20120724-20140124.csv"))
    
    myDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILMINERALISATION_L1_20140428-20160121.csv"))
    
    myDF1 <- myDF1[,c("date", "ring", "nitrification", "N_mineralisation", "P_mineralisation")]
    myDF2 <- myDF2[,c("date", "ring", "nitrification", "N_mineralisation", "P_mineralisation")]
    
    myDF1$date <- as.Date(as.character(myDF1$date), format="%Y-%m-%d")
    myDF2$date <- as.Date(as.character(myDF2$date), format="%d/%m/%Y")
    
    myDF <- rbind(myDF1, myDF2)
    
    # average across rings, dates, and depths, unit: mg/kg/d 
    myDF.m <- summaryBy(N_mineralisation~date+ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    

    # obtain ring averaged soil bulk density (0 - 10 cm only)
    bk_density <- subset(bk_density, Depth == "0_10")
    
    # add bulk density
    for (i in 1:6){
        myDF.m[myDF.m$ring == i, "bk_density"] <- bk_density[bk_density$Ring == i, "bulk_density_kg_m3"] 
    }
    
    # from mg kg-1 d-1 to mg m-2 d-1
    myDF.m$soil_n_mineralization <- myDF.m$N_mineralisation * myDF.m$bk_density * 0.1 

    
    # output table
    myDF.out <- myDF.m[,c("date", "ring", "soil_n_mineralization")]
    colnames(myDF.out) <- c("Date", "Ring", "soil_n_mineralization_flux")
    
    
    myDF.out$Start_date <- myDF.out$End_date <- myDF.out$Date
    myDF.out$Days <- 1
    
    #myDF.out <- myDF.out[complete.cases(myDF.out$soil_n_mineralization_flux),]
    
    
    return(myDF.out)
}