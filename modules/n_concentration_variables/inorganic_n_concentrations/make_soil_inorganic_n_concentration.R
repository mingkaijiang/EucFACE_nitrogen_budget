
make_soil_inorganic_n_concentration <- function() {
    
    # soil nitrate n: Unit mg/kg 
    # soil ammonia n: unit mg/kg
    
    ## read in data - extractable NP data
    myDF1 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_extractableNP_L1_20140314-2014-11-17_V1.csv"))
    
    myDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_extractableNP_L1_20150310-20151130_V1.csv"))
    
    myDF1 <- myDF1[,1:8]
    myDF2 <- myDF2[,c("Date", "sample_number", "ring", "plot", "depth", "nitrate", "ammonium", "phosphate")]
    names(myDF2) <- names(myDF1)
    
    myDF3 <- rbind(myDF1, myDF2)
    myDF3$date <- dmy(myDF3$date)
    
    # get only top 10 cm
    myDF3 <- myDF3[which(myDF3$depth %in% " 0_10cm"),]
    
    # read in Shun's data to expand the temporal coverages of the previous data
    myDF4 <- read.csv(file.path(getToPath(), 
                                "FACE_RA_P0023_SOILEXTRACTABLENUTRIENTS_L3_20120613-20140310.csv"))
    myDF4$date <- as.Date(myDF4$date)
    
    # select a subset
    inDF1 <- myDF3[,c("date", "ring", "plot", "nitrate", "ammonium")]
    inDF2 <- myDF4[,c("date", "ring", "plot", "nitrate", "ammonium")]
    
    # convert ammonium in inDF1 to numeric
    inDF1$ammonium <- as.character(inDF1$ammonium)
    inDF1 <- inDF1[complete.cases(inDF1$ammonium),]
    inDF1[inDF1$ammonium == "BDL", "ammonium"] <- 0.0
    inDF1$ammonium <- as.numeric(inDF1$ammonium)
    
    myDF <- rbind(inDF1, inDF2)
    
    out <- summaryBy(nitrate+ammonium~date+ring,data=myDF,FUN=mean,keep.names=T,na.rm=T)
    
    out$perc_nitr <- out$nitrate * 10^-4
    out$perc_ammo <- out$ammonium * 10^-4
    out$perc_tot <- out$perc_nitr + out$perc_ammo
    
    # output table
    myDF.out <- out[,c("date", "ring", "perc_nitr", "perc_ammo", "perc_tot")]
    colnames(myDF.out) <- c("Date", "Ring","Nitrate_PercN", "Ammonium_PercN", "Total_PercN")
    
    return(myDF.out)
}