#- Make the soil C pool
make_soil_c_pool <- function(bk_density, return="shallow"){

    ### download the data
    infile <- "FACE_P0014_ALL_BasicSoilProperties_L1_2012.csv"
    if(!file.exists(paste0("download/", infile))) {
        download_soil_p_data()
    }

    ### read in data - soil property data
    myDF2 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2012.csv"))
    
    myDF3 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2013.csv"))   
    
    myDF4 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2014.csv"))
    
    myDF5 <- read.csv(file.path(getToPath(), 
                                "FACE_P0014_ALL_BasicSoilProperties_L1_2015.csv"))
    
    myDF2 <- myDF2[,1:10]
    myDF3 <- myDF3[,1:10]
    myDF4 <- myDF4[,1:10]
    myDF5 <- myDF5[,1:10]
    
    colnames(myDF2) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF3) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF4) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    colnames(myDF5) <- c("Date", "SampleNumber", "ring", "plot", "depth", "pH", "gmc", "totC", "totN", "totP_ppm")
    
    myDF <- rbind(myDF2, myDF3, myDF4, myDF5)
    myDF$Date <- dmy(myDF$Date)
    
    ### get rid of spaces in the variable "Depth"
    myDF$depth <- as.character(myDF$depth)
    myDF$depth <- factor(gsub(" ", "", myDF$depth, fixed = TRUE)) 
    
    ### note that all data in 2015 are missing. Remove them.
    myDF <- subset(myDF,Date<as.Date("2015-01-01"))
    
    ### merge soil C with bulk density
    mydat <- merge(myDF,bk_density,by.x=c("depth", "ring"), by.y=c("Depth", "ring"))

    
    ### calculate soil C content of each layer. Units of g C m-2 for each 10-cm long depth increment
    ###  Note that the 10-20cm and 20-30cm layers were only measured on 3 of the 15 dates.
    ###   These deeper layers have less C than the 0-10cm layer.
    mydat$totC_g_m2 <- with(mydat,totC/100*bulk_density_kg_m3*0.1*1000) # convert to gC m-2
    
    ### get averages for the deeper depths
    dat.m.deep <- summaryBy(totC_g_m2~ring+plot+depth,data=mydat,FUN=mean,keep.names=T,na.rm=T)
    
    ### set up an empty dataframe with all potential levels of Date, Plot, Ring, and Depth in "dat"
    dat.empty <- expand.grid(depth=levels(mydat$depth),plot=levels(as.factor(mydat$plot)),
                             ring=levels(as.factor(mydat$ring)),Date=levels(as.factor(mydat$Date)))
    mydat <- merge(mydat,dat.empty,by=c("depth","plot","ring","Date"),all.y=T)
    
    ### loop over the data, if deeper data are missing, gapfill with the average for that plot
    naflag <- NA
    for (i in 1:nrow(mydat)){
        naflag <- is.na(mydat[i,"totC_g_m2"]) # is the datum missing?
        if(naflag){
            Depth_id <- mydat[i,"depth"]
            Ring_id <- mydat[i,"ring"]
            Plot_id <- mydat[i,"plot"]
            
            id <- which(dat.m.deep$depth==Depth_id & dat.m.deep$ring==Ring_id & dat.m.deep$plot==Plot_id)
            mydat[i,"totC_g_m2"] <- dat.m.deep[id,"totC_g_m2"]
        }
    }
    
    #------
    #- sum across layers on each date, if "return" is "all_depths"
    if(return=="all_depths"){
        dat.s <- summaryBy(totCgm2~Plot+Ring+Date,data=mydat,FUN=sum,keep.names=T)
        names(dat.s)[4] <- "soil_carbon_pool"
        #- average across plots within each ring
        dat.s.m <- summaryBy(soil_carbon_pool~Date+Ring,data=dat.s,FUN=mean,keep.names=T)
        dat.s.m$Ring <- as.numeric(dat.s.m$Ring)
        
    }
    
    #- return only the shallow layer on each date, if "return" is "shallow"
    if(return=="shallow"){
        dat.s <- summaryBy(totC_g_m2~plot+ring+Date,data=subset(mydat,depth=="0-10cm"),FUN=sum,keep.names=T)
        names(dat.s)[4] <- "soil_carbon_pool"
        #- average across plots within each ring
        dat.s.m <- summaryBy(soil_carbon_pool~Date+ring,data=dat.s,FUN=mean,keep.names=T)
        dat.s.m$Ring <- as.numeric(dat.s.m$ring)
        
    }
    
    #- return by depth, ring, if "return" is "by_depths"
    if(return=="by_depths"){
        dat.s <- summaryBy(totCgm2~Plot+Ring+Date+Depth,data=mydat,FUN=sum,keep.names=T)
        names(dat.s)[5] <- "soil_carbon_pool"
        
        
        dat.s.m <- summaryBy(soil_carbon_pool~Date+Ring+Depth,data=dat.s,FUN=mean,keep.names=T)
        dat.s.m$Ring <- as.numeric(dat.s.m$Ring)
        
        dat.s.m <- dat.s.m[,c("Date", "Ring", "soil_carbon_pool", "Depth")]
        
    }
    
    
    return(dat.s.m)
    
}
