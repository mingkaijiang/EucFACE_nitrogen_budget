make_coarse_root_pool <- function(c_frac, fr_pool) {
    ### Method 1 of making total root biomass pool
    ### Based on Snowdon et al., 2000. National Carbon accounting system:
    ### synthesis of allometrics, review of root biomass and design of future woody biomass sampling strategies.
    ### Australian Greenhouse Office. Technical Report No. 17.
    ### Relationship: ln(root biomass) = 0.787 * ln(stand basal area) + 1.218
    ### Root biomass in t/ha, basal area in m2/ha.
    
    #### three decisions to make:
    #### 1. return.decision: data - return the dataframe, stats - return the stats
    #### 2. If return stats, then need to choose what effect to show (trt.effect) i.e. ratio or abs
    #### 3. Then need to decide which model (stat.model): interaction, no_interaction, dynamic, and no_random_effect
    
    #- download the data from HIEv
    download_diameter_data()
    
    #- read in 2012-15 data sets
    f13 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2012-13_RAW-V1.csv"))
    f14 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2013-14_RAW_V1.csv"))
    f15 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2015_RAW_V1.csv"))
    f16 <- read.csv(file.path(getToPath(), "FACE_P0025_RA_TREEMEAS_2016_RAW_V1.csv"))
    
    # this file is not on HIEv yet!
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    
    #### Read in additional files that I used when doing the data analysis
    classif <- read.csv("download/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)
    classif$Active.FALSE.means.dead.[classif$Tree == 608] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 125] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 206] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 210] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 212] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 510] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 518] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 520] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 524] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 527] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 531] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 605] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 615] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 616] <- FALSE  # This tree dead
    classif$Active.FALSE.means.dead.[classif$Tree == 617] <- FALSE  # This tree dead
    #classif$Active.FALSE.means.dead.[classif$Tree == 101] <- FALSE  # This tree dead in 2018
    #classif$Active.FALSE.means.dead.[classif$Tree == 219] <- FALSE  # This tree dead in 2018
    #classif$Active.FALSE.means.dead.[classif$Tree == 220] <- FALSE  # This tree dead in 2018
    #classif$Active.FALSE.means.dead.[classif$Tree == 621] <- FALSE  # This tree dead in 2018
    
    
    #### Merge the files
    all <- merge(classif,f12,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f13,by=c("Tree","Ring","CO2.trt")) 
    all <- merge(all,f14,by=c("Tree","Ring","CO2.trt"))  
    all <- merge(all,f15,by=c("Tree","Ring","CO2.trt"))
    all <- merge(all,f16,by=c("Tree","Ring","CO2.trt"))
    
    #### remove dead trees
    all$Active.FALSE.means.dead.[is.na(all$Active.FALSE.means.dead.)] <- "TRUE"
    all <- subset(all, Active.FALSE.means.dead.== TRUE)
    #all <- all[complete.cases(all),]
    
    #### remove "CORR" columns and dead column
    uncorr <- all[,-grep("CORR",names(all))]
    uncorr <- uncorr[,-grep("Coor",names(uncorr))]
    uncorr <- uncorr[,names(uncorr) != "Active.FALSE.means.dead."]
    
    #### make a long-form version of dataframe
    long <- reshape(uncorr,idvar="Tree",varying=list(7:58),direction="long")
    dates <- names(uncorr)[7:58]
    long$Date <- c(rep(Sys.Date(),length(long$time)))  #wasn't sure how else to make this column date type
    for (i in (1:length(long$time))) {
        long$Date[i] <- as.Date(dates[long$time[i]],format="X%d.%m.%Y")
    }
    long <- renameCol(long,c("X17.02.2011"),c("diam"))
    
    long$diam <- as.numeric(long$diam)
    
    # Calculate Basal area, m2
    long$ba <- 0.00007854 * (long$diam)^2 
    
    dates <- c(as.Date("2012-12-20"),as.Date("2013-12-20"),
               as.Date("2014-12-23"),as.Date("2015-12-14"),
               as.Date("2016-12-21"))
    data <- long[long$Date %in% dates,]
    
    data$ba2 <- data$ba 
    data$biomass <- exp(0.787 * log(data$ba2) + 1.218) 
    
    # convert from g matter m-2 to g C m-2
    data$total_root_c_pool <- data$biomass * c_frac * water_frac * 1000000
    
    
    # sum across rings and dates
    data.m <- summaryBy(ba~Date+Ring,data=data,FUN=sum,keep.names=T,na.rm=T)
    
    # convert into m2/ha
    data.m$ba2 <- data.m$ba / (FACE_ring_area / 10000)
    
    # Calculate root biomass, return in t/ha, then convert to g/m2
    data.m$biomass <- exp(0.787 * log(data.m$ba2) + 1.218) * 100
    
    # convert from g matter m-2 to g C m-2
    data.m$total_root_c_pool <- data.m$biomass * c_frac * water_frac
    
    # subtract fineroot biomass out, assuming one froot value per ring
    fr.ring <- summaryBy(fineroot_pool~Ring, data=fr_pool, FUN=mean, keep.names=T, na.rm=T)
    for (i in 1:6) {
        data.m$coarseroot_c_pool[data.m$Ring == i] <- data.m$total_root_c_pool[data.m$Ring == i] - fr.ring$fineroot_pool[fr.ring$Ring == i]
        
    }
    
    # output
    cr_pool <- data.m[,c("Date","Ring","coarseroot_c_pool", "total_root_c_pool")]
    
    colnames(cr_pool) <- c("Date", "Ring", "coarse_root_pool", "total_root_pool")
    
    # Only use data period 2012-2016
    cr_pool <- cr_pool[cr_pool$Date<="2016-12-31",]
    
    ### Decision on what to return
    return(cr_pool)
    
}