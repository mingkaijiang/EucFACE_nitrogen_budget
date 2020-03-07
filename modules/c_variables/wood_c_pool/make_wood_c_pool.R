
# Function to calculate biomass (kg) corresponding to diameter (cm)
# Using relationship taken from Paul et al. (2013) Forest Ecol. Manag. Volume 310.
allom_agb <- function(diamcm) {
    exp(-2.15 + 2.34*log(diamcm))
}

# Make the live wood C pool
make_wood_c_pool <- function(ring_area, c_frac){
    
    #### download the data from HIEv
    download_diameter_data()
    
    #### read in 2012-15 data sets
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
    
    #### add biomass to long-form dataframe
    long$biom <- allom_agb(long$diam)  # in kg DM
    
    #### The bark removal affects the diameters mid-year. 
    #### Hence, just calculate biomass once per year 
    #### Specify dates here - may update this to March in future
    dates <- c(as.Date("2012-12-20"),as.Date("2013-12-20"),
               as.Date("2014-12-23"),as.Date("2015-12-14"),
               as.Date("2016-12-21"))
    data <- long[long$Date %in% dates,]
    
    #dates <- data.frame(c(2012:2016), NA)
    #colnames(dates) <- c("yr", "date")
    #long$yr <- year(long$Date)
    #for (i in 2012:2016) {
    #    test <- long[long$Ring==1 & long$yr == i, ]
    #    r.n <- which.max(test$biom)
    #    dates$date[dates$yr==i] <- test[r.n, "Date"]
    #}
    #
    #data <- long[long$Date %in% dates$date,]
    
    ### Update unit, gram
    data$biom_g <- data$biom * 1000
    
    ### Obtain DBH (cm) and Sapwood depth (mm) relationship
    rlt <- make_dbh_sapwood_rlt()
    data$sap.depth <- data$diam*rlt[2] + rlt[1]
    data$htwood.diam <- data$diam - (data$sap.depth/10 * 2)
    
    data$ht.biom <- allom_agb(data$htwood.diam) 
    
    #### sum across rings and dates
    data.m <- summaryBy(biom+ht.biom~Date+Ring,data=data,FUN=sum,keep.names=T,na.rm=T)
    
    #### divide by ring area to get biomass per m2
    data.m$wood_pool <- data.m$biom / ring_area
    
    #### Heart wood and sapwood pool
    data.m$heart_pool <- data.m$ht.biom / ring_area
    data.m$sap_pool <- data.m$wood_pool - data.m$heart_pool
    
    #### Estimate sapwood and heartwood C fraction
    sap.c <- make_sapwood_c_n_fraction()
    data.m$sap_c_frac[data.m$Ring %in% c(2, 3, 6)] <- sap.c$aCO2[sap.c$variable=="C"]
    data.m$sap_c_frac[data.m$Ring %in% c(1, 4, 5)] <- sap.c$eCO2[sap.c$variable=="C"]
    
    #### convert from kg DM m-2 to g C m-2
    data.m$heart_pool <- data.m$heart_pool * c_frac * 1000
    data.m$sap_pool <- data.m$sap_pool * data.m$sap_c_frac * 1000
    data.m$wood_pool <- data.m$sap_pool + data.m$heart_pool
    
    #### format dataframe to return
    wood_pool <- data.m[,c("Date","Ring","wood_pool", "sap_pool", "heart_pool")]
    
    #### Only use data period 2012-2016
    wood_pool <- wood_pool[wood_pool$Date<="2016-12-31",]
    
    return(wood_pool)
    
}