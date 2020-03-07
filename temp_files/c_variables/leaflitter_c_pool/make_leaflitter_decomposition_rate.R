make_leaflitter_decomposition_rate <- function() {
    ### Download leaf litter decomposition rate
    ### This data is based on leaf collected outside the ring
    ### Put into mesh bags of different sizes inside the ring
    ### Bag sizes: 2mm (to exclude macroinvertebrates) or 4mm (to allow macroinvertebrates)
    ### Here I am using 2 mm because rates are statistically the same
    ### and 2mm data has more temporal coverage
    
    download_leaflitter_decomposition_data()
    
    ### read in data
    ### InitialMass.g -- Initial dry mass of litter, in grams, when bag constructed 
    ### RemainingMass.g -- Final dry mass of litter, in grams, when bag harvested
    myDF <- read.csv(file.path(getToPath(), 
                               "FACE_P0030_RA_LITTER_L2_20130517-20150517.csv"))
    
    ### include only mesh bag of 2 mm
    myDF <- subset(myDF, Mesh == "2mm")
    
    ### Convert into per day
    myDF$Time_d <- myDF$Time * 30
    
    ### Add initial mass
    aDF <- data.frame(rep(1:6, each=8), rep(1:4, each=2), rep(0, 48), rep(1:2, 24), NA, NA, NA, NA)
    colnames(aDF) <- names(myDF)
    aDF$Mesh <- "2mm"    
    aDF$RemainingMass.g <- aDF$InitialMass.g <- myDF[1:48, "InitialMass.g"]
    aDF$Time_d <- 0
    myDF <- rbind(aDF, myDF)
    
    ### adding mass loss percentage 
    myDF$MassLoss <- (myDF$InitialMass.g - myDF$RemainingMass.g)/myDF$InitialMass.g * 100
    myDF$MassRemain <- myDF$RemainingMass.g/myDF$InitialMass.g * 100
    myDF$LogMassRemain <- log(myDF$MassRemain)
    
    ### Visual inspection
    #with(myDF, plot(MassLoss~Time_d))
    #with(myDF, plot(MassRemain~Time_d))
    
    ### Fit power model (y = ax^b) with mass loss
    for (i in 1:6) {
        for (j in unique(myDF$Plot)) {
            for (k in unique(myDF$Rep)) {
                testDF <- subset(myDF, Ring == i & Plot == j & Rep == k)
                mod <- nls(MassLoss~a*Time_d^b,start = list(a = 1, b = 1),data=testDF)
                
                myDF$a[myDF$Ring == i & myDF$Plot == j & myDF$Rep == k] <- coefficients(mod)[[1]]
                myDF$b[myDF$Ring == i & myDF$Plot == j & myDF$Rep == k] <- coefficients(mod)[[2]]
                
                ### plot
                #myDF$NewMassLoss[myDF$Ring == i & myDF$Plot == j & myDF$Rep == k] <- coefficients(mod)[[1]]*testDF$Time_d^coefficients(mod)[[2]]
                #with(myDF[myDF$Ring == i & myDF$Plot == j & myDF$Rep == k,], points(NewMassLoss~Time_d, type="l"))
                
            }
        }
    }
    
    ### Fit model y = exp(-kt+int) with mass remain
    for (i in 1:6) {
        for (j in unique(myDF$Plot)) {
            for (k in unique(myDF$Rep)) {
                testDF <- subset(myDF, Ring == i & Plot == j & Rep == k)
                testDF$LogMassRemain[which(is.nan(testDF$LogMassRemain))] <- NA
                testDF$LogMassRemain[which(testDF$LogMassRemain==-Inf)] <- NA
                testDF$LogMassRemain[which(testDF$LogMassRemain==Inf)] <- NA
                
                mod <- lm(LogMassRemain~Time_d,data=testDF, na.action=na.exclude)
                
                myDF$int[myDF$Ring == i & myDF$Plot == j & myDF$Rep == k] <- coefficients(mod)[[1]]
                myDF$coef[myDF$Ring == i & myDF$Plot == j & myDF$Rep == k] <- coefficients(mod)[[2]]
                
                ### plot
                myDF$Pred[myDF$Ring == i & myDF$Plot == j & myDF$Rep == k] <- exp(coefficients(mod)[[2]]*testDF$Time_d+coefficients(mod)[[1]])
                #with(myDF[myDF$Ring == i & myDF$Plot == j & myDF$Rep == k,], points(Pred~Time_d, type="l"))
                
            }
        }
    }
    
    ### Summary across rings (should test for statistical significance if do it properly)
    outDF <- summaryBy(coef+int~Ring, data=myDF, FUN=mean, na.rm=T, keep.names=T)
    
    
    ### Need to enforce intercept so that all decomposable materials at day 0 = 100 %
    outDF$int <- 4.605
    for (i in 1:6) {
        myDF$Pred2[myDF$Ring == i] <- exp(outDF$coef[outDF$Ring==i]*myDF$Time_d+outDF$int[outDF$Ring==i])
        #with(myDF[myDF$Ring == i,], points(Pred~Time_d, type="l", col="red"))
    }
    
    return(outDF)
}
