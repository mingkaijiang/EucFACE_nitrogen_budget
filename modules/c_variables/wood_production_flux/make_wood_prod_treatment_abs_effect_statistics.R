make_wood_prod_treatment_abs_effect_statistics <- function(inDF, 
                                                         var.col, 
                                                         return.outcome) {
    
    #### Assign amb and ele factor
    for (i in (1:length(inDF$Ring))) {
        if (inDF$Ring[i]==2|inDF$Ring[i]==3|inDF$Ring[i]==6) {
            inDF$Trt[i] <- "amb"
        } else {
            inDF$Trt[i] <- "ele"
        }
    }
    
    #### Assign factors
    inDF$Trt <- as.factor(inDF$Trt)
    inDF$Yr <- year(inDF$Date)
    inDF$Ring <- as.factor(inDF$Ring)
    inDF$Datef <- as.factor(inDF$Date)

    #### Update variable name so that this function can be used across different variables
    colnames(inDF)[var.col] <- "Value"
    
    #### dataframe with annual totals in g m-2 yr-1
    ###------ Treatment interacting with date, or not
    ###------ Ring random factor
    ###------ Unit g m-2 yr-1
    
    ## Get year list and ring list
    yr.list <- unique(inDF$Yr)
    tDF <- summaryBy(Value~Trt+Ring+Yr,data=inDF,FUN=sum, keep.names=T)
    tDF$Yrf <- as.factor(tDF$Yr)
    
    ### Loop through data, return annual flux in g m-2 yr-1
    for (i in 1:6) {
        for (j in yr.list) {
            ### averaged over days within a year 
            tmp <- with(inDF[inDF$Ring == i & inDF$Yr == j, ],
                        sum(Value*Days, na.rm=T)/sum(Days, na.rm=T)) * 365 / 1000
            tDF[tDF$Ring == i & tDF$Yr == j, "Value"] <- tmp
        }
    }
    
    ### Pass in covariate values (assuming 1 value for each ring)
    cov2 <- lai_variable[lai_variable$Date<="2013-02-06",]
    covDF2 <- summaryBy(lai_variable~Ring, data=cov2, FUN=mean, keep.names=T)
    
    ### Read initial basal area data
    f12 <- read.csv("temp_files/EucFACE_dendrometers2011-12_RAW.csv")
    f12$ba <- ((f12$X20.09.2012/2)^2) * pi
    baDF <- summaryBy(ba~Ring, data=f12, FUN=sum, na.rm=T, keep.names=T)
    
    ### return in unit of cm2/m2, which is m2 ha-1
    baDF$ba_ground_area <- baDF$ba / FACE_ring_area
    
    for (i in 1:6) {
        tDF$Cov2[tDF$Ring==i] <- covDF2$lai_variable[covDF2$Ring==i]
        tDF$Cov3[tDF$Ring==i] <- baDF$ba_ground_area[baDF$Ring==i]
    }

    
    ### Analyse the variable model
    ## model 1: no interaction, year as factor, ring random factor
    int.m1 <- "non-interative_with_covariate"
    modelt1 <- lmer(Value~Trt + Yrf + Cov2 + (1|Ring), data=tDF)
    
    ## anova
    m1.anova <- Anova(modelt1, test="F")
    
    ## Check ele - amb diff
    summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size1 <- coef(modelt1)[[1]][1,2]
    
    ## confidence interval 
    eff.conf1 <- confint(modelt1,"Trtele")
    
    
    out <- list(int.state=int.m1,
                mod = modelt1, 
                anova = m1.anova,
                diff = summ1,
                eff = eff.size1,
                conf = eff.conf1)
    
    
    ### Predict the model with a standard LAI value
    newDF <- tDF
    newDF$Cov2 <- mean(covDF2$lai_variable)
    newDF$predicted <- predict(out$mod, newdata=newDF)
    
    
    
    if (return.outcome == "model") {
        return(out)
    } else if (return.outcome == "predicted") {
        return(newDF)
    }
}
