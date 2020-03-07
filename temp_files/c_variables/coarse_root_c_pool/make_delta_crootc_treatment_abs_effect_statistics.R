make_delta_crootc_treatment_abs_effect_statistics <- function(inDF, 
                                                   var.col, 
                                                   stat.model, return.outcome) {

    ### create delta DF
    deltaDF <- make_yearly_delta_pool_function(inDF, var.col)
    
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
        deltaDF$Cov2[deltaDF$Ring==i] <- covDF2$lai_variable[covDF2$Ring==i]
        deltaDF$Cov3[deltaDF$Ring==i] <- baDF$ba_ground_area[baDF$Ring==i]
    }
    
    #### Assign amb and ele factor
    deltaDF$Trt[deltaDF$Ring%in%c(2,3,6)] <- "amb"
    deltaDF$Trt[deltaDF$Ring%in%c(1,4,5)] <- "ele"
    
    #### Assign factors
    deltaDF$Trt <- as.factor(deltaDF$Trt)
    deltaDF$Ring <- as.factor(deltaDF$Ring)
    deltaDF$Datef <- as.factor(deltaDF$Date)
    
    ## Get year list and ring list
    tDF <- deltaDF
    
    ### Analyse the variable model
    ## model 1: no interaction, year as factor, ring random factor, include pre-treatment effect
    int.m1 <- "non-interative_with_covariate"
    modelt1 <- lmer(delta~Trt+Datef+Cov2 + (1|Ring),data=tDF)
    
    ## anova
    m1.anova <- Anova(modelt1, test="F")
    
    ## Check ele - amb diff
    summ1 <- summary(glht(modelt1, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size1 <- coef(modelt1)[[1]][1,2]
    
    ## confidence interval 
    eff.conf1 <- confint(modelt1,"Trtele")
    
    ### Analyse the variable model
    ## model 2: interaction, year as factor, ring random factor, with pre-treatment
    int.m2 <- "interative_with_covariate"
    modelt2 <- lmer(delta~Trt*Datef+Cov2 + (1|Ring),data=tDF)
    
    ## anova
    m2.anova <- Anova(modelt2, test="F")
    
    ## Check ele - amb diff
    summ2 <- summary(glht(modelt2, linfct = mcp(Trt = "Tukey")))
    
    ## average effect size
    eff.size2 <- coef(modelt2)[[1]][1,2]
    
    ## confidence interval 
    eff.conf2 <- confint(modelt2,"Trtele")
    

    ### conditional output
    if (stat.model == "no_interaction_with_covariate") {
        out <- list(int.state=int.m1,
                    mod = modelt1, 
                    anova = m1.anova,
                    diff = summ1,
                    eff = eff.size1,
                    conf = eff.conf1)
    } else if (stat.model == "interaction_with_covariate") {
        out <- list(int.state=int.m2,
                    mod = modelt2, 
                    anova = m2.anova,
                    diff = summ2,
                    eff = eff.size2,
                    conf = eff.conf2)
    } 
    
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
