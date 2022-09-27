prepare_mip_np_ratio <- function(npDF,
                                 ambDF,
                                 eleDF) {
    
    
    var.list <- c("canopy", "leaflitter", "wood", "fineroot", "soil_0_10")
    
    
    
    ### get dimensions of different variables
    nvar <- length(var.list)
    mod.list <- unique(ambDF$ModName)
    nmod <- length(mod.list)
    
    
    ### create a DF for aCO2 and eCO2
    myDF1 <- data.frame(rep(var.list, (1+nmod)*2), 
                        rep(c("obs", mod.list), each=(nvar*2)), 
                        rep(c("aCO2", "eCO2"), each=nvar), 
                        NA, NA)
    colnames(myDF1) <- c("Variable", 
                         "Group",
                         "Trt",
                         "meanvalue",
                         "sdvalue")
    
    
    ### calculate data based treatment averages and SDs
    tmpDF1 <- reshape2::melt(cnDF, id.var="Ring")
    tmpDF1$Trt <- "eCO2"
    tmpDF1$Trt[tmpDF1$Ring%in%c(2,3,6)] <- "aCO2"
    avgDF <- summaryBy(value~variable+Trt, FUN=c(mean,sd), data=tmpDF1, na.rm=T, keep.names=T)
    
    for (i in var.list) {
        myDF1$meanvalue[myDF1$Variable==i&myDF1$Group=="obs"&myDF1$Trt=="aCO2"] <- avgDF$value.mean[avgDF$variable==i&avgDF$Trt=="aCO2"]
        myDF1$meanvalue[myDF1$Variable==i&myDF1$Group=="obs"&myDF1$Trt=="eCO2"] <- avgDF$value.mean[avgDF$variable==i&avgDF$Trt=="eCO2"]
        myDF1$sdvalue[myDF1$Variable==i&myDF1$Group=="obs"&myDF1$Trt=="aCO2"] <- avgDF$value.sd[avgDF$variable==i&avgDF$Trt=="aCO2"]
        myDF1$sdvalue[myDF1$Variable==i&myDF1$Group=="obs"&myDF1$Trt=="eCO2"] <- avgDF$value.sd[avgDF$variable==i&avgDF$Trt=="eCO2"]
    }
    
    
    ### model based results
    ambDF$canopy <- with(ambDF, NL.mean/PL.mean)
    ambDF$leaflitter <- with(ambDF, NFLITA.mean/PFLITA.mean)
    ambDF$wood <- with(ambDF, NW.mean/PW.mean)
    ambDF$fineroot <- with(ambDF, NFR.mean/PFR.mean)
    ambDF$soil_0_10 <- with(ambDF, NSOIL.mean/PSOIL.mean)

    eleDF$canopy <- with(eleDF, NL.mean/PL.mean)
    eleDF$leaflitter <- with(eleDF, NFLITA.mean/PFLITA.mean)
    eleDF$wood <- with(eleDF, NW.mean/PW.mean)
    eleDF$fineroot <- with(eleDF, NFR.mean/PFR.mean)
    eleDF$soil_0_10 <- with(eleDF, NSOIL.mean/PSOIL.mean)
    
    tmpDF1 <- ambDF[,c("ModName", "canopy", "leaflitter", "wood", "fineroot", "soil_0_10")]
    tmpDF2 <- eleDF[,c("ModName", "canopy", "leaflitter", "wood", "fineroot", "soil_0_10")]
    tmpDF1$Trt <- "aCO2"
    tmpDF2$Trt <- "eCO2"
    
    rsDF1 <- reshape2::melt(tmpDF1, id.var=c("ModName", "Trt"))
    rsDF2 <- reshape2::melt(tmpDF2, id.var=c("ModName", "Trt"))
    
    
    ### add model output
    for (i in mod.list) {
        for (j in var.list) {
            
            ### means
            myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable==j] <- rsDF1$value[rsDF1$ModName==i&rsDF1$variable==j]
            myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable==j] <- rsDF2$value[rsDF2$ModName==i&rsDF2$variable==j]

        }
    }
    
    
    
    ### convert nan to na
    myDF1$meanvalue <- ifelse(is.nan(myDF1$meanvalue), NA, myDF1$meanvalue)
    myDF1$sdvalue <- ifelse(is.nan(myDF1$sdvalue), NA, myDF1$sdvalue)
    
    
    
    return(myDF1)
}