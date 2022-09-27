prepare_mip_veg_growth_n_flux <- function(nfluxDF,
                                          ambDF,
                                          eleDF,
                                          difDF,
                                          var.list,
                                          calculate.total) {
    
    ### Purpose:
    ### to prepare annual DF for plotting
    ### for fluxs, if calculate.total = T, we will calculate the total of the fluxs
    
    
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
    
    ### add obs data
    #for (i in c("aCO2", "eCO2")) {
    #    for (j in var.list) {
    #        
    #        tryCatch({
    #            myDF1$meanvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable==j] <- eucDF[eucDF$Group=="mean"&eucDF$Trt==i, j]
    #            myDF1$sdvalue[myDF1$Group=="obs"&myDF1$Trt==i&myDF1$Variable==j] <- eucDF[eucDF$Group=="sd"&eucDF$Trt==i, j]
    #        }, error=function(e){})
    #        
    #    }
    #}
    
    ### add model output
    for (i in mod.list) {
        for (j in var.list) {
            
            ### means
            myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable==j] <- ambDF[ambDF$ModName==i, paste0(j, ".mean")]
            myDF1$meanvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable==j] <- eleDF[eleDF$ModName==i, paste0(j, ".mean")]
            
            ### sd
            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt=="aCO2"&myDF1$Variable==j] <- ambDF[ambDF$ModName==i, paste0(j, ".sd")]
            myDF1$sdvalue[myDF1$Group==i&myDF1$Trt=="eCO2"&myDF1$Variable==j] <- eleDF[eleDF$ModName==i, paste0(j, ".sd")]
        }
    }
    
    myDF1$meanvalue[myDF1$Variable=="NGL"&myDF1$Group=="obs"&myDF1$Trt=="aCO2"] <- nfluxDF$aCO2[nfluxDF$terms=="Canopy N flux"]
    myDF1$meanvalue[myDF1$Variable=="NGL"&myDF1$Group=="obs"&myDF1$Trt=="eCO2"] <- nfluxDF$eCO2[nfluxDF$terms=="Canopy N flux"]
    myDF1$sdvalue[myDF1$Variable=="NGL"&myDF1$Group=="obs"&myDF1$Trt=="aCO2"] <- nfluxDF$aCO2_sd[nfluxDF$terms=="Canopy N flux"]
    myDF1$sdvalue[myDF1$Variable=="NGL"&myDF1$Group=="obs"&myDF1$Trt=="eCO2"] <- nfluxDF$eCO2_sd[nfluxDF$terms=="Canopy N flux"]
    
    
    myDF1$meanvalue[myDF1$Variable=="NGW"&myDF1$Group=="obs"&myDF1$Trt=="aCO2"] <- nfluxDF$aCO2[nfluxDF$terms=="Wood N flux"]
    myDF1$meanvalue[myDF1$Variable=="NGW"&myDF1$Group=="obs"&myDF1$Trt=="eCO2"] <- nfluxDF$eCO2[nfluxDF$terms=="Wood N flux"]
    myDF1$sdvalue[myDF1$Variable=="NGW"&myDF1$Group=="obs"&myDF1$Trt=="aCO2"] <- nfluxDF$aCO2_sd[nfluxDF$terms=="Wood N flux"]
    myDF1$sdvalue[myDF1$Variable=="GNW"&myDF1$Group=="obs"&myDF1$Trt=="eCO2"] <- nfluxDF$eCO2_sd[nfluxDF$terms=="Wood N flux"]
    
    myDF1$meanvalue[myDF1$Variable=="NGCR"&myDF1$Group=="obs"&myDF1$Trt=="aCO2"] <- nfluxDF$aCO2[nfluxDF$terms=="Coarse Root N flux"]
    myDF1$meanvalue[myDF1$Variable=="NGCR"&myDF1$Group=="obs"&myDF1$Trt=="eCO2"] <- nfluxDF$eCO2[nfluxDF$terms=="Coarse Root N flux"]
    myDF1$sdvalue[myDF1$Variable=="NGCR"&myDF1$Group=="obs"&myDF1$Trt=="aCO2"] <- nfluxDF$aCO2_sd[nfluxDF$terms=="Coarse Root N flux"]
    myDF1$sdvalue[myDF1$Variable=="NGCR"&myDF1$Group=="obs"&myDF1$Trt=="eCO2"] <- nfluxDF$eCO2_sd[nfluxDF$terms=="Coarse Root N flux"]
    
    
    myDF1$meanvalue[myDF1$Variable=="NGFR"&myDF1$Group=="obs"&myDF1$Trt=="aCO2"] <- nfluxDF$aCO2[nfluxDF$terms=="Fine Root N flux"]
    myDF1$meanvalue[myDF1$Variable=="NGFR"&myDF1$Group=="obs"&myDF1$Trt=="eCO2"] <- nfluxDF$eCO2[nfluxDF$terms=="Fine Root N flux"]
    myDF1$sdvalue[myDF1$Variable=="NGFR"&myDF1$Group=="obs"&myDF1$Trt=="aCO2"] <- nfluxDF$aCO2_sd[nfluxDF$terms=="Fine Root N flux"]
    myDF1$sdvalue[myDF1$Variable=="NGFR"&myDF1$Group=="obs"&myDF1$Trt=="eCO2"] <- nfluxDF$eCO2_sd[nfluxDF$terms=="Fine Root N flux"]
    
    
    ### convert nan to na
    myDF1$meanvalue <- ifelse(is.nan(myDF1$meanvalue), NA, myDF1$meanvalue)
    myDF1$sdvalue <- ifelse(is.nan(myDF1$sdvalue), NA, myDF1$sdvalue)
    
    
    
    
    ### get the diff and % diff DF
    myDF2 <- data.frame(rep(var.list, (1+nmod)*2), 
                        rep(c("obs", mod.list), each=(nvar*2)), 
                        rep(c("diff", "pct_diff"), each=nvar), 
                        NA, NA)
    colnames(myDF2) <- c("Variable", 
                         "Group",
                         "Trt",
                         "meanvalue",
                         "sdvalue")
    
    ### add obs data
    #for (j in var.list) {
    #    
    #    tryCatch({
    #        myDF2$meanvalue[myDF2$Group=="obs"&myDF2$Trt=="diff"&myDF2$Variable==j] <- eucDF[eucDF$Group=="mean"&eucDF$Trt=="eCO2", j] - eucDF[eucDF$Group=="mean"&eucDF$Trt=="aCO2", j]
    #        myDF2$sdvalue[myDF2$Group=="obs"&myDF2$Trt=="diff"&myDF2$Variable==j] <- sqrt((eucDF[eucDF$Group=="sd"&eucDF$Trt=="eCO2", j]^2+eucDF[eucDF$Group=="sd"&eucDF$Trt=="aCO2", j]^2)/2)
    #        
    #        myDF2$meanvalue[myDF2$Group=="obs"&myDF2$Trt=="pct_diff"&myDF2$Variable==j] <- eucDF[eucDF$Group=="mean"&eucDF$Trt=="pct_diff", j]
    #        myDF2$sdvalue[myDF2$Group=="obs"&myDF2$Trt=="pct_diff"&myDF2$Variable==j] <- eucDF[eucDF$Group=="sd"&eucDF$Trt=="pct_diff", j]
    #    }, error=function(e){})
    #    
    #}
    
    
    ### add model output
    for (i in mod.list) {
        for (j in var.list) {
            
            ### means
            myDF2$meanvalue[myDF2$Group==i&myDF2$Trt=="diff"&myDF2$Variable==j] <- eleDF[eleDF$ModName==i, paste0(j, ".mean")]-ambDF[ambDF$ModName==i, paste0(j, ".mean")]
            myDF2$meanvalue[myDF2$Group==i&myDF2$Trt=="pct_diff"&myDF2$Variable==j] <- difDF[difDF$ModName==i, paste0(j, ".mean")]
            
            ### sd
            myDF2$sdvalue[myDF2$Group==i&myDF2$Trt=="diff"&myDF2$Variable==j] <- sqrt((ambDF[ambDF$ModName==i, paste0(j, ".sd")]^2+eleDF[eleDF$ModName==i, paste0(j, ".sd")]^2)/2)
            myDF2$sdvalue[myDF2$Group==i&myDF2$Trt=="pct_diff"&myDF2$Variable==j] <- difDF[difDF$ModName==i, paste0(j, ".sd")]
        }
    }
    
    
    myDF2$meanvalue[myDF2$Variable=="NGL"&myDF2$Group=="obs"&myDF2$Trt=="diff"] <- nfluxDF$diff[nfluxDF$terms=="Canopy N flux"]
    myDF2$meanvalue[myDF2$Variable=="NGL"&myDF2$Group=="obs"&myDF2$Trt=="pct_diff"] <- nfluxDF$percent_diff[nfluxDF$terms=="Canopy N flux"]

    
    myDF2$meanvalue[myDF2$Variable=="NGW"&myDF2$Group=="obs"&myDF2$Trt=="diff"] <- nfluxDF$diff[nfluxDF$terms=="Wood N flux"]
    myDF2$meanvalue[myDF2$Variable=="NGW"&myDF2$Group=="obs"&myDF2$Trt=="pct_diff"] <- nfluxDF$percent_diff[nfluxDF$terms=="Wood N flux"]
    
    
    myDF2$meanvalue[myDF2$Variable=="NGFR"&myDF2$Group=="obs"&myDF2$Trt=="diff"] <- nfluxDF$diff[nfluxDF$terms=="Fine Root N flux"]
    myDF2$meanvalue[myDF2$Variable=="NGFR"&myDF2$Group=="obs"&myDF2$Trt=="pct_diff"] <- nfluxDF$percent_diff[nfluxDF$terms=="Fine Root N flux"]
    
    
    myDF2$meanvalue[myDF2$Variable=="NGCR"&myDF2$Group=="obs"&myDF2$Trt=="diff"] <- nfluxDF$diff[nfluxDF$terms=="Coarse Root N flux"]
    myDF2$meanvalue[myDF2$Variable=="NGCR"&myDF2$Group=="obs"&myDF2$Trt=="pct_diff"] <- nfluxDF$percent_diff[nfluxDF$terms=="Coarse Root N flux"]
    
    
    
    ### convert nan to na
    myDF2$meanvalue <- ifelse(is.nan(myDF2$meanvalue), NA, myDF2$meanvalue)
    myDF2$sdvalue <- ifelse(is.nan(myDF2$sdvalue), NA, myDF2$sdvalue)
    
    
    ### calculate total
    if (calculate.total==T) {
        
        ### calculate total of aCO2 and eCO2 first
        totDF <- summaryBy(meanvalue~Group+Trt, FUN=sum, data=myDF1,
                           na.rm=T, keep.names=T)
        
        ### calculate sd
        for (i in unique(myDF1$Group)) {
            for (j in c("aCO2", "eCO2")) {
                sd.values <- myDF1$sdvalue[myDF1$Group==i&myDF1$Trt==j]
                totDF$sdvalue[totDF$Group==i&totDF$Trt==j] <- sqrt(sum(sd.values^2, na.rm=T)/nvar)
            }
        }
        
        ### now calculate the total difference between treatment
        tmpDF1 <- subset(totDF, Trt=="aCO2")
        tmpDF2 <- subset(totDF, Trt=="eCO2")
        tmpDF3 <- merge(tmpDF1, tmpDF2, by=c("Group"))
        tmpDF3$diff.mean <- with(tmpDF3, meanvalue.y-meanvalue.x)
        tmpDF3$diff.sd <- sqrt((tmpDF3$sdvalue.x^2+tmpDF3$sdvalue.y^2)/2)
        
        ### calculate pct diff mean and sd
        tmpDF3$pct.diff.mean <- with(tmpDF3, (meanvalue.y-meanvalue.x)/meanvalue.x*100)
        
        tmpDF3$pct.diff.sd <- sqrt((tmpDF3$sdvalue.x^2+tmpDF3$sdvalue.x^2+tmpDF3$sdvalue.y^2)/3)/tmpDF3$meanvalue.x*100 
        
        ### prepare outDF
        tmpDF3$Variable <- "Tot"
        
        subDF1 <- tmpDF3[,c("Variable", "Group", "diff.mean", "diff.sd")]
        subDF2 <- tmpDF3[,c("Variable", "Group", "pct.diff.mean", "pct.diff.sd")]
        colnames(subDF1) <- colnames(subDF2) <- c("Variable", "Group", "meanvalue", "sdvalue")
        subDF1$Trt <- "diff"
        subDF2$Trt <- "pct_diff"
        
        myDF3 <- rbind(subDF1, subDF2)
        myDF3 <- myDF3[,c("Variable", "Group", "Trt", "meanvalue", "sdvalue")]
        
        totDF$Variable <- "Tot"
        totDF <- totDF[,c("Variable", "Group", "Trt", "meanvalue", "sdvalue")]
        
        myDF3 <- rbind(myDF3, totDF)
    }
    
    
    
    ### return
    if (calculate.total==T) {
        outDF <- rbind(myDF1, rbind(myDF2, myDF3))
    } else {
        outDF <- rbind(myDF1, myDF2)
        
    }
    
    return(outDF)
}