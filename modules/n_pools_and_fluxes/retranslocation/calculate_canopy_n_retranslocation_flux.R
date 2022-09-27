calculate_canopy_n_retranslocation_flux <- function (tflux,
                                                     lflux,
                                                     retransDF) {
    
    ### merge the two
    myDF <- merge(tflux, lflux, by=c("Date", "Ring"))
    
    myDF$canopy_n_retrans_flux <- with(myDF, canopy_n_flux - leaflitter_n_flux_mg_m2_d)
    myDF$canopy_n_retrans_coef <- with(myDF, canopy_n_retrans_flux/canopy_n_flux)
    
    subDF <- summaryBy(canopy_n_retrans_coef~Ring, FUN=mean,
                       data=myDF, na.rm=T, keep.names=T)
    
    subDF$Method <- "flux"
    
    ### process retransDF
    subDF2 <- retransDF[,c("Ring", "retrans_coef")]
    
    myDF2 <- merge(myDF, subDF2, by=c("Ring"))
    myDF2$canopy_n_retrans_flux2 <- with(myDF2, leaflitter_n_flux_mg_m2_d/retrans_coef)
    
    
    colnames(subDF2) <- c("Ring", "canopy_n_retrans_coef")
    subDF2$Method <- "conc"
    
    
    ### make plot
    #plotDF <- rbind(subDF, subDF2)
    #plotDF$Trt <- "aCO2"
    #plotDF$Trt[plotDF$Ring%in%c(1,4,5)] <- "eCO2"
    #plotDF2 <- summaryBy(canopy_n_retrans_coef~Method+Trt, data=plotDF, na.rm=T, FUN=c(mean,sd),
    #                     keep.names=T)
    #
    #p1 <- ggplot(plotDF2, aes(x=Trt, y=canopy_n_retrans_coef.mean, group=Method)) +
    #    geom_bar(aes(fill=Method), position="dodge", stat="identity")
    #
    #pdf("plots_tables/checks/canopy_n_retranslocation_comparison.pdf")
    #plot(p1)
    #dev.off()
    
    
    
    ### prepare retranslocation flux
    outDF <- myDF[,c("Date", "Ring", "Start_date.x", "End_date.x",
                     "canopy_n_retrans_flux", "Days.x")]
    
    colnames(outDF) <- c("Date", "Ring", "Start_date", "End_date",
                         "canopy_n_retrans_flux", "Days")
    
    return(outDF)
    
}