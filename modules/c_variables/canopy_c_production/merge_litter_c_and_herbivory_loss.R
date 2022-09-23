merge_litter_c_and_herbivory_loss <- function(litter,
                                              herbivory) {
    
    
    ### herbivory data covers limited time points during 2012 - 2014
    ### leaf litter data covers more dates during 2012 - 2016
    ### we need to expand the herbivory data by extrapolating a relationship
    
    ### simple approach, revise the date in the herbivory because they are not very far apart
    herbivory$Date[herbivory$Date=="2012-12-10"] <- "2012-12-11"
    herbivory$Date[herbivory$Date=="2013-01-09"] <- "2013-01-08"
    herbivory$Date[herbivory$Date=="2013-08-16"] <- "2013-08-15"
    herbivory$Date[herbivory$Date=="2013-09-13"] <- "2013-09-10"
    herbivory$Date[herbivory$Date=="2013-11-14"] <- "2013-11-11"
    herbivory$Date[herbivory$Date=="2013-12-13"] <- "2013-12-12"
    herbivory$Date[herbivory$Date=="2014-03-14"] <- "2014-03-13"
    herbivory$Date[herbivory$Date=="2014-05-08"] <- "2014-05-07"
    herbivory$Date[herbivory$Date=="2014-08-13"] <- "2014-08-14"
    herbivory$Date[herbivory$Date=="2014-09-11"] <- "2014-09-15"
    
    herbivory$Start_date <- NULL
    herbivory$End_date <- NULL
    herbivory$ndays <- NULL
    
    myDF <- merge(litter, herbivory, by=c("Date", "Ring"),all.x=T)
    
    
    ### split into two time periods
    myDF1 <- myDF[myDF$Date<="2014-10-16",]
    myDF2 <- myDF[myDF$Date>"2014-10-16",]
    
    
    ### calculate ring specific proportions
    myDF1$Month <- month(myDF1$Date)
    myDF2$Month <- month(myDF2$Date)
    
    myDF1$prop <- with(myDF1, herbivory_leaf_consumption_flux/leaf_flux)
    
    
    for (i in 1:6) {
        myDF1[myDF1$Ring == i, "herbivory_leaf_consumption_flux"] <- na_interpolation(myDF1[myDF1$Ring == i, "herbivory_leaf_consumption_flux"])
    }
    
    ### calculate month and ring specific proportion and fill gaps
    prop <- summaryBy(prop~Ring+Month, FUN=c(mean,sd), data=myDF1, na.rm=T, keep.names=T)
    
    for (i in 1:6) {
        prop$prop.mean[prop$Ring==i] <- na_interpolation(prop$prop.mean[prop$Ring==i])
    }
    
    ### add info to myDF2
    for (i in 1:6) {
        for (j in 1:12) {
            myDF2$prop[myDF2$Ring==i&myDF2$Month==j] <- prop$prop.mean[prop$Ring==i&prop$Month==j]
        }
    }
    
    myDF2$herbivory_leaf_consumption_flux <- myDF2$leaf_flux*myDF2$prop
    
    outDF <- rbind(myDF1, myDF2)
    
    outDF$Month <- NULL
    outDF$prop <- NULL
    outDF$total_flux <- outDF$leaf_flux+outDF$herbivory_leaf_consumption_flux
    
    
    return(outDF)
    
}