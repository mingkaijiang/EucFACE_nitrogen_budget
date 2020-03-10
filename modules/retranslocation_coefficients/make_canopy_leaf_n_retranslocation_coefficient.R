make_canopy_leaf_n_retranslocation_coefficient <- function(df1, df2){
    
    
    df1$year <- year(df1$Date)
    df2$year <- year(df2$Date)
    
    df1.yr <- summaryBy(PercN~Ring+year, data=df1, FUN=mean, keep.names=T)
    df2.yr <- summaryBy(PercN~Ring+year, data=df2, FUN=mean, keep.names=T)
    
    
    retransDF <- data.frame(rep(c(1:6), each=6), rep(c(2012:2017), 6),NA, NA)
    colnames(retransDF) <- c("Ring", "Year", "old", "senesced")
    
    
    for (i in c(2012:2017)) {
        for (j in c(1:6)) {
            retransDF$senesced[retransDF$Ring == j & retransDF$Year == i] <- df2.yr$PercN[df2.yr$Ring == j & df2.yr$year == (i+1)]
        }
    }
    
    for (i in c(2012:2015)) {
        for (j in c(1:6)) {
            retransDF$old[retransDF$Ring == j & retransDF$Year == i] <- df1.yr$PercN[df1.yr$Ring == j & df1.yr$year == i]
        }
    }
    
    
    retransDF$old[retransDF$Ring == "5" & retransDF$Year == "2016"] <- df1.yr$PercN[df1.yr$Ring == "5" & df1.yr$year == "2016"]
    retransDF$old[retransDF$Ring == "3" & retransDF$Year == "2016"] <- df1.yr$PercN[df1.yr$Ring == "3" & df1.yr$year == "2016"]
    retransDF$old[retransDF$Ring == "2" & retransDF$Year == "2016"] <- df1.yr$PercN[df1.yr$Ring == "2" & df1.yr$year == "2016"]
    retransDF$old[retransDF$Ring == "1" & retransDF$Year == "2016"] <- df1.yr$PercN[df1.yr$Ring == "1" & df1.yr$year == "2016"]
    
    
    ### calculate retranslocation coefficient for each ring
    test <- subset(retransDF, Year < "2016")
    test$retrans <- with(test, (old - senesced) / old)
    
    reDF <- summaryBy(retrans~Ring, FUN=mean, keep.names=T, data=test)
    
    ### calculate year 2017
    for (j in c(1:6)) {
        retransDF$old[retransDF$Ring == j & retransDF$Year == "2017"] <- retransDF$senesced[retransDF$Ring == j & retransDF$Year == "2017"] / (1 - reDF$retrans[reDF$Ring==j])
    }
    
    for (j in c(4,6)) {
        retransDF$old[retransDF$Ring == j & retransDF$Year == "2016"] <- retransDF$senesced[retransDF$Ring == j & retransDF$Year == "2016"] / (1 - reDF$retrans[reDF$Ring==j])
        
    }
    
    retransDF$retrans_coef <- (retransDF$old - retransDF$senesced) / retransDF$old 
    
    ### Plot eCO2 effect on retranslocation coefficient
    retransDF$CO2 <- c("eCO2", "aCO2", "aCO2", "eCO2", "eCO2", "aCO2")
    
    
    outDF <- retransDF[,c("Ring", "retrans_coef", "CO2", "Year")]
    
    
    outDF2 <- summaryBy(retrans_coef~Ring, FUN=mean, data=outDF, keep.names=T)
    
    ###
    #sumDF <- summaryBy(retrans_coef~CO2, data=outDF, FUN=mean, keep.names=T)
    
    return(outDF2)
    
}