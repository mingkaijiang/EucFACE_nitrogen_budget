make_sapwood_c_n_fraction <- function() {
    ### Download the data
    downloadHIEv(hiev=searchHIEv("FACE_P0079_RA_SAPWOOD_N_RAW_2015-11_v1"))
    
    ### read df
    myDF <- read.csv("download/FACE_P0079_RA_SAPWOOD_N_RAW_2015-11_v1.csv")
    
    ### outdf
    outDF <- data.frame(c("C","N"), NA, NA)
    colnames(outDF) <- c("variable", "aCO2", "eCO2")
    
    ### grep aCO2 and eCO2 DF
    aCO2 <- myDF[grep("amb", myDF$SampleID), ]    
    eCO2 <- myDF[grep("ele", myDF$SampleID), ]    
    
    
    ### assign values
    outDF$aCO2[outDF$variable == "C"] <- mean(aCO2$X.C) / 100
    outDF$eCO2[outDF$variable == "C"] <- mean(eCO2$X.C) / 100
    
    outDF$aCO2[outDF$variable == "N"] <- mean(aCO2$X.N) / 100
    outDF$eCO2[outDF$variable == "N"] <- mean(eCO2$X.N) / 100
    
    return(outDF)
}