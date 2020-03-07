make_understorey_aboveground_c_pool_2 <- function(c_frac) {
    ### download data
    download_understorey_aboveground_data()
    
    ### read in the data 
    inDF <- read.csv(file.path(getToPath(), 
                               "FACE_TLAPSE_MASSALL_L2_RA_20150202-20170308.csv"))
    
    ### convert from kg to g
    inDF$Total_g_m2 <- inDF$AGBpred * 1000
    inDF$Total_g_C_m2 <- inDF$Total_g_m2 * c_frac
    
    ### ring stuffs
    inDF$ring <- gsub("ring1", "1", inDF$ring)
    inDF$ring <- gsub("ring2", "2", inDF$ring)
    inDF$ring <- gsub("ring3", "3", inDF$ring)
    inDF$ring <- gsub("ring4", "4", inDF$ring)
    inDF$ring <- gsub("ring5", "5", inDF$ring)
    inDF$ring <- gsub("ring6", "6", inDF$ring)
    
    ### process class
    inDF$Date <- as.Date(as.character(inDF$Date), format="%d/%m/%Y")
    inDF$Ring <- as.numeric(inDF$ring)
    
    ### out DF
    out <- inDF[,c("Date", "Ring", "Total_g_C_m2")]
    
    return(out)
}


