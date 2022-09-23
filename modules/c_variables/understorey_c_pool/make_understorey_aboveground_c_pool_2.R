make_understorey_aboveground_c_pool_camera <- function(c_frac, plot.option) {
    
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
    
    ### prepare live percentage
    pctDF <- make_understorey_percent_live_estimate()
    
    ### prepare a large date series
    date.list <- unique(c(unique(pctDF$Date), unique(inDF$Date)))
    
    ### prepare an empty space
    tmpDF <- data.frame("Date" = rep(date.list, each=6),
                        "Ring" = rep(c(1:6), length(date.list)))
    
    tmpDF <- merge(tmpDF, pctDF, by=c("Ring", "Date"),
                   all.x=T)
    
    tmpDF <- tmpDF[order(tmpDF$Ring, tmpDF$Date),]
    
    for (i in 1:6) {
        tmpDF[tmpDF$Ring == i, "percent_live"] <- na.interpolation(tmpDF[tmpDF$Ring == i, "percent_live"])
    }
    
 
    
    ### merge with the C DF
    outDF <- merge(inDF, tmpDF, by=c("Date", "Ring"))
    outDF$Live_g_C_m2 <- with(outDF, percent_live*Total_g_C_m2)
    outDF$Dead_g_C_m2 <- with(outDF, (1-percent_live)*Total_g_C_m2)
    
    ### out DF
    out <- outDF[,c("Date", "Ring", "Live_g_C_m2", "Dead_g_C_m2", "Total_g_C_m2")]
    
    
    return(out)
}