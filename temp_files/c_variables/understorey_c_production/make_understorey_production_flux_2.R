make_understorey_aboveground_production_flux_2 <- function(c_frac) {
    
    ### currently only Varsha's harvest data on HIEv
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
    
    #- count number of days between two dates  
    d <- unique(inDF$Date)
    b <- count_ndays(d)
    
    #- convert into mg m-2 d-1
    inDF$ndays <- rep(b, each = 6)
    
    for (i in c(2:length(d))) {
        for (j in c(1:6)) {
            inDF[inDF$Date == d[i] & inDF$ring == j, "diff"] <- inDF[inDF$Date == d[i] & inDF$ring == j, "Total_g_C_m2"] -
                inDF[inDF$Date == d[i-1] & inDF$ring == j, "Total_g_C_m2"]
        }
    }
    
    out <- dplyr::mutate(inDF, 
                         Date = as.Date(inDF$Date, format = "%d/%m/%Y"),
                         Start_date = Date - ndays,
                         End_date = Date,
                         understorey_production_flux = diff * g_to_mg / ndays)
    
    #- drop NA rows
    out <- out[complete.cases(out),]
    df <- out[Reduce(`&`, lapply(out, is.finite)),]
    
    df$ndays <- as.numeric(df$End_date - df$Start_date) + 1
    
    
    #- format dataframe to return
    out <-df[,c("Start_date", "End_date", "Date", "Ring","understorey_production_flux", "ndays")]
    colnames(out) <- c("Start_date", "End_date", "Date", "Ring", "understorey_production_flux", "Days")
    
    return(out)
}
