download_understorey_aboveground_data <- function() {
    
    infile1 <- "FACE_P0061_RA_PATHARE_UNDERSTORY_ABOVEGROUND_BIOMASS_L2_20150201_20160730.csv"
    
    infile2 <- "FACE_TLAPSE_MASSALL_L2_RA_20150202-20170308.csv"
    
    infile3 <- "FACE_TLAPSE_15DGROWTH_L2_RA_20140801-20170331.csv"
    
    # Varsha's harvest data
    if (!file.exists(paste0("download/", infile1))) {
        downloadHIEv(hiev=searchHIEv(infile1))
    }

    # Matthias's stereo camera estimates
    if (!file.exists(paste0("download/", infile2))) {
        downloadCSV(infile2)
    }
    
    # Luke's growth estimate based on change in % cover / 15 day window
    if (!file.exists(paste0("download/", infile3))) {
        downloadCSV(infile3)
    }
}
