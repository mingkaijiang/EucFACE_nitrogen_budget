download_mortality_data <- function(){
    
    infile1 <- "FACE_P0045_RA_MORTALITY_RAW_20150501_v1.csv"
    
    if (!file.exists(paste0("download/", infile1))) {
        downloadHIEv(hiev=searchHIEv(infile1))
    }
    
}
