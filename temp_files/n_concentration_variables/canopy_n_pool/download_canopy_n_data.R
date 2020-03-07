download_canopy_n_data <- function(){

    infile1 <- "FACE_P0020_RA_LeafN_L2_20130213-20131115.csv"
    infile2 <- "FACE_P0020_RA_LeafN_L2_20140130-20140502.csv"
    infile3 <- "FACE_P0020_RA_LeafN_L2_20150201-20180201.csv"
    
    
    if(!file.exists(paste0("download/", infile1))) {
    downloadHIEv(hiev=searchHIEv(infile1))
    }
    
    if(!file.exists(paste0("download/", infile2))) {
        downloadHIEv(hiev=searchHIEv(infile2))
    }
    
    if(!file.exists(paste0("download/", infile3))) {
        downloadHIEv(hiev=searchHIEv(infile3))
    }
}
