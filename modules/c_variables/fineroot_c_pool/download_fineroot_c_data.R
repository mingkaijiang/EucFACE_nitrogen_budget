download_fineroot_c_data <- function(){
    
    infile1 <- "FACE_P0083_RA_FR-BIOMASS_L1_20140201-20150915.csv"
    infile2 <- "FACE_P0083_RA_FR-PRODUCTION_L1_20140601-20150915.csv"
    infile3 <- "FACE_PILOTROOT_RA_FINEROOTS_L1_20131201-20131231.csv"
    
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
