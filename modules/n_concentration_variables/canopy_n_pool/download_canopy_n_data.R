download_canopy_n_data <- function(){
    
    ### this file only exists in local now
    infile <- "FACE_P0020_RA_NPleaf_2012-2018-L2.csv"
    
    if(!file.exists(paste0("download/", infile))) {
        downloadHIEv(hiev=searchHIEv(infile))
    }
}
