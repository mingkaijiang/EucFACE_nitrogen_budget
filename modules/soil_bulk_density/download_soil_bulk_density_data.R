download_soil_bulk_density_data <- function() {
    
    infile <- "FACE_P0088_RA_BULKDENSITY_L1_20170914.csv"
    # Bulk density data
    if(!file.exists(paste0("download/", infile))) {
        downloadHIEv(hiev=searchHIEv(infile))
        
    }

}
