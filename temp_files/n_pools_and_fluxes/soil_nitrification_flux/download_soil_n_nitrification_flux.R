download_soil_n_nitrification_flux <- function() {
    
    infile1 <- "FACE_RA_P0023_SOILMINERALISATION_L3_20120724-20140124.csv"
    infile2 <- "FACE_RA_P0023_SOILMINERALISATION_L1_20140428-20160121.csv"

    if(!file.exists(paste0("download/", infile1))) {
        downloadHIEv(hiev=searchHIEv("FACE_RA_P0023_SOILMINERALISATION_L3_20120724-20140124"))
        
    }
    
    if(!file.exists(paste0("download/", infile2))) {
        downloadHIEv(hiev=searchHIEv("FACE_RA_P0023_SOILMINERALISATION_L1_20140428-20160121"))
        
    }

}