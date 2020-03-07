download_soil_phosphate_data <- function() {
    # Data from Cat
    # Data for 0-2cm and 0-10cm for March, June, September and November sampling points. 
    # covers period 20140314 - 20141117 & 20150310 - 20151130
    downloadHIEv(hiev=searchHIEv("FACE_P0014_ALL_extractableNP_L1"))
    
    # Data from Shun
    # covers period 20120620 - 20140118
    downloadHIEv(hiev=searchHIEv("FACE_RA_P0010_SOILIEMNUTRIENTS_L3_20120620-20140118"))
    
    # Data from Shun
    # covers period 20120613 - 20140310
    downloadHIEv(hiev=searchHIEv("FACE_RA_P0023_SOILEXTRACTABLENUTRIENTS_L3_20120613-20140310"))
    
    
}