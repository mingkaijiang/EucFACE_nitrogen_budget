download_soil_p_data <- function() {
    
    # Samples dates are June 2012 (0-10cm and 10-20cm), Sept 2012, Dec 2012, March 2013 (0-10cm) June 2013 (20-30cm) Sept 2013, Dec 2013, March 2014 (0-10cm) 
    downloadHIEv(hiev=searchHIEv("FACE_P0014_ALL_ ElementalAnalysis_2012to2014_V2.csv"))
    
    # March, September and December samples were collected to 10 cm. June samples were collected to 30 cm and devidied into 0-10, 10-20 and 20-30cm. 
    downloadHIEv(hiev=searchHIEv("FACE_P0014_ALL_BasicSoilProperties_L1"))
    
    # Data for 0-2cm and 0-10cm for March, June, September and November sampling points. 
    downloadHIEv(hiev=searchHIEv("FACE_P0014_ALL_extractableNP_L1"))
}
