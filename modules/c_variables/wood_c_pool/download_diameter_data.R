
# this function downloads the diameter files from HIEv
download_diameter_data <- function() {
    
    infile1 <- "FACE_P0025_RA_TREEMEAS_2012-13_RAW-V1.csv"
    infile2 <- "FACE_P0025_RA_TREEMEAS_2013-14_RAW_V1.csv"
    infile3 <- "FACE_P0025_RA_TREEMEAS_2015_RAW_V1.csv"
    infile4 <- "FACE_P0045_RA_MORTALITY_RAW_20150501_v1.csv"
    infile5 <- "FACE_P0079_RA_SAPWOOD_N_RAW_2015-11_v1.csv"
    infile6 <- "FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv"
    infile7 <- "All_dendros_diameter_and_biomass_data.csv"
    infile8 <- "FACE_P0025_RA_TREEMEAS_2016_RAW_V1.csv"
    infile9 <- "FACE_P0018_R0_SAPWOOD_20130418.csv"
    
    downloadHIEv(hiev=searchHIEv(infile1))
    downloadHIEv(hiev=searchHIEv(infile2))
    downloadHIEv(hiev=searchHIEv(infile3))
    downloadHIEv(hiev=searchHIEv(infile4))
    downloadHIEv(hiev=searchHIEv(infile5))
    downloadHIEv(hiev=searchHIEv(infile6))
    downloadHIEv(hiev=searchHIEv(infile7))
    downloadHIEv(hiev=searchHIEv(infile8))
    downloadHIEv(hiev=searchHIEv(infile9))
  
 
  }