#- Make the soil hedley P concentration
make_soil_hedley_p_concentration <- function(func){
    # return ring-specific, time series data of soil P content 
    # need to read in multiple P data sources
    # and soil bulk density data

    # download the data
    
    
    ## read in data 
    ## unit in mg P kg-1 soil
    myDF <- read.csv("temp_files/Hedley_P_2013_Summary_12-2-19.csv")
    
    ## replace R in Ring
    myDF$Ring <- gsub("R", "", myDF$Ring)
    myDF$Ring <- as.numeric(myDF$Ring)

    ## only 2013 data is quality checked
    ## for now, to create the code structure, replace 2015 with 2013 data
    myDF2 <- subset(myDF, Year == 2013)
    #myDF2 <- rbind(myDF, myDF)
    #myDF2$Year[25:48] <- 2015
    
    myDF2$F1_2_Po_Exhanagable <- as.numeric(as.character(myDF2$F1_2_Po_Exhanagable))

    
    # average plots within ring
    myDF3 <- summaryBy(F1_2_Pi_Exhanagable+F1_2_Po_Exhanagable+F3_Po_Moderately_labile+
                           F3_Fe_bound_P_Secondary_mineral+F4_Ca_bound_Primary_Mineral+
                           F5_6_Occluded+Total_Aqua_Regia_P~Year+Ring,
                       data=myDF2,FUN=func,keep.names=T,na.rm=T)
    
    # convert the unit from mg P kg-1 soil to concentration
    myDF3$F1_2_Pi_Exhanagable <- myDF3$F1_2_Pi_Exhanagable * 10^-4
    myDF3$F1_2_Po_Exhanagable <- myDF3$F1_2_Po_Exhanagable * 10^-4
    myDF3$F3_Po_Moderately_labile <- myDF3$F3_Po_Moderately_labile * 10^-4
    myDF3$F3_Fe_bound_P_Secondary_mineral <- myDF3$F3_Fe_bound_P_Secondary_mineral * 10^-4
    myDF3$F4_Ca_bound_Primary_Mineral <- myDF3$F4_Ca_bound_Primary_Mineral * 10^-4
    myDF3$F5_6_Occluded <- myDF3$F5_6_Occluded * 10^-4
    myDF3$Total_Aqua_Regia_P <- myDF3$Total_Aqua_Regia_P * 10^-4
    
    colnames(myDF3) <- c("Year", "Ring", "F1_2_Pi_Exchangeable", "F1_2_Po_Exchangeable", 
                         "F3_Po_Moderately_labile", "F3_Fe_bound_P_Secondary_mineral",
                         "F4_Ca_bound_Primary_Mineral", "F5_6_Occluded", "Total_Aqua_Regia_P")
    
    
    ### test
    #myDF3$total <- with(myDF3, F1_2_Pi_Exchangeable+F1_2_Po_Exchangeable+F3_Po_Moderately_labile+F3_Fe_bound_P_Secondary_mineral+F4_Ca_bound_Primary_Mineral+F5_6_Occluded)
    
    
    return(myDF3)
    
}
