#- Make the understorey Litter P concentration
make_understorey_litter_p_concentration <- function(func){

    ### read in the most recent harvest data
    df2 <- read.csv("temp_files/understorey_P_concentration_data_2017_06.csv")
    df2 <- subset(df2, Component == "Understorey")
    df2 <- subset(df2, Subclass == "Dead")
    df2$PercP <- df2$P_conc_mg_g / 10
    df3 <- df2[,c("Date", "Ring", "PercP")]
    df3$Date <- as.Date(df3$Date, format="%d/%m/%y")
    
    
    ### ring and date specific data
    outDF <- summaryBy(PercP~Ring+Date,
                             data=df3,FUN=func,keep.names=T,na.rm=T)

    return(outDF)
    
}
