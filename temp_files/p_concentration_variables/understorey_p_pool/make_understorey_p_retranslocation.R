#- Make the understorey P retranslocation coefficient
make_understorey_p_retranslocation <- function(){

    ### read in the most recent harvest data
    df <- read.csv("temp_files/understorey_P_concentration_data_2017_06.csv")
    df <- subset(df, Component == "Understorey")
    
    ### subset live and calculate mean conc
    df1 <- subset(df, Subclass == "Live")
    df2 <- summaryBy(P_conc_mg_g~Ring, FUN=mean, data=df1, keep.names=T, na.rm=T)
    
    ### subset dead
    df3 <- subset(df, Subclass == "Dead")
    
    ### Calculate retranslocation coefficient
    for (i in 1:6) {
        df2$dead[df2$Ring==i] <- df3$P_conc_mg_g[df3$Ring==i]
    }
    
    df2$retrans_coef <- with(df2, (P_conc_mg_g - dead)/P_conc_mg_g)

    outDF <- df2[,c("Ring", "retrans_coef")]

    return(outDF)
    
}
