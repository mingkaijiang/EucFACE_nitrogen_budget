make_leaflitter_p_concentration_new <- function(func) {
    df2 <- read.csv("temp_files/Litter_Data_Mingkai.csv")

    #### setting up the date
    df2$Date <- gsub("-Feb", "-02", df2$Campaign)
    df2$Date <- paste0(as.character(df2$Date), "-01")
    df2$Date <- as.Date(df2$Date, "%Y-%m-%d")
    
    ### check data
    #p1 <- ggplot(df2,
    #             aes(Date, Perc.P)) + 
    #    geom_point(aes(color=Origin, shape=CO2Treat, size=2)) +
    #    xlab("Date") + ylab("Canopy P conc (%)")+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=10), 
    #          axis.text.x = element_text(size=10),
    #          axis.text.y=element_text(size=10),
    #          axis.title.y=element_text(size=10),
    #          legend.text=element_text(size=10),
    #          legend.title=element_text(size=12),
    #          panel.grid.major=element_blank(),
    #          legend.position="bottom") 
    #plot(p1)

    #### Leaf litter p, average across rings and date, unit = %
    df.litter.p <- summaryBy(Perc.P~Ring+Date,
                             data=df2,FUN=func,keep.names=T,na.rm=T)
    colnames(df.litter.p) <- c("Ring", "Date", "PercP")
    df.litter.p$month <- month(df.litter.p$Date)
    df.litter.p$year <- year(df.litter.p$Date)

    return(df.litter.p)
}