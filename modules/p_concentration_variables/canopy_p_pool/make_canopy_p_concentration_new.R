#- Make the canopy P concentration
make_canopy_p_concentration_new <- function(func) {
    ### return ring-specific canopy P data (mg/kg)
    ### this script is written for new data that Kristine processed

    ### read in
    df <- read.csv("temp_files/GreenLeaves_allP-clean_Mingkai.csv")
    
    ### setting up the date
    df$Date <- paste0("01-", as.character(df$Campaign))
    #df$DATE <- gsub("/", "-", df$DATE)
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    
    ### check data
    #p1 <- ggplot(df,
    #             aes(Date, Perc.P)) + 
    #    geom_point(aes(color=AGE, shape=CO2TREAT, size=2)) +
    #    geom_smooth(method="gam")+
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
    
    ### only include new leaf, as this is the total required
    df2 <- subset(df, AGE == "old")
    
    ### check data
    #p2 <- ggplot(df2,
    #             aes(Date, Perc.P)) + 
    #    geom_point(aes(shape=CO2TREAT, size=2)) +
    #    geom_smooth(method="gam")+
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
    #plot(p2)

    ### obtain ring means
    out <- summaryBy(Perc.P~Ring+Date,
                      data=df2,FUN=func,keep.names=T,na.rm=T)
    out$month <- month(out$Date)
    out$year <- year(out$Date)
    
    colnames(out) <- c("Ring", "Date", "PercP", "month", "year")
    out <- out[complete.cases(out$Date),]
    
    return(out)

}


