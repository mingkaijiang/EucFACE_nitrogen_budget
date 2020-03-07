
compare_canopy_p_conc_datasets <- function(inDF1, inDF2) {
    ### compares the two canopy dataset
    
    pDF1 <- summaryBy(PercP~Date, FUN=mean, data=inDF1, keep.names=T)
    pDF2 <- summaryBy(PercP~Date, FUN=mean, data=inDF2, keep.names=T, na.rm=T)
    #pDF3 <- summaryBy(PercP~Date, FUN=mean, data=inDF2[inDF2$Age=="new",], keep.names=T, na.rm=T)
        
        
    p1 <- ggplot() + 
        geom_point(data=pDF1, aes(Date, PercP), color="black") +
        geom_point(data=pDF2, aes(Date, PercP), color="red") +
        #geom_point(data=pDF3, aes(Date, PercP), color="blue") +
        
        geom_smooth(data=pDF1, aes(Date, PercP), color="black", method="gam") +
        geom_smooth(data=pDF2, aes(Date, PercP), color="red", method="gam") +
        #geom_smooth(data=pDF3, aes(Date, PercP), color="blue", method="gam") +
        
        xlab("Date") + ylab("Canopy P conc (%)")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom") 
    
    pdf("plots_tables/Leaf_P_conc_over_time.pdf")
    plot(p1)
    dev.off()
    
    
    
}