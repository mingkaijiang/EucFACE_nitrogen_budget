make_understorey_aboveground_growth_estimate <- function(plotting) {
    
    ### download data
    download_understorey_aboveground_data()
    
    ### read in the data 
    inDF <- read.csv(file.path(getToPath(), 
                                "FACE_TLAPSE_15DGROWTH_L2_RA_20140801-20170331.csv"))
    
    ### ring stuffs
    inDF$ring <- gsub("ring1", "1", inDF$ring)
    inDF$ring <- gsub("ring2", "2", inDF$ring)
    inDF$ring <- gsub("ring3", "3", inDF$ring)
    inDF$ring <- gsub("ring4", "4", inDF$ring)
    inDF$ring <- gsub("ring5", "5", inDF$ring)
    inDF$ring <- gsub("ring6", "6", inDF$ring)
    
    ### process class
    inDF$Date <- as.Date(as.character(inDF$Date), format="%d/%m/%Y")
    inDF$Ring <- as.numeric(inDF$ring)
    inDF$End_date <- inDF$Date
    inDF$Start_date <- inDF$End_date - 15
    inDF$ndays <- 15
    inDF$growth_per_day <- inDF$growth / inDF$ndays
    
    ### format dataframe to return
    out <-inDF[,c("Start_date", "End_date", "Date", "Ring", "growth_per_day", "ndays")]
    colnames(out) <- c("Start_date", "End_date", "Date", "Ring", "Daily_growth", "Days")
    
    if (plotting == T) {
        #pdf("plots_tables/understorey_growth_estimated_from_cover.pdf")
        #
        #p <- ggplot(out, aes(Date, Daily_growth, color=factor(Ring))) +   
        #    geom_point(size = 5) + geom_line() + 
        #    xlab("Date") + ylab("Understorey Growth (% cover change / day)") + 
        #    scale_color_manual(values=c("#FF7F50", "#00FFFF", "#6495ED",
        #                                "#FF4040", "#8B0000", "#0000FF"))
        #
        #plot(p)
        #
        #dev.off()
    } else {
        return(out)
    }
    
}
