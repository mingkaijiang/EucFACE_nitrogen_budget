make_n_concentration_summary_plots <- function(inDF) {
    
    
    ### Plot 1
    plotDF1 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Canopy N Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Canopy N Conc"],
                            inDF$aCO2[inDF$conc.terms=="Leaflitter N Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Leaflitter N Conc"]), 
                          NA, NA)
    colnames(plotDF1) <- c("mean", "sd", "Variable")
    plotDF1$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Canopy N Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Canopy N Conc"],
                    inDF$aCO2_sd[inDF$conc.terms=="Leaflitter N Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Leaflitter N Conc"])
    plotDF1$Variable <- rep(c("Canopy", "Canopy leaflitter"), each=2)
    plotDF1$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF1$pos <- with(plotDF1, mean + sd)
    plotDF1$neg <- with(plotDF1, mean - sd)
    
    
    ### Plot 3
    plotDF3 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Fine Root N Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Fine Root N Conc"],
                            inDF$aCO2[inDF$conc.terms=="Sapwood N Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Sapwood N Conc"]), 
                          NA, NA)
    colnames(plotDF3) <- c("mean", "sd", "Variable")
    plotDF3$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Fine Root N Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Fine Root N Conc"],
                    inDF$aCO2_sd[inDF$conc.terms=="Sapwood N Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Sapwood N Conc"])
    plotDF3$Variable <- rep(c("Fine Root", "Sapwood"), each=2)
    plotDF3$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF3$pos <- with(plotDF3, mean + sd)
    plotDF3$neg <- with(plotDF3, mean - sd)
    
    ### Plot 4
    plotDF4 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Understorey N Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Understorey N Conc"],
                            inDF$aCO2[inDF$conc.terms=="Frass N Conc"], 
                            inDF$eCO2[inDF$conc.terms=="Frass N Conc"]), 
                          NA, NA)
    colnames(plotDF4) <- c("mean", "sd", "Variable")
    plotDF4$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Frass N Conc"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Frass N Conc"])
    plotDF4$Variable <- rep(c("Understorey leaf", "Frass"), each=2)
    plotDF4$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF4$pos <- with(plotDF4, mean + sd)
    plotDF4$neg <- with(plotDF4, mean - sd)
    
    ### Plot 5
    plotDF5 <- data.frame(c(inDF$aCO2[inDF$conc.terms=="Soil N Conc 0-10cm"], 
                            inDF$eCO2[inDF$conc.terms=="Soil N Conc 0-10cm"],
                            inDF$aCO2[inDF$conc.terms=="Microbial N Conc 0-10cm"], 
                            inDF$eCO2[inDF$conc.terms=="Microbial N Conc 0-10cm"]), 
                          NA, NA)
    colnames(plotDF5) <- c("mean", "sd", "Variable")
    plotDF5$sd <- c(inDF$aCO2_sd[inDF$conc.terms=="Soil N Conc 0-10cm"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Soil N Conc 0-10cm"],
                    inDF$aCO2_sd[inDF$conc.terms=="Microbial N Conc 0-10cm"], 
                    inDF$eCO2_sd[inDF$conc.terms=="Microbial N Conc 0-10cm"])
    plotDF5$Variable <- rep(c("Soil 0-10cm", "Microbe"), each=2)
    plotDF5$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF5$pos <- with(plotDF5, mean + sd)
    plotDF5$neg <- with(plotDF5, mean - sd)
    
    
    
    ### Plotting
    p1 <- ggplot(plotDF1, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y="N concentration (%)")+
        theme_linedraw() +
        ylim(0,2)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    
    p3 <- ggplot(plotDF3, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y="N concentration (%)")+
        theme_linedraw() +
        ylim(0,1.5)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    p4 <- ggplot(plotDF4, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y="N concentration (%)")+
        theme_linedraw() +
        ylim(0,2)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    p5 <- ggplot(plotDF5, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y="N concentration (%)")+
        theme_linedraw() +
        ylim(0,0.2)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    require(grid)
    require(cowplot)
    
    ## plot 
    pdf("output/n_budget/N_Concentration_Summary_Plots.pdf", width=8,height=8)
    plot_grid(p1, p4, p5, p3, labels="", ncol=2, align="v", axis = "l",
              rel_heights = c(1, 1.2))
    grid.text(grid.labs, x = c(0.1, 0.6, 0.1, 0.6),
              y = c(0.95, 0.95, 0.5, 0.5), 
              gp=gpar(fontsize=14, col="black", fontface="bold"))
    dev.off()
    
}


