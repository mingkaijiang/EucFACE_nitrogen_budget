make_n_fluxes_summary_plots <- function(inDF) {
    
    
    ### Plot 1
    plotDF1 <- data.frame(c(inDF$aCO2[inDF$terms=="Canopy N flux"], 
                            inDF$eCO2[inDF$terms=="Canopy N flux"],
                            inDF$aCO2[inDF$terms=="Leaflitter N flux"], 
                            inDF$eCO2[inDF$terms=="Leaflitter N flux"],
                            inDF$aCO2[inDF$terms=="Fine Root N flux"], 
                            inDF$eCO2[inDF$terms=="Fine Root N flux"],
                            inDF$aCO2[inDF$terms=="Fineroot Litter N flux"], 
                            inDF$eCO2[inDF$terms=="Fineroot Litter N flux"],
                            inDF$aCO2[inDF$terms=="Understorey N flux"], 
                            inDF$eCO2[inDF$terms=="Understorey N flux"],
                            inDF$aCO2[inDF$terms=="Understorey Litter N flux"], 
                            inDF$eCO2[inDF$terms=="Understorey Litter N flux"],
                            inDF$aCO2[inDF$terms=="Frass N flux"], 
                            inDF$eCO2[inDF$terms=="Frass N flux"]), 
                          NA, NA)
    colnames(plotDF1) <- c("mean", "sd", "Variable")
    plotDF1$sd <- c(inDF$aCO2_sd[inDF$terms=="Canopy N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Canopy N flux"],
                    inDF$aCO2_sd[inDF$terms=="Leaflitter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Leaflitter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Fine Root N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Fine Root N flux"],
                    inDF$aCO2_sd[inDF$terms=="Fineroot Litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Fineroot Litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Understorey N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Understorey N flux"],
                    inDF$aCO2_sd[inDF$terms=="Understorey Litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Understorey Litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Frass N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Frass N flux"])
    plotDF1$Variable <- rep(c("Canopy", "Canopy litter", "Fineroot", "Fineroot Litter", 
                              "Understorey", "Understorey litter", "Frass"), each=2)
    plotDF1$Trt <- rep(c("aCO2", "eCO2"), 7)
    plotDF1$pos <- with(plotDF1, mean + sd)
    plotDF1$neg <- with(plotDF1, mean - sd)
    
    ### Plot 2
    plotDF2 <- data.frame(c(inDF$aCO2[inDF$terms=="Wood N flux"], 
                            inDF$eCO2[inDF$terms=="Wood N flux"],
                            inDF$aCO2[inDF$terms=="Twig litter N flux"], 
                            inDF$eCO2[inDF$terms=="Twig litter N flux"],
                            inDF$aCO2[inDF$terms=="Bark litter N flux"], 
                            inDF$eCO2[inDF$terms=="Bark litter N flux"],
                            inDF$aCO2[inDF$terms=="Seed litter N flux"], 
                            inDF$eCO2[inDF$terms=="Seed litter N flux"],
                            inDF$aCO2[inDF$terms=="Coarse Root N flux"], 
                            inDF$eCO2[inDF$terms=="Coarse Root N flux"]), 
                          NA, NA)
    colnames(plotDF2) <- c("mean", "sd", "Variable")
    plotDF2$sd <- c(inDF$aCO2_sd[inDF$terms=="Wood N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Wood N flux"],
                    inDF$aCO2_sd[inDF$terms=="Twig litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Twig litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Bark litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Bark litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Seed litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Seed litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Coarse Root N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Coarse Root N flux"])
    plotDF2$Variable <- rep(c("Wood", "Twig", "Bark", "Seed", "Coarseroot"), each=2)
    plotDF2$Trt <- rep(c("aCO2", "eCO2"), 5)
    plotDF2$pos <- with(plotDF2, mean + sd)
    plotDF2$neg <- with(plotDF2, mean - sd)
    
    ### Plot 3
    plotDF3 <- data.frame(c(inDF$aCO2[inDF$terms=="Mineralization N flux"], 
                            inDF$eCO2[inDF$terms=="Mineralization N flux"]), 
                          NA, NA)
    colnames(plotDF3) <- c("mean", "sd", "Variable")
    plotDF3$sd <- c(inDF$aCO2_sd[inDF$terms=="Mineralization N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Mineralization N flux"])
    plotDF3$Variable <- rep(c("N mineralization rate"), each=2)
    plotDF3$Trt <- rep(c("aCO2", "eCO2"), 1)
    plotDF3$pos <- with(plotDF3, mean + sd)
    plotDF3$neg <- with(plotDF3, mean - sd)
    
    
    
    ### Plotting
    p1 <- ggplot(plotDF1, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste("N flux (g N ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="top")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))
    
    
    p2 <- ggplot(plotDF2, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste("N flux (g N ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
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
        labs(x="", y=expression(paste("N flux (g N ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
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
    
    
    
    grid.labs <- c("(a)", "(b)", "(c)")
    
    require(grid)
    require(cowplot)
    
    require(gridExtra)
    
    ## plot 
    pdf("plots_tables/N_Fluxes_Summary_Plots.pdf", width=8,height=8)
    bot_row <- plot_grid(p2, p3, ncol=2)
    plot_grid(p1, bot_row,  ncol = 1, rel_widths = c(1, 1, 0.2),
              rel_heights=c(1.2, 1, 1))
    grid.text(grid.labs,x = c(0.12, 0.14, 0.6), y = c(0.9, 0.42, 0.42),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    
}
