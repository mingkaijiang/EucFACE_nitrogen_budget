make_n_fluxes_summary_plots <- function(inDF) {
    
    
    ### Plot 1
    plotDF1 <- data.frame(c(inDF$aCO2[inDF$terms=="Canopy N flux"], 
                            inDF$eCO2[inDF$terms=="Canopy N flux"],
                            inDF$aCO2[inDF$terms=="Wood N flux"], 
                            inDF$eCO2[inDF$terms=="Wood N flux"],
                            inDF$aCO2[inDF$terms=="Fine Root N flux"], 
                            inDF$eCO2[inDF$terms=="Fine Root N flux"],
                            inDF$aCO2[inDF$terms=="Understorey N flux"], 
                            inDF$eCO2[inDF$terms=="Understorey N flux"],
                            inDF$aCO2[inDF$terms=="Twig litter N flux"], 
                            inDF$eCO2[inDF$terms=="Twig litter N flux"],
                            inDF$aCO2[inDF$terms=="Bark litter N flux"], 
                            inDF$eCO2[inDF$terms=="Bark litter N flux"],
                            inDF$aCO2[inDF$terms=="Seed litter N flux"], 
                            inDF$eCO2[inDF$terms=="Seed litter N flux"],
                            inDF$aCO2[inDF$terms=="Coarse Root N flux"], 
                            inDF$eCO2[inDF$terms=="Coarse Root N flux"]), 
                          NA, NA)
    colnames(plotDF1) <- c("mean", "sd", "Variable")
    plotDF1$sd <- c(inDF$aCO2_sd[inDF$terms=="Canopy N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Canopy N flux"],
                    inDF$aCO2_sd[inDF$terms=="Wood N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Wood N flux"],
                    inDF$aCO2_sd[inDF$terms=="Fine Root N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Fine Root N flux"],
                    inDF$aCO2_sd[inDF$terms=="Understorey N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Understorey N flux"],
                    inDF$aCO2_sd[inDF$terms=="Twig litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Twig litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Bark litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Bark litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Seed litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Seed litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Coarse Root N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Coarse Root N flux"])
    plotDF1$Variable <- rep(c("Canopy", "Wood", "Fineroot", 
                              "Understorey", "Twig", "Bark", "Seed", "Coarseroot"), each=2)
    plotDF1$Trt <- rep(c("aCO2", "eCO2"), 8)
    plotDF1$pos <- with(plotDF1, mean + sd)
    plotDF1$neg <- with(plotDF1, mean - sd)
    
    ### Plot 2
    plotDF2 <- data.frame(c(inDF$aCO2[inDF$terms=="Leaflitter N flux"], 
                            inDF$eCO2[inDF$terms=="Leaflitter N flux"],
                            inDF$aCO2[inDF$terms=="Fineroot Litter N flux"], 
                            inDF$eCO2[inDF$terms=="Fineroot Litter N flux"],
                            inDF$aCO2[inDF$terms=="Understorey Litter N flux"], 
                            inDF$eCO2[inDF$terms=="Understorey Litter N flux"],
                            inDF$aCO2[inDF$terms=="Twig litter N flux"], 
                            inDF$eCO2[inDF$terms=="Twig litter N flux"],
                            inDF$aCO2[inDF$terms=="Bark litter N flux"], 
                            inDF$eCO2[inDF$terms=="Bark litter N flux"],
                            inDF$aCO2[inDF$terms=="Seed litter N flux"], 
                            inDF$eCO2[inDF$terms=="Seed litter N flux"],
                            inDF$aCO2[inDF$terms=="Frass N flux"], 
                            inDF$eCO2[inDF$terms=="Frass N flux"]), 
                          NA, NA)
    colnames(plotDF2) <- c("mean", "sd", "Variable")
    plotDF2$sd <- c(inDF$aCO2_sd[inDF$terms=="Leaflitter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Leaflitter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Fineroot Litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Fineroot Litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Understorey Litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Understorey Litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Twig litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Twig litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Bark litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Bark litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Seed litter N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Seed litter N flux"],
                    inDF$aCO2_sd[inDF$terms=="Frass N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Frass N flux"])
    plotDF2$Variable <- rep(c("Leaf", "Fineroot", "Understorey",
                              "Twig", "Bark", "Seed", "Frass"), each=2)
    plotDF2$Trt <- rep(c("aCO2", "eCO2"), 7)
    plotDF2$pos <- with(plotDF2, mean + sd)
    plotDF2$neg <- with(plotDF2, mean - sd)
    
    ### Plot 3
    plotDF3 <- data.frame(c(inDF$aCO2[inDF$terms=="Mineralization N flux"], 
                            inDF$eCO2[inDF$terms=="Mineralization N flux"],
                            inDF$aCO2[inDF$terms=="Nitrification N flux"], 
                            inDF$eCO2[inDF$terms=="Nitrification N flux"]), 
                          NA, NA)
    colnames(plotDF3) <- c("mean", "sd", "Variable")
    plotDF3$sd <- c(inDF$aCO2_sd[inDF$terms=="Mineralization N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Mineralization N flux"],
                    inDF$aCO2_sd[inDF$terms=="Nitrification N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Nitrification N flux"])
    plotDF3$Variable <- rep(c("N mineralization", "N nitrification"), each=2)
    plotDF3$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF3$pos <- with(plotDF3, mean + sd)
    plotDF3$neg <- with(plotDF3, mean - sd)
    
    
    ### Plot 4
    plotDF4 <- data.frame(c(inDF$aCO2[inDF$terms=="Canopy retrans N flux"], 
                            inDF$eCO2[inDF$terms=="Canopy retrans N flux"],
                            inDF$aCO2[inDF$terms=="Sapwood retrans N flux"], 
                            inDF$eCO2[inDF$terms=="Sapwood retrans N flux"],
                            inDF$aCO2[inDF$terms=="Fineroot retrans N flux"], 
                            inDF$eCO2[inDF$terms=="Fineroot retrans N flux"],
                            inDF$aCO2[inDF$terms=="Understorey retrans N flux"], 
                            inDF$eCO2[inDF$terms=="Understorey retrans N flux"]), 
                          NA, NA)
    colnames(plotDF4) <- c("mean", "sd", "Variable")
    plotDF4$sd <- c(inDF$aCO2_sd[inDF$terms=="Canopy retrans N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Canopy retrans N flux"],
                    inDF$aCO2_sd[inDF$terms=="Sapwood retrans N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Sapwood retrans N flux"],
                    inDF$aCO2_sd[inDF$terms=="Fineroot retrans N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Fineroot retrans N flux"],
                    inDF$aCO2_sd[inDF$terms=="Understorey retrans N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Understorey retrans N flux"])
    plotDF4$Variable <- rep(c("Canopy", "Sapwood", "Fineroot", "Understorey"), each=2)
    plotDF4$Trt <- rep(c("aCO2", "eCO2"), 4)
    plotDF4$pos <- with(plotDF4, mean + sd)
    plotDF4$neg <- with(plotDF4, mean - sd)
    
    
    ### Plot 5
    plotDF5 <- data.frame(c(inDF$aCO2[inDF$terms=="Atmospheric deposition N flux"], 
                            inDF$eCO2[inDF$terms=="Atmospheric deposition N flux"],
                            inDF$aCO2[inDF$terms=="Leaching N flux"], 
                            inDF$eCO2[inDF$terms=="Leaching N flux"]), 
                          NA, NA)
    colnames(plotDF5) <- c("mean", "sd", "Variable")
    plotDF5$sd <- c(inDF$aCO2_sd[inDF$terms=="Atmospheric deposition N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Atmospheric deposition N flux"],
                    inDF$aCO2_sd[inDF$terms=="Leaching N flux"], 
                    inDF$eCO2_sd[inDF$terms=="Leaching N flux"])
    plotDF5$Variable <- rep(c("N deposition", "N leaching"), each=2)
    plotDF5$Trt <- rep(c("aCO2", "eCO2"), 2)
    plotDF5$pos <- with(plotDF5, mean + sd)
    plotDF5$neg <- with(plotDF5, mean - sd)
    
    
    
    ### Plotting
    p1 <- ggplot(plotDF1, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste("Production (g N ", m^-2, " ", yr^-1, ")")))+
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
        labs(x="", y=expression(paste("Litter (g N ", m^-2, " ", yr^-1, ")")))+
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
        labs(x="", y=expression(paste("Soil (g N ", m^-2, " ", yr^-1, ")")))+
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
    
    
    p4 <- ggplot(plotDF4, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste("Retranslocation (g N ", m^-2, " ", yr^-1, ")")))+
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
    
    
    
    p5 <- ggplot(plotDF5, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste("Input output (g N ", m^-2, " ", yr^-1, ")")))+
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
    
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)")
    
    require(grid)
    require(cowplot)
    
    require(gridExtra)
    
    ## plot 
    pdf("output/n_budget/N_Fluxes_Summary_Plots.pdf", width=8,height=8)
    mid_row <- plot_grid(p2, p3, ncol=2, rel_widths = c(1.2, 0.8))
    bot_row <- plot_grid(p4, p5, ncol=2)
    plot_grid(p1, mid_row, bot_row,  ncol = 1, rel_widths = c(1, 1, 0.2),
              rel_heights=c(1.2, 1, 1))
    grid.text(grid.labs,x = c(0.12, 0.1, 0.75, 0.1, 0.95), y = c(0.9, 0.59, 0.59, 0.27, 0.27),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    
}
