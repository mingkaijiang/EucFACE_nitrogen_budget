make_n_budget_summary_plots <- function(inDF) {
    
    ################### Plot all N summary budget plots
    ### Plot 1df
    plotDF1 <- data.frame(c(inDF$aCO2[inDF$terms=="Total plant N stock"], 
                            inDF$eCO2[inDF$terms=="Total plant N stock"]), 
                          NA)
    colnames(plotDF1) <- c("mean", "sd")
    plotDF1$sd <- c(inDF$aCO2_sd[inDF$terms=="Total plant N stock"], 
                    inDF$eCO2_sd[inDF$terms=="Total plant N stock"])
    plotDF1$Trt <- c("aCO2", "eCO2")
    plotDF1$pos <- with(plotDF1, mean + sd)
    plotDF1$neg <- with(plotDF1, mean - sd)
    
    ### Plot 2
    plotDF2 <- data.frame(c(inDF$aCO2[inDF$terms=="Overstorey aboveground N stock"], 
                            inDF$eCO2[inDF$terms=="Overstorey aboveground N stock"],
                            inDF$aCO2[inDF$terms=="Understorey aboveground N stock"], 
                            inDF$eCO2[inDF$terms=="Understorey aboveground N stock"],
                            inDF$aCO2[inDF$terms=="Belowground N stock"], 
                            inDF$eCO2[inDF$terms=="Belowground N stock"]), 
                          NA)
    colnames(plotDF2) <- c("mean", "sd")
    plotDF2$sd <- c(inDF$aCO2_sd[inDF$terms=="Overstorey aboveground N stock"], 
                    inDF$eCO2_sd[inDF$terms=="Overstorey aboveground N stock"],
                    inDF$aCO2_sd[inDF$terms=="Understorey aboveground N stock"], 
                    inDF$eCO2_sd[inDF$terms=="Understorey aboveground N stock"],
                    inDF$aCO2_sd[inDF$terms=="Belowground N stock"], 
                    inDF$eCO2_sd[inDF$terms=="Belowground N stock"])
    plotDF2$Trt <- rep(c("aCO2", "eCO2"), 3)
    plotDF2$Variable <- rep(c("OA", "UA", "B"), each=2)
    plotDF2$pos <- with(plotDF2, mean + sd)
    plotDF2$neg <- with(plotDF2, mean - sd)
    aC <- sum(plotDF2$mean[plotDF2$Trt=="aCO2"])
    eC <- sum(plotDF2$mean[plotDF2$Trt=="eCO2"])
    
    plotDF2$prop[plotDF2$Trt=="aCO2"] <- plotDF2$mean[plotDF2$Trt=="aCO2"] / aC * 100
    plotDF2$prop[plotDF2$Trt=="eCO2"] <- plotDF2$mean[plotDF2$Trt=="eCO2"] / eC * 100
    
    
    ### Plot 3
    plotDF3 <- data.frame(c(inDF$aCO2[inDF$terms=="Total plant N requirement flux"], 
                            inDF$eCO2[inDF$terms=="Total plant N requirement flux"]), 
                          NA)
    colnames(plotDF3) <- c("mean", "sd")
    plotDF3$sd <- c(inDF$aCO2_sd[inDF$terms=="Total plant N requirement flux"], 
                    inDF$eCO2_sd[inDF$terms=="Total plant N requirement flux"])
    plotDF3$Trt <- c("aCO2", "eCO2")
    plotDF3$pos <- with(plotDF3, mean + sd)
    plotDF3$neg <- with(plotDF3, mean - sd)
    
    ### Plot 4
    plotDF4 <- data.frame(c(inDF$aCO2[inDF$terms=="Total plant N retranslocation flux"], 
                            inDF$eCO2[inDF$terms=="Total plant N retranslocation flux"]), 
                          NA)
    colnames(plotDF4) <- c("mean", "sd")
    plotDF4$sd <- c(inDF$aCO2_sd[inDF$terms=="Total plant N retranslocation flux"], 
                    inDF$eCO2_sd[inDF$terms=="Total plant N retranslocation flux"])
    plotDF4$Trt <- c("aCO2", "eCO2")
    plotDF4$pos <- with(plotDF4, mean + sd)
    plotDF4$neg <- with(plotDF4, mean - sd)
    
    ### Plot 5
    plotDF5 <- data.frame(c(inDF$aCO2[inDF$terms=="Plant N uptake flux"], 
                            inDF$eCO2[inDF$terms=="Plant N uptake flux"]), 
                          NA)
    colnames(plotDF5) <- c("mean", "sd")
    plotDF5$sd <- c(inDF$aCO2_sd[inDF$terms=="Plant N uptake flux"], 
                    inDF$eCO2_sd[inDF$terms=="Plant N uptake flux"])
    plotDF5$Trt <- c("aCO2", "eCO2")
    plotDF5$pos <- with(plotDF5, mean + sd)
    plotDF5$neg <- with(plotDF5, mean - sd)
    
    ### Plot 6
    plotDF6 <- data.frame(c(inDF$aCO2[inDF$terms=="Soil N mineralization flux"], 
                            inDF$eCO2[inDF$terms=="Soil N mineralization flux"]), 
                          NA)
    colnames(plotDF6) <- c("mean", "sd")
    plotDF6$sd <- c(inDF$aCO2_sd[inDF$terms=="Soil N mineralization flux"], 
                    inDF$eCO2_sd[inDF$terms=="Soil N mineralization flux"])
    plotDF6$Trt <- c("aCO2", "eCO2")
    plotDF6$pos <- with(plotDF6, mean + sd)
    plotDF6$neg <- with(plotDF6, mean - sd)
    
    
    ### Plot efficiencies
    plotDF7 <- data.frame(c(inDF$aCO2[inDF$terms=="Plant N MRT"], 
                            inDF$eCO2[inDF$terms=="Plant N MRT"]), 
                          NA)
    colnames(plotDF7) <- c("mean", "sd")
    plotDF7$sd <- c(inDF$aCO2_sd[inDF$terms=="Plant N MRT"], 
                    inDF$eCO2_sd[inDF$terms=="Plant N MRT"])
    plotDF7$Trt <- c("aCO2", "eCO2")
    plotDF7$pos <- with(plotDF7, mean + sd)
    plotDF7$neg <- with(plotDF7, mean - sd)
    
    
    plotDF8 <- data.frame(c(inDF$aCO2[inDF$terms=="Plant NUE"], 
                            inDF$eCO2[inDF$terms=="Plant NUE"]), 
                          NA)
    colnames(plotDF8) <- c("mean", "sd")
    plotDF8$sd <- c(inDF$aCO2_sd[inDF$terms=="Plant NUE"], 
                    inDF$eCO2_sd[inDF$terms=="Plant NUE"])
    plotDF8$Trt <- c("aCO2", "eCO2")
    plotDF8$pos <- with(plotDF8, mean + sd)
    plotDF8$neg <- with(plotDF8, mean - sd)
    
    ### Plotting
    
    p1 <- ggplot(plotDF1,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("Standing N stock (g N ", m^-2, ")")))+
        theme_linedraw() +
        ylim(0, 120.0)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="bottom")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    p2 <- ggplot(plotDF2, aes(x="", y=prop, fill=Variable))+
        geom_bar(width = 1, stat = "identity") +
        coord_polar(theta="y") +
        facet_grid(facets=. ~ Trt) +
        theme_minimal()+
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              panel.border = element_blank(),
              panel.grid=element_blank(),
              axis.ticks = element_blank(),
              plot.title=element_text(size=14, face="bold"),
              legend.position="bottom")+
        scale_fill_manual(name="Component", values = c("UA" = "green", "OA" = "darkgreen", "B" = "orange"),
                          labels=c("B","OA", "UA"))
    
    p3 <- ggplot(plotDF3,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("Plant N requirement flux (g N ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 20)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    p4 <- ggplot(plotDF4,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("Plant N retranslocation (g N ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 20)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    p5 <- ggplot(plotDF5,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("Plant N uptake (g N ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 20)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    
    p6 <- ggplot(plotDF6,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("Soil N mineralization (g N ", m^-2, " ", yr^-1, ")")))+
        theme_linedraw() +
        ylim(0, 20)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    
    p7 <- ggplot(plotDF7,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab("Plant N MRT (yr)")+
        theme_linedraw() +
        ylim(0,20)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    
    p8 <- ggplot(plotDF8,
                 aes(Trt, mean)) + 
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        xlab("") + ylab(expression(paste("Plant NUE ( gC" * " " *gP^-1 * " )")))+
        theme_linedraw() +
        ylim(0,150)+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=10),
              axis.title.y=element_text(size=10),
              legend.text=element_text(size=10),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_fill_manual(name="", values = c("aCO2" = "blue2", "eCO2" = "red3"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_colour_manual(name="", values = c("aCO2" = "black", "eCO2" = "black"),
                            labels=c(expression(aCO[2]), expression(eCO[2])))+
        scale_x_discrete(limits=c("aCO2","eCO2"),
                         labels=c(expression(aCO[2]),
                                  expression(eCO[2])))
    
    
    
    grid.labs <- c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)", "(g)", "(h)")
    
    require(grid)
    require(cowplot)
    
    ## plot 
    pdf("output/n_budget/N_Budget_Summary_Plots.pdf", width=10,height=14)
    plot_grid(p3, p4, p5, p6, p1, p2, p7, p8, labels="", ncol=2, align="v", axis = "l",
              rel_heights = c(1, 1, 1.2, 1))
    grid.text(grid.labs, x = c(0.1, 0.6, 0.1, 0.6, 0.1, 0.6, 0.1, 0.6),
              y = c(0.97, 0.97, 0.73, 0.73, 0.5, 0.5, 0.21, 0.21), 
              gp=gpar(fontsize=14, col="black", fontface="bold"))
    dev.off()
    
    
}
