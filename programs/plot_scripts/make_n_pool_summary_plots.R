make_n_pools_summary_plots <- function(inDF) {
    
    ### Plot 1
    plotDF1 <- data.frame(c(inDF$aCO2[inDF$terms=="Canopy N Pool"], 
                            inDF$eCO2[inDF$terms=="Canopy N Pool"],
                            inDF$aCO2[inDF$terms=="Fine Root N Pool"], 
                            inDF$eCO2[inDF$terms=="Fine Root N Pool"],
                            inDF$aCO2[inDF$terms=="Coarse Root N Pool"], 
                            inDF$eCO2[inDF$terms=="Coarse Root N Pool"],
                            inDF$aCO2[inDF$terms=="Understorey N Pool"], 
                            inDF$eCO2[inDF$terms=="Understorey N Pool"]), 
                          NA, NA)
    colnames(plotDF1) <- c("mean", "sd", "Variable")
    plotDF1$sd <- c(inDF$aCO2_sd[inDF$terms=="Canopy N Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Canopy N Pool"],
                    inDF$aCO2_sd[inDF$terms=="Fine Root N Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Fine Root N Pool"],
                    inDF$aCO2_sd[inDF$terms=="Coarse Root N Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Coarse Root N Pool"],
                    inDF$aCO2_sd[inDF$terms=="Understorey N Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Understorey N Pool"])
    plotDF1$Variable <- rep(c("Canopy", "Fine Root", "Coarse Root", "Understorey"), each=2)
    plotDF1$Trt <- rep(c("aCO2", "eCO2"), 4)
    plotDF1$pos <- with(plotDF1, mean + sd)
    plotDF1$neg <- with(plotDF1, mean - sd)
    
    ### Plot 2
    plotDF2 <- data.frame(c(inDF$aCO2[inDF$terms=="Microbial N Pool"], 
                            inDF$eCO2[inDF$terms=="Microbial N Pool"]), 
                          NA, NA)
    colnames(plotDF2) <- c("mean", "sd", "Variable")
    plotDF2$sd <- c(inDF$aCO2_sd[inDF$terms=="Microbial N Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Microbial N Pool"])
    plotDF2$Variable <- rep(c("Microbe"), each=2)
    plotDF2$Trt <- rep(c("aCO2", "eCO2"), 1)
    plotDF2$pos <- with(plotDF2, mean + sd)
    plotDF2$neg <- with(plotDF2, mean - sd)
    
    ### Plot 3
    plotDF3 <- data.frame(c(inDF$aCO2[inDF$terms=="Wood N Pool"], 
                            inDF$eCO2[inDF$terms=="Wood N Pool"]), 
                          NA, NA)
    colnames(plotDF3) <- c("mean", "sd", "Variable")
    plotDF3$sd <- c(inDF$aCO2_sd[inDF$terms=="Wood N Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Wood N Pool"])
    plotDF3$Variable <- rep(c("Wood"), each=2)
    plotDF3$Trt <- rep(c("aCO2", "eCO2"), 1)
    plotDF3$pos <- with(plotDF3, mean + sd)
    plotDF3$neg <- with(plotDF3, mean - sd)
    
    
    ### Plot 4
    plotDF4 <- data.frame(c(inDF$aCO2[inDF$terms=="Soil N Pool"], 
                            inDF$eCO2[inDF$terms=="Soil N Pool"]), 
                          NA, NA)
    colnames(plotDF4) <- c("mean", "sd", "Variable")
    plotDF4$sd <- c(inDF$aCO2_sd[inDF$terms=="Soil N Pool"], 
                    inDF$eCO2_sd[inDF$terms=="Soil N Pool"])
    plotDF4$Variable <- rep(c("Soil"), each=2)
    plotDF4$Trt <- rep(c("aCO2", "eCO2"), 1)
    plotDF4$pos <- with(plotDF4, mean + sd)
    plotDF4$neg <- with(plotDF4, mean - sd)
    
    ### Plotting
    p1 <- ggplot(plotDF1, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste("N pool (g N ", m^-2, ")")))+
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
    
    
    p2 <- ggplot(plotDF2, aes(x=Variable, y=mean))+
        geom_bar(stat = "identity", aes(fill=Trt), position="dodge")+
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(Trt)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        labs(x="", y=expression(paste("N pool (g N ", m^-2, ")")))+
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
        labs(x="", y=expression(paste("N pool (g N ", m^-2, ")")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=10), 
              axis.text.x = element_text(size=10),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
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
        labs(x="", y=expression(paste("N pool (g N ", m^-2, ")")))+
        theme_linedraw() +
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
    pdf("plots_tables/N_Pool_Summary_Plots.pdf", width=8,height=8)
    plot_grid(p1, p2, p3, p4, labels="", ncol=2, align="v", axis = "l",
              rel_heights = c(1, 1.2))
    grid.text(grid.labs, x = c(0.12, 0.62, 0.12, 0.62),
              y = c(0.95, 0.95, 0.5, 0.5), 
              gp=gpar(fontsize=14, col="black", fontface="bold"))
    dev.off()
    
    
    
}
