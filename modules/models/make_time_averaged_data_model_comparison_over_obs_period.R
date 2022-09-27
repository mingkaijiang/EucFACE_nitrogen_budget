make_time_averaged_data_model_comparison_over_obs_period <- function(nconDF,
                                                                     nfluxDF,
                                                                     npoolDF,
                                                                     cpoolDF,
                                                                     cfluxDF,
                                                                     nbudgetDF,
                                                                     cnDF,
                                                                     npDF,
                                                                     scenario) {
    
    
    ##################################################################
    ### Purpose:
    ### to compare model predictions against data,
    ### including ambient and elevated treatment means,
    ### stoichiometry, efficiency, residence time, etc.,
    ### and the CO2 response difference and ratio,
    ### try to include all relevant variables.
    
    
    ##################################################################
    #### Set up basics
    ### setting out path to store the files
    ### this is only valid for variable climate
    out.dir <- paste0(getwd(), "/output/mip/obs/")
    
    ### create output folder
    if(!dir.exists(out.dir)) {
        dir.create(out.dir, showWarnings = FALSE)
    }
    
    
    ### read in anual datasets
    ambDF <- readRDS(paste0("output/mip/input/MIP_OBS_", scenario, "_AMB_annual.rds"))
    eleDF <- readRDS(paste0("output/mip/input/MIP_OBS_", scenario, "_ELE_annual.rds"))
    
    d <- dim(ambDF)[2]
    
    ### remove N models
    #ambDF <- ambDF[ambDF$ModName!="I_GDAYN",]
    #ambDF <- ambDF[ambDF$ModName!="J_LPJGN",]
    #eleDF <- eleDF[eleDF$ModName!="I_GDAYN",]
    #eleDF <- eleDF[eleDF$ModName!="J_LPJGN",]
    
    
    #### calculate 5-yr means in the simulation datasets
    ambDF <- subset(ambDF, YEAR>2012 & YEAR<2018)
    eleDF <- subset(eleDF, YEAR>2012 & YEAR<2018)
    
    ### calculate % difference
    annDF.pct.diff <- ambDF
    annDF.pct.diff[,3:d] <- (eleDF[,3:d]-ambDF[,3:d])/ambDF[,3:d] * 100.0
    
    
    ambDF.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                           data=ambDF,
                           keep.names=T, na.rm=T)
    
    eleDF.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                           data=eleDF,
                           keep.names=T, na.rm=T)
    
    annDF.diff.sum <- summaryBy(.~ModName, FUN=c(mean,sd),
                                data=annDF.pct.diff,
                                keep.names=T, na.rm=T)
    
    
    ### get the list of models
    mod.list <- unique(ambDF.sum$ModName)
    nmod <- length(mod.list)
    

    
    ##########################################################################
    ### Plotting
    
    ################# Major carbon pools  ####################
    ### Firstly we will check the major carbon pools, 
    ### as these data are provided in Table 1 in the parameter file. 
    ### Note that:
    ### * CFR combines fineroot (< 2 mm in diameter) and intermediate root (2-3 mm) in the observation;
    ### * CL includes overstorey leaf only in the observation;
    ### * CW includes branch and stem in the model simulation.
    vegDF <- prepare_mip_veg_n_pool(npoolDF=npoolDF,
                                    ambDF=ambDF.sum,
                                    eleDF=eleDF.sum,
                                    difDF=annDF.diff.sum,
                                    var.list=c("NL", "NW", "NFR", "NCR", "NSTOR"),
                                    calculate.total=T)
    
    
    
    ### split into ambDF, pctDF
    plotDF1 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("NL","NW","NCR","NFR","NSTOR"),]
    plotDF2 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("Tot"),]
    
    plotDF3 <- vegDF[vegDF$Trt=="pct_diff"&vegDF$Variable%in%c("Tot"),]
    
    ### Plotting
    ### additional to-do list:
    ### 1. fill color by manual selection
    p1 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        geom_errorbar(data=plotDF2, 
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        geom_point(data=plotDF2,aes(Group, meanvalue), 
                   size=2,fill="white", pch=21, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position=c(.85,.2),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Nitrogen pools (g B " * m^2*")")))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        guides(fill=guide_legend(nrow=3))+
        scale_fill_manual(name=expression(N[veg]),
                          values=c("NL"=cbbPalette[2],
                                   "NW"=cbbPalette[3],
                                   "NFR"=cbbPalette[4],
                                   "NCR"=cbbPalette[7],
                                   "NSTOR"=cbbPalette[8]),
                          labels=c("NL"=expression(N[leaf]), 
                                   "NW"=expression(N[wood]), 
                                   "NFR"=expression(N[froot]), 
                                   "NCR"=expression(N[croot]),
                                   "NSTOR"=expression(N[store])));p1
    
    
    p2 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group),
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        geom_point(aes(Group, meanvalue), 
                   size=2,fill="white", pch=21, col="black") +
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * " effect (%)"))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Model",
                          values=c(col.values, obs="black"),
                          labels=c(model.labels, "obs"= "OBS"))+
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "obs"="black"))),
               color = guide_legend(nrow=12, byrow=F));p2
    
    
    
    ################# Delta C pools  ####################
    #vegDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
    #                                                                      ambDF=ambDF.sum,
    #                                                                      eleDF=eleDF.sum,
    #                                                                      difDF=annDF.diff.sum,
    #                                                                      var.list=c("deltaCL", "deltaCW", "deltaCFR", "deltaCCR", "deltaCSTOR"),
    #                                                                      calculate.total=T)
    #
    #### split into ambDF, pctDF
    #plotDF1 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("deltaCL", "deltaCW", "deltaCFR", "deltaCCR", "deltaCSTOR"),]
    #plotDF2 <- vegDF[vegDF$Trt=="aCO2"&vegDF$Variable%in%c("Tot"),]
    #
    #plotDF3 <- vegDF[vegDF$Trt=="diff"&vegDF$Variable%in%c("Tot"),]
    #
    #### Plotting
    #### additional to-do list:
    #### 1. fill color by manual selection
    #p5 <- ggplot(data=plotDF1, 
    #             aes(Group, meanvalue)) +
    #    geom_bar(stat = "identity", aes(fill=Variable), 
    #             position="stack", col="black") +
    #    geom_errorbar(data=plotDF2, 
    #                  aes(x=Group, ymin=meanvalue-sdvalue,
    #                      ymax=meanvalue+sdvalue), 
    #                  col="black", 
    #                  position=position_dodge2(), width=0.3)+
    #    geom_point(data=plotDF2, aes(x=Group, y=meanvalue), col="black",
    #               fill="white", size=2, pch=21)+
    #    geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
    #    xlab("")+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=14),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position=c(.1,.2),
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          plot.title = element_text(size=14, face="bold.italic", 
    #                                    hjust = 0.5))+
    #    ylab(expression(paste(Delta * C[veg] * " (g C " * m^2 * " " * yr^-1 * ")")))+
    #    scale_x_discrete(limit=c(mod.list, "obs"),
    #                     label=c(model.labels, "obs" = expression(bold("OBS"))))+
    #    scale_fill_manual(name=expression(C[veg]),
    #                      values=c("deltaCL"=cbbPalette[2],
    #                               "deltaCW"=cbbPalette[3],
    #                               "deltaCFR"=cbbPalette[4],
    #                               "deltaCCR"=cbbPalette[7],
    #                               "deltaCSTOR"=cbbPalette[8]),
    #                      labels=c("deltaCL"=expression(Delta*C[leaf]), 
    #                               "deltaCW"=expression(Delta*C[wood]), 
    #                               "deltaCFR"=expression(Delta*C[froot]), 
    #                               "deltaCCR"=expression(Delta*C[croot]),
    #                               "deltaCSTOR"=expression(Delta*C[store]))); p5
    #
    #
    #p6 <- ggplot(data=plotDF3, 
    #             aes(Group, meanvalue)) +
    #    geom_bar(stat = "identity", aes(fill=Group),
    #             position="stack", col="black") +
    #    geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
    #                      ymax=meanvalue+sdvalue), 
    #                  col="black", 
    #                  position=position_dodge2(), width=0.3)+
    #    geom_point(data=plotDF3, aes(x=Group, y=meanvalue), col="black",
    #               fill="white", size=2, pch=21)+
    #    geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
    #    xlab("")+
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.text.x=element_text(size=12),
    #          axis.title.x=element_text(size=14),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=14),
    #          panel.grid.major=element_blank(),
    #          legend.position="none",
    #          legend.box = 'horizontal',
    #          legend.box.just = 'left',
    #          plot.title = element_text(size=14, face="bold.italic", 
    #                                    hjust = 0.5))+
    #    ylab(expression(CO[2] * " effect (g C " * m^2 * " " * yr^-1 * ")"))+
    #    scale_x_discrete(limit=c(mod.list, "obs"),
    #                     label=c(model.labels, "obs" = expression(bold("OBS"))))+
    #    scale_fill_manual(name="Model",
    #                      values=c(col.values, obs="black"),
    #                      labels=c(model.labels, "obs"= "OBS"))+
    #    guides(fill = guide_legend(override.aes = list(col = c(col.values, "obs"="black"))),
    #           color = guide_legend(nrow=12, byrow=F)); p6
    
   
    
   
    ################# Major growth N fluxes  ####################
    myDF <- prepare_mip_veg_growth_n_flux(nfluxDF=nfluxDF,
                                           ambDF=ambDF.sum,
                                           eleDF=eleDF.sum,
                                           difDF=annDF.diff.sum,
                                           var.list=c("NGL", "NGW", "NGCR", "NGFR"),
                                           calculate.total=T)
    
    
    ### split into ambDF, pctDF
    plotDF1 <- myDF[myDF$Trt=="aCO2"&myDF$Variable%in%c("NGL", "NGW", "NGCR", "NGFR"),]
    plotDF2 <- myDF[myDF$Trt=="aCO2"&myDF$Variable%in%c("Tot"),]
    
    plotDF3 <- myDF[myDF$Trt=="pct_diff"&myDF$Variable%in%c("Tot"),]
    
    
    ### plotting 
    p3 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position="stack", col="black") +
        geom_errorbar(data=plotDF2,
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        #geom_point(data=plotDF2, aes(x=Group, y=meanvalue), col="black",
        #         fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position=c(.8,.8),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste(N[growth] * " (g N " * m^2 * " " * yr^-1 * ")")))+
        scale_fill_manual(name=expression(N[growth]),
                          values=c("NGL"=cbbPalette[2],
                                   "NGW"=cbbPalette[3],
                                   "NGFR"=cbbPalette[4],
                                   "NGCR"=cbbPalette[7]),
                          labels=c("NGL"=expression(N[gleaf]), 
                                   "NGW"=expression(N[gwood]), 
                                   "NGFR"=expression(N[gfroot]), 
                                   "NGCR"=expression(N[gcroot])))+
        guides(fill=guide_legend(nrow=2))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS")))); p3
    
    
    p4 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position="stack", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        geom_point(data=plotDF3, aes(x=Group, y=meanvalue), col="black",
                   fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * " effect (%)"))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Model",
                          values=c(col.values, obs="black"),
                          labels=c(model.labels, "obs"= "OBS"))+
        guides(fill = guide_legend(override.aes = list(col = c(col.values, "obs"="black"))),
               color = guide_legend(nrow=12, byrow=F)); p4
    
    
    
    
    
    ################# N uptake and mineralization ####################
    myDF <- prepare_mip_veg_n_upt_flux(nbudgetDF=nbudgetDF,
                                        ambDF=ambDF.sum,
                                        eleDF=eleDF.sum,
                                        difDF=annDF.diff.sum,
                                        var.list=c("NUP", "NMIN"),
                                        calculate.total=F)
    
    
    ### split into ambDF, pctDF
    plotDF1 <- myDF[myDF$Trt=="aCO2",]
    plotDF2 <- myDF[myDF$Trt=="aCO2",]
    
    plotDF3 <- myDF[myDF$Trt=="pct_diff",]
    
    
    ### plotting 
    p5 <- ggplot(data=plotDF1, 
                 aes(Group, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position=position_dodge2(), col="black") +
        geom_errorbar(data=plotDF2,
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_point(data=plotDF2, aes(x=Group, y=meanvalue), 
                   position=position_dodge2(width=0.9), col="black",
                   fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position=c(.1,.2),
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(paste("Nitrogen fluxes (g N " * m^2 * " " * yr^-1 * ")")))+
        scale_fill_manual(name=expression(N[fluxes]),
                          values=c("NMIN"=alpha("red3", 0.5),
                                   "NUP"=alpha("blue3", 0.5)),
                          labels=c("NMIN"=expression(N[net]), 
                                   "NUP"=expression(N[upt])))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS")))); p5
    
    
    #require(ggbreak)
    
    p6 <- ggplot(data=plotDF3, 
                 aes(Group, meanvalue, group=Variable)) +
        geom_bar(stat = "identity", aes(fill=Variable), 
                 position=position_dodge2(), col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue, grou=Variable), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_point(data=plotDF3, aes(x=Group, y=meanvalue), 
                   position=position_dodge2(width=0.9), col="black",
                   fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        xlab("")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab(expression(CO[2] * " effect (%)"))+
        #scale_y_break(c(-400, -30, 50, 410))+
        scale_fill_manual(name=expression(N[fluxes]),
                          values=c("NMIN"=alpha("red3", 0.5),
                                   "NUP"=alpha("blue3", 0.5)),
                          labels=c("NMIN"=expression(N[net]), 
                                   "NUP"=expression(N[upt])))+
        ylim(c(-30, 100))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))));p6
    
    
    pdf(paste0(out.dir, "/mip_time_averaged_", scenario, "_comparison_n_variables.pdf"), 
        width=16, height=16)
    plot_grid(p1, p2,
              p3, p4,
              p5, p6, 
              labels=c("(a)", "(b)", "(c)", "(d)",
                       "(e)", "(f)"), label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=2)
    dev.off()
    
    
    
    
    ################# CN ratio ####################
    myDF <- prepare_mip_cn_ratio(cnDF=cnDF,
                                 ambDF=ambDF.sum,
                                 eleDF=eleDF.sum)
    
    
    ### split into ambDF, pctDF
    plotDF1 <- myDF[myDF$Variable=="canopy",]
    plotDF2 <- myDF[myDF$Variable=="leaflitter",]
    plotDF3 <- myDF[myDF$Variable=="wood",]
    plotDF4 <- myDF[myDF$Variable=="fineroot",]
    plotDF5 <- myDF[myDF$Variable=="soil_0_10",]

    
    
    ## stoichiometry
    p11 <- ggplot(data=plotDF1, 
                  aes(Group, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue, group=Trt), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Canopy CN ratio")+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        #scale_fill_manual(name="Model",
        #                  values=c(col.values, obs="black"),
        #                  labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p11
    
    
    
    
    p12 <- ggplot(data=plotDF2, 
                  aes(Group, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue, group=Trt), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Leaflitter CN ratio")+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        #scale_fill_manual(name="Model",
        #                  values=c(col.values, obs="black"),
        #                  labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p12
    
    
    p13 <- ggplot(data=plotDF3, 
                  aes(Group, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue, group=Trt), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Wood CN ratio")+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        #scale_fill_manual(name="Model",
        #                  values=c(col.values, obs="black"),
        #                  labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p13
    
    
    p14 <- ggplot(data=plotDF4, 
                  aes(Group, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue, group=Trt), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Fineroot CN ratio")+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        #scale_fill_manual(name="Model",
        #                  values=c(col.values, obs="black"),
        #                  labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p14
    
    
    
    p15 <- ggplot(data=plotDF5, 
                  aes(Group, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue, group=Trt), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Soil CN ratio (0-10cm)")+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        #scale_fill_manual(name="Model",
        #                  values=c(col.values, obs="black"),
        #                  labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p15
    
    
    
    
    
    
    pdf(paste0(out.dir, "/mip_time_averaged_", scenario, "_comparison_cn_ratios.pdf"), 
        width=16, height=16)
    plot_grid(p11, p12,  
              p13, p14,
              p15,
              labels=c("(a)", "(b)", "(c)", "(d)",
                       "(e)"), label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=2)
    dev.off()
    
    
    
    ################# NP ratio ####################
    myDF <- prepare_mip_np_ratio(npDF=npDF,
                                 ambDF=ambDF.sum,
                                 eleDF=eleDF.sum)
    
    
    ### split into ambDF, pctDF
    plotDF1 <- myDF[myDF$Variable=="canopy",]
    plotDF2 <- myDF[myDF$Variable=="leaflitter",]
    plotDF3 <- myDF[myDF$Variable=="wood",]
    plotDF4 <- myDF[myDF$Variable=="fineroot",]
    plotDF5 <- myDF[myDF$Variable=="soil_0_10",]
    
    
    
    ## stoichiometry
    p11 <- ggplot(data=plotDF1, 
                  aes(Group, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue, group=Trt), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Canopy NP ratio")+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        #scale_fill_manual(name="Model",
        #                  values=c(col.values, obs="black"),
        #                  labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p11
    
    
    
    
    p12 <- ggplot(data=plotDF2, 
                  aes(Group, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue, group=Trt), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Leaflitter NP ratio")+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        #scale_fill_manual(name="Model",
        #                  values=c(col.values, obs="black"),
        #                  labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p12
    
    
    p13 <- ggplot(data=plotDF3, 
                  aes(Group, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue, group=Trt), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Wood NP ratio")+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        #scale_fill_manual(name="Model",
        #                  values=c(col.values, obs="black"),
        #                  labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p13
    
    
    p14 <- ggplot(data=plotDF4, 
                  aes(Group, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue, group=Trt), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Fineroot NP ratio")+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        #scale_fill_manual(name="Model",
        #                  values=c(col.values, obs="black"),
        #                  labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p14
    
    
    
    p15 <- ggplot(data=plotDF5, 
                  aes(Group, meanvalue, group=Trt)) +
        geom_bar(stat = "identity", aes(fill=Trt), 
                 position="dodge", col="black") +
        geom_errorbar(aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue, group=Trt), 
                      col="black", 
                      position=position_dodge2(), width=0.9)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.text.x=element_text(size=12),
              axis.title.x=element_text(size=14),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.box = 'horizontal',
              legend.box.just = 'left',
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"),
              plot.title = element_text(size=14, face="bold.italic", 
                                        hjust = 0.5))+
        ylab("Soil NP ratio (0-10cm)")+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        #scale_fill_manual(name="Model",
        #                  values=c(col.values, obs="black"),
        #                  labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p15
    
    
    
    
    
    
    pdf(paste0(out.dir, "/mip_time_averaged_", scenario, "_comparison_np_ratios.pdf"), 
        width=16, height=16)
    plot_grid(p11, p12,  
              p13, p14,
              p15,
              labels=c("(a)", "(b)", "(c)", "(d)",
                       "(e)"), label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=2)
    dev.off()
    
    
}

    