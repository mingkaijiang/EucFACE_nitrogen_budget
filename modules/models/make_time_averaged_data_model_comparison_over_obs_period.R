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
    
    
    
    
    
    
    
    ################# PUE ####################
    subDF1 <- subset(budgetDF, Variable%in%c("PUE"))
    subDF1$meanvalue <- ifelse(is.infinite(subDF1$meanvalue), NA, subDF1$meanvalue)  
    
    
    plotDF1 <- subDF1[subDF1$Trt=="aCO2",]
    plotDF2 <- subDF1[subDF1$Trt=="pct_diff",]
    
    
    ## stoichiometry
    p11 <- ggplot(data=plotDF1, 
                  aes(Group, meanvalue, group=Group)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position="dodge", col="black") +
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
        ylab(expression("PUE (g C " * g^-1 * " P)"))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Model",
                          values=c(col.values, obs="black"),
                          labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p11
    
    
    
    
    p12 <- ggplot(data=plotDF2, 
                  aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position=position_dodge2(), col="black") +
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
        guides(fill=guide_legend(nrow=6)); p12
    
    
    
    
    ################# Plab ####################
    plabDF <- prepare_plot_DF_for_time_averaged_data_model_intercomparison(eucDF=eucDF,
                                                                           ambDF=ambDF.sum,
                                                                           eleDF=eleDF.sum,
                                                                           difDF=annDF.diff.sum,
                                                                           var.list=c("PLAB"),
                                                                           calculate.total=F)
    
    
    
    
    ### split into ambDF, pctDF
    plotDF1 <- plabDF[plabDF$Trt=="aCO2"&plabDF$Variable%in%c("PLAB"),]
    
    plotDF2 <- plabDF[plabDF$Trt=="pct_diff",]
    
    ### ELMV1 assumes top 1 m soil, not top 10 cm
    plotDF1$meanvalue[plotDF1$Group=="B_ELMV1"] <- plotDF1$meanvalue[plotDF1$Group=="B_ELMV1"]/ 10
    plotDF1$sdvalue[plotDF1$Group=="B_ELMV1"] <- plotDF1$sdvalue[plotDF1$Group=="B_ELMV1"]/ 10
    
    ## plab pool
    #require(ggbreak)
    
    p13 <- ggplot(data=plotDF1, 
                  aes(Group, meanvalue, group=Group)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position="dodge", col="black") +
        geom_errorbar(data=plotDF1, 
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
        geom_point(data=plotDF1, aes(x=Group, y=meanvalue), 
                   position=position_dodge2(width=0.9), col="black",
                   fill="white", size=2, pch=21)+
        geom_vline(xintercept=c(6.5, 8.5, 10.5), lty=2)+
        theme_linedraw() +
        #scale_y_break(c(2,10))+
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
        ylab(expression(P[lab] * " (g P " * m^-2 * ")"))+
        scale_x_discrete(limit=c(mod.list, "obs"),
                         label=c(model.labels, "obs" = expression(bold("OBS"))))+
        scale_fill_manual(name="Model",
                          values=c(col.values, obs="black"),
                          labels=c(model.labels, "obs"= "OBS"))+
        guides(fill=guide_legend(nrow=6));p13
    
    
    
    
    p14 <- ggplot(data=plotDF2, 
                  aes(Group, meanvalue)) +
        geom_bar(stat = "identity", aes(fill=Group), 
                 position=position_dodge2(), col="black") +
        geom_errorbar(data=plotDF2, 
                      aes(x=Group, ymin=meanvalue-sdvalue,
                          ymax=meanvalue+sdvalue), 
                      col="black", 
                      position=position_dodge2(), width=0.3)+
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
        guides(fill=guide_legend(nrow=6)); p14
    
    pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_P_variables.pdf"), 
        width=16, height=16)
    plot_grid(p1, p2,   # Pveg
              p5, p6,   # pupt and pmin
              #p11, p12, # PUE
              #p9, p10,  # CP ratio
              p13, p14, # Plab
              labels=c("(a)", "(b)", "(c)", "(d)",
                       "(e)", "(f)"), label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=2)
    dev.off()
    
    
    pdf(paste0(out.dir, "/MIP_time_averaged_", scenario, "_comparison_P_variables_SI.pdf"), 
        width=16, height=16)
    plot_grid(p3, p4,   # Pgrowth
              #p7, p8,   # Pupt over requirement
              p11, p12, # PUE
              p9, p10,  # CP ratio
              
              labels=c("(a)", "(b)",
                       "(c)", "(d)",
                       "(e)", "(f)"),
              #"(g)", "(h)"), 
              label_x=0.1, label_y=0.95,
              label_size=24,
              ncol=2)
    dev.off()
    
    
    ###########################################################################
    ##### Step 4. Time-varying validation
    #
    ##### Leaf area index
    #### A time series LAI data over the period of 2012 - 2016 was provided for validation purpose. 
    #### Models should aim to match the magnitude of LAI as well as its temporal patterns. 
    #### Note that in the observed dataset, the LAI data is really indicative of the vegetation structure as well as canopy leaf area. 
    #### validation LAI
    #laiDF <- read.csv("validation_dataset/EucFACE_LAI_2012_2016.csv")
    #laiDF <- laiDF[laiDF$Trt=="aCO2",]
    #laiDF$Date <- as.Date(as.character(laiDF$Date))
    #laiDF$ModName <- "OBS"
    #names(laiDF)[names(laiDF)=="lai"] <- "LAI"
    #laiDF <- laiDF[,c("Date", "LAI", "ModName")]
    #
    #
    #### read in multi-model lai data
    #modDF <- readRDS(paste0(out.dir, "/MIP_obs_var_amb_daily.rds"))
    #
    #### simulated LAI, subset
    #subDF <- subset(modDF, YEAR <= 2016)
    #subDF <- subDF[,c("YEAR", "DOY", "Date", "LAI", "ModName")]
    #subDF$Date <- as.Date(as.character(subDF$Date))
    #subDF <- subDF[,c("Date", "LAI", "ModName")]
    #    
    #### there is something wrong with LPJGP-VD, we may need to do something about it later
    ##tDF <- subDF[subDF$ModName=="L_LPJGP-VD",]
    ##summary(tDF$LAI)
    #
    #### merge the two dataset
    #testDF1 <- rbind(subDF, laiDF)
    #
    #### plot all data
    #p1 <- ggplot(testDF1, aes(x=Date)) +
    #    geom_line(aes(y=LAI, color=ModName, lty=ModName), lwd = 1) +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=12),
    #          panel.grid.major=element_blank(),
    #          plot.title = element_text(size = 10, face = "bold"),
    #          legend.position="right")+
    #    scale_color_manual(name="Model",
    #                       values=c(col.values, "OBS"="black"),
    #                       labels=c(model.labels, "OBS"= "OBS"))+
    #    scale_linetype_manual(name="Model", 
    #                          values=c(linetype.values, "OBS"=1),
    #                          labels=c(model.labels, "OBS"="OBS"))+
    #    guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
    #                                                   lty = c(linetype.values, "OBS"=1))),
    #           color = guide_legend(nrow=12, byrow=F))+
    #    ylab("LAI"); p1
    #
    #
    ##### Soil respiration
    #### The measured soil respiration rate represents both root 
    #### and soil heterotrophic respiration flux. 
    #### It was up-scaled from the LICOR chambers by averaging 
    #### all measurements within the same treatment. 
    #### It was a model product, 
    #### in that we used DAMM model to establish relationship with soil temperature, 
    #### and then obtained the daily rate throughout the year. 
    #### Nevertheless, we expect modelers to provide a good match simulation to this dataset. 
    #
    #### Note that we didn't ask the modelers to output soil respiration flux in the output protocol. 
    #### Please add heterotrophic respiration and root respiration to obtain soil respiration flux. 
    #### Also, please note that, the unit for all carbon fluxes is given in the output protocol, as gC m-2 d-1. 
    #### validation Rsoil
    #rsoilDF <- read.csv("validation_dataset/EucFACE_daily_soil_respiration_flux_2013_2015.csv")
    #rsoilDF <- rsoilDF[rsoilDF$Trt=="aCO2",]
    #rsoilDF$Date <- as.Date(as.character(rsoilDF$Date))
    #
    #### convert unit, from mg m-2 d-1 to g m-2 d-1
    #rsoilDF$Rsoil <- rsoilDF$Rsoil_mg_m2_d / 1000.0
    #rsoilDF$ModName <- "OBS"
    #rsoilDF <- rsoilDF[,c("Date", "Rsoil", "ModName")]
    #
    #### simulated Rsoil, subset
    #subDF <- subset(modDF, YEAR <= 2015 & YEAR > 2012)
    #subDF <- subDF[,c("YEAR", "DOY", "Date", "RHET", "RCR", "RFR", "ModName")]
    #subDF$Date <- as.Date(as.character(subDF$Date))
    #
    #subDF[subDF<=-999.] <- NA
    #
    #subDF$Rsoil<- rowSums(data.frame(subDF$RHET, subDF$RCR, subDF$RFR), na.rm=T)
    #subDF <- subDF[,c("Date", "Rsoil", "ModName")]
    #
    #
    #### merge the two dataset
    #testDF1 <- rbind(subDF, rsoilDF)
    #
    #### plot all data
    #p2 <- ggplot(testDF1, aes(x=Date)) +
    #    geom_line(aes(y=Rsoil, color=ModName, lty=ModName), lwd = 1) +
    #    theme_linedraw() +
    #    theme(panel.grid.minor=element_blank(),
    #          axis.title.x = element_text(size=14), 
    #          axis.text.x = element_text(size=12),
    #          axis.text.y=element_text(size=12),
    #          axis.title.y=element_text(size=14),
    #          legend.text=element_text(size=12),
    #          legend.title=element_text(size=12),
    #          panel.grid.major=element_blank(),
    #          plot.title = element_text(size = 10, face = "bold"),
    #          legend.position="right")+
    #    scale_color_manual(name="Model",
    #                       values=c(col.values, "OBS"="black"),
    #                       labels=c(model.labels, "OBS"= "OBS"))+
    #    scale_linetype_manual(name="Model", 
    #                          values=c(linetype.values, "OBS"=1),
    #                          labels=c(model.labels, "OBS"="OBS"))+
    #    guides(fill = guide_legend(override.aes = list(col = c(col.values, "OBS"="black"),
    #                                                   lty = c(linetype.values, "OBS"=1))),
    #           color = guide_legend(nrow=12, byrow=F))+
    #ylab("Soil respiration flux"); p2
    #
    #
    #
    #
    #### print plots to file, change numbering if needed
    #pdf(paste0(out.dir, '/MIP_Time_varying_variables.pdf',sep=''),width=12,height=8)
    #for (i in 1:2) {
    #    print(get(paste("p",i,sep="")))
    #}
    #dev.off()
    #
    ###########################################################################
}

    