
fit_leaf_p_with_gcc <- function() {
    ### This function fits leaf P with GCC
    
    green.df <- downloadCSV("FACE_P0037_RA_CANOPYGREENNESS-FULL_OPEN_L2.dat")
    names(green.df) <- c("DateTime","Date","Ring","angle","Green")
    
    ### average by ring and date
    green.sum.df <- doBy::summaryBy(Green~Date + Ring,
                                    data = green.df,FUN = mean,na.rm=TRUE,keep.names = TRUE)
    lai.gcc.df <- green.sum.df[complete.cases(green.sum.df),]
    
    ### normalize gcc
    lai.gcc.df$gcc.norm <-  (lai.gcc.df$Green - quantile(lai.gcc.df$Green,probs = 0.2,na.rm=T)[[1]]) / 
        (quantile(lai.gcc.df$Green,probs = 0.8)[[1]] - quantile(lai.gcc.df$Green,probs = 0.2,na.rm=T)[[1]])
    
    ### function to get the no days from Oct 1
    get.days.func <- function(lai.gcc.df){
        
        lai.gcc.df$ndays <- NA
        df.years <- unique(year(lai.gcc.df$Date))
        temp.ls <- list()
        for (i in seq_along(df.years)){
            s.date <- as.Date(paste0(df.years[i],"-10-01"))
            e.date <- as.Date(paste0(df.years[i] + 1,"-10-01"))
            temp.ls[[i]] <- lai.gcc.df[lai.gcc.df$Date >= as.Date(s.date) & lai.gcc.df$Date < as.Date(e.date),] 
            temp.ls[[i]]$ndays <- as.numeric(temp.ls[[i]]$Date - as.Date(s.date))
        }
        
        out.df <- do.call(rbind,temp.ls)
        return(out.df)
    }
    
    ### process number of days since 1 oct
    lai.gcc.df$Date <- as.Date(lai.gcc.df$Date)
    lai.gcc.df <- get.days.func(lai.gcc.df)

    ### read in leaf P data    
    df <- read.csv("temp_files/GreenLeaves_allP-clean_Mingkai.csv")
    
    ### setting up the date
    df$Date <- paste0("01-", as.character(df$Campaign))
    df$Date <- as.Date(df$Date, "%d-%b-%y")
    df <- get.days.func(df)
    
    # make the number of days smaller so the fitting doesn't go wild#####
    pheno.gcc.func <- function(day.num,a=66,b=7,c=0.01){
        x <- day.num/100
        return(a * x^b * c^x )
    }
    
    # Fit for GCC pheno####
    fit.ggc <- nls(gcc.norm~pheno.gcc.func(ndays,a,b,0.0005),data = lai.gcc.df,
                   start = list(a = 2000,b=9))
    
    # plot fitted leaf P concentration phenology
    df$treat <- NA
    df$treat[df$Ring %in% c(1,4,5)] <- 'E'
    df$treat[df$Ring %in% c(2,3,6)] <- 'A'
    df$treat <- as.factor(df$treat)
    df.pheno <- split(df,df$treat)
    
    
    fit.a <- nls(Perc.P~c((up.q - low.q) *
                             pheno.gcc.func(ndays,coef(fit.ggc)[[1]],coef(fit.ggc)[[2]],0.0005) +
                             low.q),
                 data = df.pheno[[1]],start = list(up.q=90, low.q=70))
    fit.e <- nls(Perc.P~c((up.q - low.q) *
                             pheno.gcc.func(ndays,coef(fit.ggc)[[1]],coef(fit.ggc)[[2]],0.0005) +
                             low.q),
                 data = df.pheno[[2]],start = list(up.q=90, low.q=70))
    
    
    fit.p.func <- function(in.vec,days.vec){
        fit.28 <- nls(in.vec~c((up.q - low.q) *
                                   pheno.gcc.func(days.vec,coef(fit.ggc)[[1]],coef(fit.ggc)[[2]],0.0005) +
                                   low.q),start = list(up.q=90, low.q=70))
        
        up.q <- coef(fit.28)[[1]]
        low.q <- coef(fit.28)[[2]]
        return(c(up.q,low.q))
    }
    
    pred.p.func <- function(in.vec,days.vec,pred.days.vec=1:365){
        # up.q <- quantile(in.vec,probs = 0.8)
        # low.q <- quantile(in.vec,probs = 0.2)
        
        fit.vec <- fit.p.func(in.vec,days.vec)
        up.q <- fit.vec[1]
        low.q <- fit.vec[2]
        p.min.df <- data.frame(x = pred.days.vec,
                                    y = (up.q - low.q) *
                                        pheno.gcc.func(pred.days.vec,coef(fit.ggc)[[1]],coef(fit.ggc)[[2]],0.0005) +
                                        low.q)
        p.min.df <- p.min.df[order(p.min.df$x),]
        return(p.min.df)
    }
    
    plot.p.fit <- function(v.vec,day.vec,col.in){
        p.min.df <- pred.p.func(v.vec,days.vec=day.vec)
        lines(p.min.df$x,p.min.df$y, lwd=2,col=col.in)
    }
    
    
    ### make a plot with ndays as x and percP as y
    #palette(c('blue','red'))
    #plot(Perc.P~ndays,data = df,xlim=c(0,365),
    #     col=treat,pch=16,cex=1,
    #     xlab='Days since Oct 1')
    #plot.p.fit(df.pheno[[1]]$Perc.P,df.pheno[[1]]$ndays,col.in = 'blue')
    #plot.p.fit(df.pheno[[2]]$Perc.P,df.pheno[[2]]$ndays,col.in = 'red')
    #legend('topright',legend = c('A','E'),lty='solid',col=c('blue','red'),bty='n')
    
    euc.leafp.df <- data.frame(p.a = pred.p.func(euc.vc.ls[[1]]$Perc.P,df.pheno[[1]]$ndays),
                               p.e = pred.p.func(euc.vc.ls[[2]]$Perc.P,df.pheno[[2]]$ndays))
    
    euc.4y.df <- data.frame(date = seq.Date(as.Date('2013-01-01'),
                                            as.Date('2016-12-31'),
                                            by='day'))
    
    euc.4y.df$ndays <- yday(euc.4y.df$date) - 91
    
    euc.4y.df$ndays[euc.4y.df$ndays < 0] <- euc.4y.df$ndays[euc.4y.df$ndays < 0] + 365
    
    euc.4y.ls <- split(euc.4y.df,year(euc.4y.df$date))
    
    euc.pred.ls <- list()
    for (i in 1:length(euc.4y.ls)){

        euc.pred.ls[[i]] <- data.frame(p.a = pred.p.func(euc.vc.ls[[1]]$Perc.P,df.pheno[[1]]$ndays,
                                                          euc.4y.ls[[i]]$ndays)$y,
                                       p.e = pred.p.func(euc.vc.ls[[2]]$Perc.P,df.pheno[[2]]$ndays,
                                                          euc.4y.ls[[i]]$ndays)$y,
                                       date = euc.4y.ls[[i]]$date)
        
    }
    
    euc.pred.df <- do.call(rbind,euc.pred.ls)
    
    temp.df <- data.frame(date = seq.Date(as.Date('2012-01-01'),
                                          as.Date('2018-12-31'),
                                          by='week'))
    
    out.df <- merge(temp.df,euc.pred.df,by='date')
    
    return(out.df)
}