#' Plot a generalized additive model
#' @param x Variable for X axis (unquoted)
#' @param y Variable for Y axis (unquoted)
#' @param data Dataframe containing x and y
#' @param kgam the \code{k} parameter for smooth terms in gam.
#' @param R An optional random effect (quoted)
#' @param log Whether to add log axes for x or y (but no transformations are done).
#' @param fitoneline Whether to fit only 
smoothplot <- function(x,y,g=NULL,data,
                       fittype=c("gam","lm"),
                       kgam=4,
                       R=NULL,
                       randommethod=c("lmer","aggregate"),
                       log="",
                       axes=TRUE,
                       fitoneline=FALSE,
                       pointcols=NULL,
                       linecols=NULL, 
                       xlab=NULL, ylab=NULL,
                       polycolor=alpha("lightgrey",0.7),
                       plotit=TRUE, add=FALSE,
                       npred=101,
                       ...){
    
    fittype <- match.arg(fittype)
    randommethod <- match.arg(randommethod)
    if(log != "")require(magicaxis)
    
    if(!is.null(substitute(g))){
        data$G <- as.factor(eval(substitute(g),data))
    } else {
        fitoneline <- TRUE
        data$G <- 1
    }
    data$X <- eval(substitute(x),data)
    data$Y <- eval(substitute(y),data)
    data <- droplevels(data)
    
    data <- data[!is.na(data$X) & !is.na(data$Y) & !is.na(data$G),]
    nlev <- length(unique(data$G))
    if(length(polycolor) == 1)polycolor <- rep(polycolor,nlev)
    
    if(class(data$X) == "Date"){
        xDate <- TRUE
        data$X <- as.numeric(data$X)
    } else {
        xDate <- FALSE
    }
    
    if(is.null(pointcols))pointcols <- palette()
    if(is.null(linecols))linecols <- palette()
    
    if(is.null(xlab))xlab <- substitute(x)
    if(is.null(ylab))ylab <- substitute(y)
    
    # If randommethod = aggregate, average by group and fit simple gam.
    if(!is.null(R) && randommethod == "aggregate"){
        data$R <- data[,R]
        
        data <- summaryBy(. ~ R, FUN=mean, na.rm=TRUE, keep.names=TRUE, data=data,
                          id=~G)
        R <- NULL
    }
    
    if(!fitoneline){
        
        d <- split(data, data$G)
        
        if(fittype == "gam"){
            fits <- lapply(d, function(x)try(fitgam("X","Y",x, k=kgam, R=R)))
            if(!is.null(R))fits <- lapply(fits, "[[", "gam")
        } else {
            fits <- lapply(d, function(x)lm(Y ~ X, data=x))
        }
        hran <- lapply(d, function(x)range(x$X, na.rm=TRUE))
    } else {
        if(fittype == "gam"){
            fits <- list(fitgam("X","Y",data, k=kgam, R=R))
            if(!is.null(R))fits <- lapply(fits, "[[", "gam")
        } else {
            fits <- list(lm(Y ~ X, data=data))
        }
        hran <- list(range(data$X, na.rm=TRUE))
        
    }
    
    if(plotit){
        if(xDate){
            data$X <- as.Date(data$X, origin="1970-1-1")
        }
        
        if(!add){
            with(data, plot(X, Y, axes=FALSE, pch=16, col=pointcols[G],
                            xlab=xlab, ylab=ylab, ...))
        } else {
            with(data, points(X, Y, pch=16, col=pointcols[G],
                              ...))
        }
        
        if(!add && axes){
            if(log=="xy")magaxis(side=1:2, unlog=1:2)
            if(log=="x"){
                magaxis(side=1, unlog=1)
                axis(2)
                box()
            }
            if(log=="y"){
                magaxis(side=2, unlog=2)
                axis(1)
                box()
            }
            if(log==""){
                if(xDate)
                    axis.Date(1, data$X)
                else
                    axis(1)
                axis(2)
                box()
            }
        }
        
        for(i in 1:length(fits)){
            
            if(fittype == "gam"){
                nd <- data.frame(X=seq(hran[[i]][1], hran[[i]][2], length=npred))
                if(!inherits(fits[[i]], "try-error")){
                    p <- predict(fits[[i]],nd,se.fit=TRUE)
                    addpoly(nd$X, p$fit-2*p$se.fit, p$fit+2*p$se.fit, col=polycolor[i])
                    lines(nd$X, p$fit, col=linecols[i], lwd=2)
                }
            }
            if(fittype == "lm"){
                pval <- summary(fits[[i]])$coefficients[2,4]
                LTY <- if(pval < 0.05)1 else 5
                predline(fits[[i]], col=linecols[i], lwd=2, lty=LTY)
            }
        }
    }
    return(invisible(fits))
}
