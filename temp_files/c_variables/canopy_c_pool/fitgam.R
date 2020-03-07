#' Function for smoothplot. Probably not use otherwise.
fitgam <- function(X,Y,dfr, k=-1, R=NULL){
    dfr$Y <- dfr[,Y]
    dfr$X <- dfr[,X]
    if(!is.null(R)){
        dfr$R <- dfr[,R]
        model <- 2
    } else model <- 1
    dfr <- droplevels(dfr)
    
    k=15
    #browser()
    
    if(model ==1){
        g <- gam(Y ~ s(X, k= k), data=dfr)
        #g <- gam(Y ~ s(X), data=dfr)
        
    }
    if(model ==2){
        g <- gamm(Y ~ s(X, k=k), random = list(R=~1), data=dfr)
    }
    
    return(g)
}