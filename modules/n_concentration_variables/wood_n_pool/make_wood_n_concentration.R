make_wood_n_concentration <- function() {
    
    ### download the data
    infile <- "FACE_P0079_RA_Wood_N_conc_RAW_V1.csv"
    if(!file.exists(paste0("download/", infile))) {
        downloadHIEv(hiev=searchHIEv(infile))
    }
    
    df <- read.csv(paste0("download/", infile))
    
    colnames(df) <- c("subset_code", "Date_collected", "Location", "Location_1", "Type",
                      "Tree", "N_conc", "C_conc", "CN")
    
    ### setting up the date
    df$Date <- paste0("1-", as.character(df$Date_collected))
    df$Date <- gsub("_", "-", df$Date)
    df$Date <- as.Date(df$Date, "%d-%b-%Y")
    
 
    ### Assign ring information based on tree number
    treeDF <- read.csv("download/FACE_AUX_RA_TREE-DESCRIPTIONS_R_20130201.csv",stringsAsFactors = FALSE)
    tree.list <- unique(df$Tree)
    tree.list <- tree.list[!is.na(tree.list)]
    
    for (i in tree.list) {
        df[df$Tree == i, "Ring"] <- treeDF[treeDF$Tree == i, "Ring"]
    }
    
    
    ### Wood n, average across rings and date, unit = %
    df.wood <- summaryBy(N_conc~Ring+Date,
                           data=df,FUN=mean,keep.names=T,na.rm=T)
    df.wood$month <- month(df.wood$Date)
    df.wood$year <- year(df.wood$Date)
    
    colnames(df.wood) <- c("Ring", "Date", "PercN", "month", "year")
    
    return(df.wood[,1:3])
    

}