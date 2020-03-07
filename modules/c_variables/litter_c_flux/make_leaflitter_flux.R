make_leaflitter_flux <- function(){
    
    download_leaflitter()
    
    #### read in data
    f16 <- read.csv(file.path(getToPath(), "FACE_P0017_RA_Litter_20160101-20161212-L1-V2.csv"))
    f15 <- read.csv(file.path(getToPath(), "FACE_P0017_RA_Litter_20150101-20151217-L1-V2.csv"))
    f14 <- read.csv(file.path(getToPath(), "FACE_P0017_RA_Litter_20140101-20141216-L1-V2.csv"))
    f13 <- read.csv(file.path(getToPath(), "FACE_P0017_RA_Litter_20121001-20131231-L1-V2.csv"))
    
    #### set up col names
    colnames(f13) <- c("Ring", "Date", "Trap", "Twig", "Bark", "Seed", "Leaf", "Other", "Insect", "Comments", "days.past")
    colnames(f14) <- c("Ring", "Date", "Trap", "Twig", "Bark", "Seed", "Leaf", "Other", "Insect", "Comments", "days.past")
    colnames(f15) <- c("Ring", "Date", "Trap", "Twig", "Bark", "Seed", "Leaf", "Other", "Insect", "Comments", "days.past")
    colnames(f16) <- c("Ring", "Date", "Trap", "Twig", "Bark", "Seed", "Leaf", "Other", "Insect", "Comments", "days.past")
    
    #### Merge the files
    litter_raw <- rbind(f13, f14, f15, f16) 
    
    # glitch fix
    litter_raw$Ring <- as.character(litter_raw$Ring)
    litter_raw$Trap <- as.character(litter_raw$Trap)
    litter_raw$Ring[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
    litter_raw$TRAP[is.na(litter_raw$Ring)] <- litter_raw$RING[is.na(litter_raw$Ring)]
    litter_raw$Twig <- as.numeric(litter_raw$Twig)
    litter_raw$Bark <- as.numeric(litter_raw$Bark)
    litter_raw$Seed <- as.numeric(litter_raw$Seed)
    litter_raw$Leaf <- as.numeric(litter_raw$Leaf)
    litter_raw$Other <- as.numeric(litter_raw$Other)
    litter_raw$Insect <- as.numeric(litter_raw$Insect)
    
    # remove two data points where big branches fall into litter bascket
    line.num <- which.max(litter_raw$Twig)
    litter_raw <- litter_raw[-line.num,]
    
    line.num <- which.max(litter_raw$Twig)
    litter_raw <- litter_raw[-line.num,]
    
    c_frac <- c_fraction
    
    # Conversion factor from g basket-1 to mg m-2
    conv <- c_frac * 1000 / frass_basket_area
    
    litter <- dplyr::mutate(litter_raw, 
                            Date = as.Date(litter_raw$Date, format = "%d/%m/%Y"),
                            Start_date = Date - days.past,
                            End_date = Date,
                            Leaf = as.numeric(Leaf) * conv / days.past)
    
    # Averages by Ring
    litter_a <- summaryBy(Leaf ~ Date + Ring, FUN=mean, na.rm=TRUE,
                          data=litter, id = ~Start_date + End_date, keep.names=TRUE)
    
    # return leaf flux in unit of mg m-2 d-1 (not C or P, just leaf litter mass!!!)
    litter_a <- as.data.frame(dplyr::rename(litter_a,
                                            leaf_flux = Leaf))
    
    return(litter_a)
}
