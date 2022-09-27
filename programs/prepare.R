
if(!dir.exists("download"))dir.create("download")

if(!require(HIEv)){
  stop("Install the HIEv package first from bitbucket.org/remkoduursma/HIEv")
}

setToken(tokenfile="tokenfile.txt", quiet=TRUE)
setToPath("download")

if(!require(pacman))install.packages("pacman")
pacman::p_load(dplyr, 
               doBy, 
               readxl, 
               lubridate,
               gdata,
               ggplot2,
               knitr,
               imputeTS,
               lme4,
               RColorBrewer,
               car,
               matrixStats,
               multcomp,
               grid,       # plot
               cowplot,    # plot
               mgcv)       # gam


# Loading constants
source("definitions/constants.R")

# Sourcing all R files in the modules subdirectory
sourcefiles <- dir("modules", pattern="[.]R$", recursive = TRUE, full.names = TRUE)
for(z in sourcefiles)source(z)

# color blind friendly
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


if(!dir.exists("output")) {
    dir.create("output")
}

if(!dir.exists("output/n_budget")) {
    dir.create("output/n_budget")
}

if(!dir.exists("output/mip")) {
    dir.create("output/mip")
}

