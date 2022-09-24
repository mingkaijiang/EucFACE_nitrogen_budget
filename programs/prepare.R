
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


if(!dir.exists("plots_tables")) {
    dir.create("plots_tables")
}

if(!dir.exists("plots_tables/checks")) {
    dir.create("plots_tables/checks")
}

#if(!dir.exists("plots_tables/covariate")) {
#    dir.create("plots_tables/covariate")
#}

if(!dir.exists("plots_tables/summary_tables")) {
    dir.create("plots_tables/summary_tables")
}

#if(!dir.exists("plots_tables/summary_tables/normalized")) {
#    dir.create("plots_tables/summary_tables/normalized")
#}

if(!dir.exists("plots_tables/summary_tables/unnormalized")) {
    dir.create("plots_tables/summary_tables/unnormalized")
}

if(!dir.exists("plots_tables/output")) {
    dir.create("plots_tables/output")
}

#if(!dir.exists("plots_tables/output/normalized")) {
#    dir.create("plots_tables/output/normalized")
#}

if(!dir.exists("plots_tables/output/unnormalized")) {
    dir.create("plots_tables/output/unnormalized")
}

