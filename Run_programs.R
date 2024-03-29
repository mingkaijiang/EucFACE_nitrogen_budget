####################################################################################################################
##### ---------------------------------------------------------------------------------------------------------##### 
#####                    Master script for data-model intercomparison of the EucFACE nitrogen Budget           #####
##### ---------------------------------------------------------------------------------------------------------##### 
####################################################################################################################
####
#### Written by: Mingkai Jiang (m.jiang@westernsydney.edu.au)
#### 
#### Code structure:
#### A1. Preparing necessary scripts
#### B1. Compute nitrogen concentrations for major pools and fluxes
#### B2. Compute retranslocation coefficients
#### B3. Compute biomass pools
#### B4. Generate N pools and fluxes
#### B5. Import P concentrations
#### B6. Generate summary tables, based on unnormalized responses
#### C1. Import MIP output


#### B7. Make plots, based on unnormalized responses
#### D1. Merge data and model for comparison


##### ---------------------------------------------------------------------------------------------------------##### 
##### Step A: Prepare the repository (clean and read in necessary packages)
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("programs/prepare.R")

#### turn warnings off globally
# options(warn=-1)



##### ---------------------------------------------------------------------------------------------------------##### 
########################### Step B1. Nitrogen concentrations

### soil bulk density - updated with 0-10, 10-30, transition
soil_bulk_density <- make_soil_bulk_density()


### Canopy N concentration
canopy_n_concentration <- make_canopy_n_concentration()

### Wood N concentration
### we do not know if this is the sapwood or heartwood N concentration!
wood_n_concentration <- make_wood_n_concentration()

### Fineroot N concentration, 0-10, 10-30
fineroot_n_concentration <- make_fineroot_n_concentration()

### Frass N concentration
frass_n_concentration <- make_frass_n_concentration()

### Soil N concentration 0-10, 10-30
soil_n_concentration <- make_soil_n_concentration()

### Soil inorganic N concentration, 0-10, in NH4 and NO3
soil_inorganic_n_concentration <- make_soil_inorganic_n_concentration()

### Understorey N concentration
understorey_n_concentration <- make_understorey_n_concentration()

### microbial N concentration 0 - 10cm
microbial_n_concentration <- make_microbial_n_concentration()

### leaflitter N concentration
leaflitter_n_concentration <- make_leaflitter_n_concentration()

### understorey N concentration - no data available



##### ---------------------------------------------------------------------------------------------------------##### 
########################### Step B2. Retranslocation coefficients
### retranslocation coefficients
leaf_n_retrans_coefficient <- make_canopy_leaf_n_retranslocation_coefficient(df1=canopy_n_concentration,
                                                                             df2=leaflitter_n_concentration)

### understorey leaf n retranslocation coefficient
### assumed value
understorey_n_retrans_coefficient <- make_understorey_n_retrans_coefficient(retrans=0.3)

### fineroot retrans
### assumed value
fineroot_n_retrans_coefficient <- make_fineroot_n_retrans_coefficient(retrans=0.3)

### wood retrans
### assumed value
wood_n_retrans_coefficient <- make_stem_n_retrans_coefficient(retrans=0.3)

### coarseroot retrans
### assumed value
coarseroot_n_retrans_coefficient <- make_stem_n_retrans_coefficient(retrans=0.3)



##### ---------------------------------------------------------------------------------------------------------##### 
##### Step B3: preparing C related variables
#### For all C pools, unit in g C m-2,
#### For all C fluxes, output rate in unit of mg C m-2 d-1, and the period over which this rate applies


### need to update all scripts following P budget example, check with 周展安


lai_variable <- make_lai_variable()

sla_variable <- make_sla_variable()


#### 2.1 Canopy related variables (SLA, LAI, Canopy biomass)
canopy_c_pool <- make_canopy_c_pool(lai_variable, sla_variable, sla_option="variable")

#### 2.2 Litter production (leaf, twig, bark, seed)
litter_c_production_flux <- make_litter_c_flux(c_fraction)

leaflitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "leaf_flux", "Start_date", "End_date", "Days")]
twiglitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "twig_flux", "Start_date", "End_date", "Days")]
barklitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "bark_flux", "Start_date", "End_date", "Days")]
seedlitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "seed_flux", "Start_date", "End_date", "Days")]

### herbivore leaf c consumption flux
### extrapolated based on frass weight, leaf area consumed and sla data
frass_c_production_flux <- make_frass_c_production_flux()

herbivory_leaf_consumption_flux <- make_herbivory_leaf_consumption_flux(sla=sla_variable, 
                                                                        frass_flux=frass_c_production_flux)


canopy_c_production_flux <- merge_litter_c_and_herbivory_loss(litter=leaflitter_c_production_flux,
                                                              herbivory=herbivory_leaf_consumption_flux)


#### 2.4 Wood C pool
# year 2011-12 data on local directory
wood_c_pool <- make_wood_c_pool(ring_area=FACE_ring_area,
                                c_frac=c_fraction)

# wood c without excluding mortality
# mortality is not a big issue here as only one tree was marked dead!
# missing data? Tree shrinking? Measurement error?
#wood_c_pool_total <- make_wood_c_pool_total(ring_area=FACE_ring_area,
#                                            c_frac=c_fraction)

## standing dead wood c pool
standing_dead_c_pool <- make_standing_dead_c_pool(ring_area=FACE_ring_area,
                                                  c_frac=c_fraction)

#### 2.5 Wood C production
wood_c_production <- make_wood_production_flux(wood_c_pool)

## standing dead wood c flux
standing_dead_c_flux <- make_standing_dead_c_flux(standing_dead_c_pool)

#### 2.6 Fineroot pool
### include intermediate root size
fineroot_c_pool <- make_fineroot_c_pool(back.calculate=T,
                                        soil_bulk_density=soil_bulk_density,
                                        root.size="intermediate")

#### 2.7 Fineroot production
fineroot_c_production_flux <- make_fineroot_c_production_flux()

#### 2.8 Understorey aboveground biomass - 1: Varsha's clipping; 2: Matthias's stereo camera
#understorey_c_pool <- make_understorey_aboveground_c_pool(c_fraction_ud,
#                                                          strip_area)

understorey_c_pool <- make_understorey_aboveground_c_pool_camera(c_frac=c_fraction_ud,
                                                                 plot.option=T)

#### 2.9 Understorey production flux - 1: Varsha's clipping; 2: Matthias's stereo camera
understorey_c_flux <- make_understorey_aboveground_production_flux(c_fraction_ud)

#understorey_c_flux_2 <- make_understorey_aboveground_production_flux_2(c_fraction_ud)

#### 2.10 understorey litter flux
understorey_litter_c_flux <- make_understorey_litter_flux(c_fraction_ud)


#### 2.12 Soil C content


# return sum of all depths
soil_c_pool <- make_soil_c_pool(bk_density=soil_bulk_density)


#### 2.13 Microbial C pool
microbial_c_pool <- make_microbial_c_pool(soil_bulk_density)


#### 2.15 Coarse root C pool 
coarse_root_c_pool <- make_coarse_root_pool(c_fraction, fr_pool=fineroot_c_pool) 

#### 2.16 Coarse root C production
coarse_root_c_flux <- make_coarse_root_production_flux(coarse_root_c_pool) 

#### 2.17 Leaf litter pool
leaflitter_c_pool <- make_leaflitter_pool(c_fraction)





##### ---------------------------------------------------------------------------------------------------------##### 
##### Step B4: Multiple pools and fluxes with concentration
#### Note: % N of total dry biomass should NOT be directly applied to C result, 
#### as the amount of C is not the amount of dry weight !!!

###### Nitrogen pools and fluxes
### Canopy N pool
canopy_n_pool <- make_canopy_n_pool(n_conc=canopy_n_concentration,
                                    biom=canopy_c_pool,
                                    c_frac=c_fraction)


### wood N pool
wood_n_pool <- make_wood_n_pool(n_conc=wood_n_concentration,
                                c_pool=wood_c_pool,
                                n_retrans=wood_n_retrans_coefficient)


### fineroot N pool， 0-10 and 10-30 and transition
fineroot_n_pool <- make_fineroot_n_pool(n_conc=fineroot_n_concentration,
                                        c_pool=fineroot_c_pool)

### coarseroot N pool
coarseroot_n_pool <- make_coarseroot_n_pool(n_conc=wood_n_concentration,
                                            c_pool=coarse_root_c_pool,
                                            n_retrans=wood_n_retrans_coefficient)

### Understorey N pool
understorey_n_pool <- make_understorey_n_pool(n_conc=understorey_n_concentration,
                                              c_pool=understorey_c_pool,
                                              c_frac=c_fraction_ud,
                                              live_or_total = "Live")


### leaflitter N pool
leaflitter_n_pool <- make_leaflitter_n_pool(n_conc=leaflitter_n_concentration,
                                            c_pool=leaflitter_c_pool,
                                            c_frac=c_fraction)

### Soil N pool 0 - 30 cm
soil_n_pool <- make_soil_n_pool(n_conc=soil_n_concentration,
                                bk_density=soil_bulk_density)

### Soil Inorganic N pool 0 - 10 cm
## there are 3 possible data estimates:
## IEM method
## extractable method
## Lysimeter (shallow and deep depth)
## not sure which to use
soil_inorganic_n_pool <- make_soil_inorganic_n_pool(n_conc=soil_inorganic_n_concentration,
                                                    bk_density=soil_bulk_density)


### microbial N pool Top 10 cm
microbial_n_pool <- make_microbial_n_pool(n_conc=microbial_n_concentration,
                                          bk_density=soil_bulk_density)





### Canopy N production flux
canopy_n_flux <- make_canopy_n_production(n_conc=canopy_n_concentration,
                                          c_flux=canopy_c_production_flux,
                                          c_frac=c_fraction)

### leaflitter N flux
leaflitter_n_flux <- make_leaflitter_n_flux(n_conc=leaflitter_n_concentration,
                                            c_flux=leaflitter_c_production_flux,
                                            c_frac=c_fraction)

twig_litter_n_flux <- make_twiglitter_n_flux(n_conc=wood_n_concentration, 
                                             litter_flux=twiglitter_c_production_flux)  

bark_litter_n_flux <- make_barklitter_n_flux(n_conc=wood_n_concentration, 
                                             litter_flux=barklitter_c_production_flux)  

seed_litter_n_flux <- make_seedlitter_n_flux(n_conc=wood_n_concentration, 
                                             litter_flux=seedlitter_c_production_flux)  


canopy_n_retranslocation_flux <- calculate_canopy_n_retranslocation_flux(tflux=canopy_n_flux,
                                                                         lflux=leaflitter_n_flux,
                                                                         retransDF=leaf_n_retrans_coefficient)

### wood N production
wood_n_production <- make_wood_n_production(n_conc=wood_n_concentration,
                                            c_flux=wood_c_production)


sapwood_n_retranslocation_flux <- calculate_sapwood_n_retranslocation_flux(tflux=wood_n_production,
                                                                           retransDF=wood_n_retrans_coefficient)


### fineroot N production
fineroot_n_production <- make_fineroot_n_production(n_conc=fineroot_n_concentration,
                                                    c_flux=fineroot_c_production_flux)


### fineroot litter N flux
### and fine root c production flux is fine root c litter flux
fineroot_litter_n_flux <- make_fineroot_litter_n_flux(n_conc=fineroot_n_concentration,
                                                      c_flux=fineroot_c_production_flux,
                                                      n_retrans=fineroot_n_retrans_coefficient)


fineroot_n_retranslocation_flux <- make_fineroot_retranslocation_n_flux(df1=fineroot_n_production,
                                                                        df2=fineroot_litter_n_flux)


### coarseroot N production
coarseroot_n_production <- make_coarseroot_n_production(n_conc=wood_n_concentration,
                                                        c_flux=coarse_root_c_flux,
                                                        n_retrans=wood_n_retrans_coefficient)


### Frass N production
frass_c_fraction <- make_frass_c_fraction()
frass_n_production <- make_frass_n_production_flux(n_conc=frass_n_concentration,
                                                   c_flux=frass_c_production_flux,
                                                   c_frac=frass_c_fraction)


### Understorey N flux
understorey_n_flux <- make_understorey_n_flux(n_conc=understorey_n_concentration,
                                              c_flux=understorey_c_flux,
                                              c_frac=c_fraction_ud)



understorey_litter_n_flux <- make_understorey_litter_n_flux(n_conc=understorey_n_concentration,
                                                            c_flux=understorey_c_flux,
                                                            n_retrans=understorey_n_retrans_coefficient)


understorey_n_retranslocation_flux <- make_understorey_retranslocation_n_flux(df1=understorey_n_flux,
                                                                              df2=understorey_litter_n_flux)



### Soil nitrification flux 0 - 10 cm
soil_nitrification_n_flux <- make_soil_n_nitrification_flux(bk_density=soil_bulk_density)

### Soil N mineralization flux 0 - 10 cm
soil_mineralization_n_flux <- make_soil_n_mineralization_flux(bk_density=soil_bulk_density)


### leaching flux
soil_leaching_n_flux <- make_soil_leaching_n_flux()

### atmospheric N deposition flux
atmospheric_deposition_n_flux <- make_atmospheric_deposition_n_flux()




##### ---------------------------------------------------------------------------------------------------------##### 
##### Step B5: Import P-related variables -- yet to be finalized
import_P_budget_output()


##### ---------------------------------------------------------------------------------------------------------##### 
##### Step B6: Making N budgeting variables and tables, based on raw data
#### 6.1 Summary Tables
summary_table_concentration <- make_conc_summary_table(canopy_n_concentration=canopy_n_concentration,
                                                       wood_n_concentration=wood_n_concentration,
                                                       fineroot_n_concentration=fineroot_n_concentration,
                                                       understorey_n_concentration=understorey_n_concentration,
                                                       frass_n_concentration=frass_n_concentration,
                                                       leaflitter_n_concentration=leaflitter_n_concentration,
                                                       microbial_n_concentration=microbial_n_concentration,
                                                       soil_n_concentration=soil_n_concentration,
                                                       soil_inorganic_n_concentration=soil_inorganic_n_concentration)


### N pools by treatment and ring
summary_table_pool <- make_pool_summary_table(canopy_n_pool=canopy_n_pool,
                                              wood_n_pool=wood_n_pool,
                                              fineroot_n_pool=fineroot_n_pool,
                                              coarseroot_n_pool=coarseroot_n_pool,
                                              leaflitter_n_pool=leaflitter_n_pool,
                                              understorey_n_pool=understorey_n_pool,
                                              soil_n_pool=soil_n_pool,
                                              soil_inorganic_n_pool=soil_inorganic_n_pool,
                                              microbial_n_pool=microbial_n_pool)


### N fluxes by treatment and ring
summary_table_flux <- make_flux_summary_table(soil_mineralization_n_flux=soil_mineralization_n_flux,
                                              soil_nitrification_n_flux=soil_nitrification_n_flux,
                                              canopy_n_flux=canopy_n_flux,
                                              wood_n_production=wood_n_production,
                                              fineroot_n_production=fineroot_n_production,
                                              coarseroot_n_production=coarseroot_n_production,
                                              canopy_n_retranslocation_flux=canopy_n_retranslocation_flux,
                                              sapwood_n_retranslocation_flux=sapwood_n_retranslocation_flux,
                                              fineroot_n_retranslocation_flux=fineroot_n_retranslocation_flux,
                                              understorey_n_retranslocation_flux=understorey_n_retranslocation_flux,
                                              leaflitter_n_flux=leaflitter_n_flux,
                                              bark_litter_n_flux=bark_litter_n_flux,
                                              seed_litter_n_flux=seed_litter_n_flux,
                                              twig_litter_n_flux=twig_litter_n_flux,
                                              fineroot_litter_n_flux=fineroot_litter_n_flux,
                                              atmospheric_n_deposition=atmospheric_deposition_n_flux,
                                              leaching_n_flux=soil_leaching_n_flux,
                                              understorey_n_flux=understorey_n_flux,
                                              understorey_litter_n_flux=understorey_litter_n_flux)



### C pools by treatment and ring
summary_table_c_pool <- make_c_pool_summary_table(norm="unnormalized",
                                                  canopy_c_pool=canopy_c_pool,
                                                  wood_c_pool=wood_c_pool,
                                                  fineroot_c_pool=fineroot_c_pool,
                                                  coarse_root_c_pool=coarse_root_c_pool,
                                                  understorey_c_pool=understorey_c_pool,
                                                  soil_c_pool=soil_c_pool,
                                                  microbial_c_pool=microbial_c_pool,
                                                  leaflitter_c_pool=leaflitter_c_pool)

### C fluxes by treatment and ring
summary_table_c_flux <- make_c_flux_summary_table(norm="unnormalized",
                                                  leaflitter_c_production_flux=leaflitter_c_production_flux,
                                                  twiglitter_c_production_flux=twiglitter_c_production_flux,
                                                  barklitter_c_production_flux=barklitter_c_production_flux,
                                                  seedlitter_c_production_flux=seedlitter_c_production_flux,
                                                  canopy_c_production_flux=canopy_c_production_flux,
                                                  wood_c_production=wood_c_production,
                                                  fineroot_c_production_flux=fineroot_c_production_flux,
                                                  coarse_root_c_flux=coarse_root_c_flux,
                                                  understorey_c_flux=understorey_c_flux,
                                                  frass_c_production_flux=frass_c_production_flux)


#### CN ratios 
summary_cn_ratios <- make_cn_ratios(c_pool=summary_table_c_pool,
                                    n_pool=summary_table_pool,
                                    c_flux=summary_table_c_flux,
                                    n_flux=summary_table_flux)


#### NP ratios
summary_np_ratios <- make_summary_table_np_ratios(n_conc=summary_table_concentration,
                                                  n_flux=summary_table_flux,
                                                  n_pool=summary_table_pool)



### Some summary variables
### vegetation standing N stocks
vegetation_standing_n_stock <- make_vegetation_standing_n_stock(leaf=canopy_n_pool,
                                                                wood=wood_n_pool,
                                                                fineroot=fineroot_n_pool,
                                                                coarseroot=coarseroot_n_pool,
                                                                understorey=understorey_n_pool)

### total plant N requirement flux, retranslocation flux, and uptake flux
### for total retranslocation flux and uptake flux,
total_plant_n_fluxes <- make_total_plant_n_fluxes(sumDF=summary_table_flux)


### N mean residence time in plant
plant_n_MRT <- make_plant_N_mean_residence_time(n_stand=vegetation_standing_n_stock,
                                                n_flux=total_plant_n_fluxes)

### Plant nitrogen use efficiency
plant_n_use_efficiency <- make_plant_N_use_efficiency(c_flux=summary_table_c_flux,
                                                      n_flux=total_plant_n_fluxes)


#### N budget summary
### Calculate all N budgeting variables
total_n_budget <- make_total_n_budget()


##### ---------------------------------------------------------------------------------------------------------##### 
##### Step C1. import model output

### prepare model defintions etc.
source("programs/prepare_model.R")


### import model output
import_model_output()


### read in MIP output over observed period (var scenario)
### go into function to plot
scenario="VAR"
nconDF=summary_table_concentration
nfluxDF=summary_table_flux
npoolDF=summary_table_pool
cpoolDF=summary_table_c_pool
cfluxDF=summary_table_c_flux
nbudgetDF=total_n_budget
cnDF=summary_cn_ratios
npDF=summary_np_ratios
make_time_averaged_data_model_comparison_over_obs_period(nconDF=summary_table_concentration,
                                                         nfluxDF=summary_table_flux,
                                                         npoolDF=summary_table_pool,
                                                         cpoolDF=summary_table_c_pool,
                                                         cfluxDF=summary_table_c_flux,
                                                         nbudgetDF=total_n_budget,
                                                         cnDF=summary_cn_ratios,
                                                         npDF=summary_np_ratios,
                                                         scenario="VAR")











##### ---------------------------------------------------------------------------------------------------------##### 
##### Step B7. Make plots, based on unnormalized responses
### for all these plotting scripts, need to open the function, then make plots within the function

source("programs/plot_scripts/make_n_budget_summary_plots.R")
inDF=total_n_budget
make_n_budget_summary_plots(inDF=total_n_budget)

source("programs/plot_scripts/make_n_concentration_summary_plots.R")
inDF=summary_table_concentration
make_n_concentration_summary_plots(inDF=summary_table_concentration)


source("programs/plot_scripts/make_n_pools_summary_plots.R")
inDF=summary_table_pool
make_n_pools_summary_plots(inDF=summary_table_pool)


source("programs/plot_scripts/make_n_fluxes_summary_plots.R")
inDF=summary_table_flux
make_n_fluxes_summary_plots(inDF=summary_table_flux)













###### ---------------- End -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))
options(war=0) 

