####################################################################################################################
##### ---------------------------------------------------------------------------------------------------------##### 
#####                            Master script for EucFACE nitrogen Budget                                   #####
##### ---------------------------------------------------------------------------------------------------------##### 
####################################################################################################################
####
#### Written by: Mingkai Jiang (m.jiang@westernsydney.edu.au)
#### 
#### Code structure:
#### 0. Preparing necessary scripts
#### 1. Compute nitrogen concentrations for major pools and fluxes
#### 2. Compute biomass pools
#### 3. Generate N pools and fluxes
#### 4. Generate P concentrations
#### 5. Generate N:P ratios
#### 6. Generate summary tables, based on unnormalized responses
#### 7. Make plots, based on unnormalized responses


##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 0: Prepare the repository (clean and read in necessary packages)
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("programs/prepare.R")

#### turn warnings off globally
# options(warn=-1)



##### ---------------------------------------------------------------------------------------------------------##### 
########################### Step 1. Nitrogen concentrations
### Canopy N concentration
canopy_n_concentration <- make_canopy_n_concentration()

### Wood N concentration
### we do not know if this is the sapwood or heartwood N concentration!
wood_n_concentration <- make_wood_n_concentration()

### Fineroot N concentration
fineroot_n_concentration <- make_fineroot_n_concentration()

### Frass N concentration
frass_n_concentration <- make_frass_n_concentration()

### Soil N concentration
soil_n_concentration <- make_soil_n_concentration()

### Soil inorganic N concentration
soil_inorganic_n_concentration <- make_soil_inorganic_n_concentration()

### Understorey N concentration
understorey_n_concentration <- make_understorey_n_concentration()

### microbial N concentration
microbial_n_concentration <- make_microbial_n_concentration()

### leaflitter N concentration
leaflitter_n_concentration <- make_leaflitter_n_concentration()

### understorey N concentration - no data available


##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 2: preparing C related variables
#### For all C pools, unit in g C m-2,
#### For all C fluxes, output rate in unit of mg C m-2 d-1, and the period over which this rate applies
#### Then assign the P concentration to C pools and fluxes. 
#### Note: % P of total dry biomass should not be directly applied to C result, 
#### as amount of C is not amount of dry weight !!!


#### 2.1 Canopy related variables (SLA, LAI, Canopy biomass)
lai_variable <- make_lai_variable()
lai_variable_smoothed <- make_smooth_lai_variable(timestep="1 day", kgam=15, return.option="dataframe")

sla_variable <- make_sla_variable()

canopy_biomass_pool <- make_canopy_biomass_pool(lai_variable, sla_variable, sla_option="variable")

canopy_biomass_pool_smoothed <- make_canopy_biomass_pool_smooth_lai(lai_variable_smoothed, sla_variable)

#### 2.2 Litter production (leaf, twig, bark, seed)
litter_c_production_flux <- make_litter_c_flux(c_fraction)

leaflitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "leaf_flux", "Start_date", "End_date", "Days")]
twiglitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "twig_flux", "Start_date", "End_date", "Days")]
barklitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "bark_flux", "Start_date", "End_date", "Days")]
seedlitter_c_production_flux <- litter_c_production_flux[,c("Date", "Ring", "seed_flux", "Start_date", "End_date", "Days")]

#### 2.3 Canopy C production
## assume it's the same as litterfall C production
canopy_c_production_flux <- leaflitter_c_production_flux

## based on change in leaf area and litterfall
## calculate change in leaf pool as well as litterfall
dLEAF_litter_flux <- make_dLAI_litter(litter=leaflitter_c_production_flux, sla_variable=sla_variable)

canopy_c_production_flux_new <- make_canopy_c_production_flux_new(inDF=dLEAF_litter_flux)

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
fineroot_c_pool <- make_fineroot_c_pool()

#### 2.7 Fineroot production
fineroot_c_production_flux <- make_fineroot_c_production_flux()

#### 2.8 Understorey aboveground biomass - 1: Varsha's clipping; 2: Matthias's stereo camera
understorey_c_pool <- make_understorey_aboveground_c_pool(c_fraction_ud,
                                                          strip_area)

understorey_c_pool_2 <- make_understorey_aboveground_c_pool_2(c_fraction_ud)

#### 2.9 Understorey production flux - 1: Varsha's clipping; 2: Matthias's stereo camera
understorey_c_flux <- make_understorey_aboveground_production_flux(c_fraction_ud)

understorey_c_flux_2 <- make_understorey_aboveground_production_flux_2(c_fraction_ud)

#### 2.10 understorey litter flux
understorey_litter_c_flux <- make_understorey_litter_flux(c_fraction_ud)


### estimate biomass growth based on cover data
make_understorey_aboveground_growth_estimate(plotting = T)

### estimate % live and % dead
understorey_live_percent <- make_understorey_percent_live_estimate()

#### 2.11 Frass production
frass_c_production_flux <- make_frass_c_production_flux()

#### 2.12 Soil C content
## Ring-specific bulk density
soil_bulk_density <- make_soil_bulk_density()

# return sum of all depths
soil_c_pool <- make_soil_c_pool(soil_bulk_density)

#### 2.13 Microbial C pool
# this pool has data only at 0-10cm depth - Cat's data
microbial_c_pool <- make_microbial_c_pool(soil_bulk_density)

### Yolima's data
#microbial_c_pool2 <- make_microbial_pool2(soil_bulk_density)

#### 2.14 Soil mycorrhizal production
mycorrhizal_c_pool <- make_mycorrhizal_c_pool(microbial_c_pool)

#### 2.15 Coarse root C pool 
coarse_root_c_pool <- make_coarse_root_pool(c_fraction, fr_pool=fineroot_c_pool) 

#### 2.16 Coarse root C production
coarse_root_c_flux <- make_coarse_root_production_flux(coarse_root_c_pool) 

#### 2.17 Leaf litter pool
leaflitter_c_pool <- make_leaflitter_pool(c_fraction)





##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 3: Multiple pools and fluxes with concentration
###### Nitrogen pools and fluxes
### Canopy N pool
canopy_n_pool <- make_canopy_n_pool(n_conc=canopy_n_concentration,
                                    biom=canopy_biomass_pool)

### Canopy N production flux
canopy_n_flux <- make_canopy_n_production(n_conc=canopy_n_concentration,
                                          c_flux=canopy_c_production_flux,
                                          c_frac=c_fraction)

### leaflitter N flux
leaflitter_n_flux <- make_leaflitter_n_flux(n_conc=leaflitter_n_concentration)

twig_litter_n_flux <- make_twiglitter_n_flux(n_conc=wood_n_concentration, litter_flux=twiglitter_c_production_flux)  
bark_litter_n_flux <- make_barklitter_n_flux(n_conc=wood_n_concentration, litter_flux=barklitter_c_production_flux)  
seed_litter_n_flux <- make_seedlitter_n_flux(n_conc=wood_n_concentration, litter_flux=seedlitter_c_production_flux)  


### wood N pool
wood_n_pool <- make_wood_n_pool(n_conc=wood_n_concentration,
                                c_pool=wood_c_pool,
                                case_consideration="total")

### wood N production
wood_n_production <- make_wood_n_production(n_conc=wood_n_concentration,
                                            c_flux=wood_c_production)

### fineroot N pool
fineroot_n_pool <- make_fineroot_n_pool(n_conc=fineroot_n_concentration,
                                        c_pool=fineroot_c_pool)


### fineroot N production
fineroot_n_production <- make_fineroot_n_production(n_conc=fineroot_n_concentration,
                                                    c_flux=fineroot_c_production_flux)

### fineroot litter N flux
### assuming N retranslocation coefficient for fine root is 50%
### and fine root c production flux is fine root c litter flux
fineroot_litter_n_flux <- make_fineroot_litter_n_flux(n_conc=fineroot_n_concentration,
                                                      c_flux=fineroot_c_production_flux,
                                                      n_retrans=0.5)


### coarseroot N pool
coarseroot_n_pool <- make_coarseroot_n_pool(n_conc=wood_n_concentration,
                                            c_pool=coarse_root_c_pool,
                                            case_consideration="total")

### coarseroot N production
coarseroot_n_production <- make_coarseroot_n_production(n_conc=wood_n_concentration,
                                                        c_flux=coarse_root_c_flux)


### Frass N production
frass_c_fraction <- make_frass_c_fraction()
frass_n_production <- make_frass_n_production_flux(n_conc=frass_n_concentration,
                                                   c_flux=frass_c_production_flux,
                                                   c_frac=frass_c_fraction)

### Understorey N pool
understorey_n_pool <- make_understorey_n_pool(n_conc=understorey_n_concentration,
                                              c_pool=understorey_c_pool,
                                              c_frac=c_fraction_ud,
                                              live_or_total = "Live")

### Understorey N flux
understorey_n_flux <- make_understorey_n_flux(n_conc=understorey_n_concentration,
                                              c_flux=understorey_c_flux,
                                              c_frac=c_fraction_ud)

### Understorey litter N flux
### no data
understorey_litter_n_flux <- make_understorey_litter_n_flux(n_conc=understorey_n_concentration,
                                                      c_flux=understorey_c_flux,
                                                      n_retrans=0.5)

### leaflitter N pool
leaflitter_n_pool <- make_leaflitter_n_pool(n_conc=leaflitter_n_concentration,
                                            c_pool=leaflitter_c_pool,
                                            c_frac=c_fraction)

### Soil N pool
soil_n_pool <- make_soil_n_pool(n_conc=soil_n_concentration,
                                bk_density=soil_bulk_density)

### Soil Inorganic N pool
## there are 3 possible data estimates:
## IEM method
## extractable method
## Lysimeter (shallow and deep depth)
## not sure which to use
soil_inorganic_n_pool <- make_soil_inorganic_n_pool(n_conc=soil_inorganic_n_concentration,
                                                    bk_density=soil_bulk_density)

### Soil nitrification flux
soil_nitrification_n_flux <- make_soil_n_nitrification_flux(bk_density=soil_bulk_density)

### Soil N mineralization flux
soil_mineralization_n_flux <- make_soil_n_mineralization_flux(bk_density=soil_bulk_density)

### microbial N pool
#### Top 10 cm
microbial_n_pool <- make_microbial_n_pool(n_conc=microbial_n_concentration,
                                          bk_density=soil_bulk_density)


### leaching flux
#soil_leaching_n_flux <- make_soil_leaching_n_flux()

### atmospheric N deposition flux
### get a literature value for this
#atmospheric_deposition_n_flux <- make_atmospheric_deposition_n_flux()


##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 4: Generate P concentrations
#### 1.1: Soil P concentrations 
soil_p_concentration <- make_soil_p_concentration(func=mean)


#### 1.2: Soil phosphate conc, this returns % of P, not % of PO4!
#### Only top 10 cm!
soil_phosphate_concentration <- make_soil_phosphate_concentration(func=mean)

#### 1.3 Microbial P conc.
#### Only top 10 cm!
microbial_p_concentration <- make_microbial_p_concentration()


#### 1.4 Canopy P conc.
canopy_p_concentration <- make_canopy_p_concentration(func=mean)

#### 1.5 Leaf litter P conc. 
leaflitter_p_concentration <- make_leaflitter_p_concentration(func=mean)


#### 1.6 Wood P conc. 
wood_p_concentration <- make_wood_p_concentration(func=mean)


#### 1.7 Frass P conc.
frass_p_concentration <- make_frass_p_concentration(func=mean)


#### 1.8 Fineroot P conc.
fineroot_p_concentration <- make_fineroot_p_concentration(func=mean)


#### 1.9 Understorey P conc.
understorey_p_concentration <- make_understorey_p_concentration(func=mean)

#### 1.10 Understorey litter P conc.
understorey_litter_p_concentration <- make_understorey_litter_p_concentration(func=mean)


#### 1.11 Understorey P retranslocation coefficient
understorey_p_retranslocation_coefficient <- make_understorey_p_retranslocation()

#### 1.12 Hedley fractionation dataset
soil_hedley_p_concentration <- make_soil_hedley_p_concentration(func=mean)

#### soil P pool and soil phosphate P pool
soil_p_pool <- make_soil_p_pool(p_conc=soil_p_concentration,
                                bk_density=soil_bulk_density)

soil_phosphate_pool <- make_soil_phosphate_pool(p_conc=soil_phosphate_concentration,
                                                bk_density=soil_bulk_density)





##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 5: Making NP ratios
### Canopy N:P ratio
canopy_np_ratio <- make_canopy_np_ratios(n_conc=canopy_n_concentration,
                                         p_conc=canopy_p_concentration)

### leaflitter N:P ratio
leaflitter_np_ratio <- make_leaflitter_np_ratios(n_conc=leaflitter_n_concentration,
                                                 p_conc=leaflitter_p_concentration)

### wood N:P ratio
wood_np_ratio <- make_wood_np_ratios(n_conc=wood_n_concentration,
                                     p_conc=wood_p_concentration)

### fineroot N:P ratio
fineroot_np_ratio <- make_fineroot_np_ratios(n_conc=fineroot_n_concentration,
                                             p_conc=fineroot_p_concentration)


### Frass N:P ratios
frass_np_ratio <- make_frass_np_ratios(n_conc=frass_n_concentration,
                                       p_conc=frass_p_concentration)

### Understorey N:P ratios
understorey_np_ratio <- make_understorey_np_ratios(n_conc=understorey_n_concentration,
                                                   p_conc=understorey_p_concentration)


### Soil N:P ratios
soil_np_ratio <- make_soil_np_ratios(n_pool=soil_n_pool,
                                     p_pool=soil_p_pool)

### readily available N:P ratio (i.e. sum of nitrate and ammonium : phosphate-P)
readily_available_soil_np_ratio <- make_readily_available_soil_np_ratios(n_pool=soil_inorganic_n_pool,
                                                                         p_pool=soil_phosphate_pool)


### microbes
microbial_np_ratio <- make_microbial_np_ratios(n_conc=microbial_n_concentration,
                                               p_conc=microbial_p_concentration)


##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 6: Making N budgeting variables and tables, based on raw data
#### 6.1 Summary Tables
source("programs/summary_tables/unnormalized/make_conc_summary_table.R")
summary_table_concentration <- make_conc_summary_table()

### N pools by treatment and ring
source("programs/summary_tables/unnormalized/make_pool_summary_table.R")
summary_table_pool <- make_pool_summary_table()
# why canopy N pool is small relative to canopy P pool (i.e. NP ratio = 10)


### N fluxes by treatment and ring
source("programs/summary_tables/unnormalized/make_flux_summary_table.R")
summary_table_flux <- make_flux_summary_table()

### C pools by treatment and ring
source("programs/summary_tables/unnormalized/make_c_pool_summary_table.R")
summary_table_c_pool <- make_c_pool_summary_table()

### C fluxes by treatment and ring
source("programs/summary_tables/unnormalized/make_c_flux_summary_table.R")
summary_table_c_flux <- make_c_flux_summary_table()

#### CN ratios 
summary_cn_ratios <- make_cn_ratios(c_pool=summary_table_c_pool,
                                    n_pool=summary_table_pool,
                                    c_flux=summary_table_c_flux,
                                    n_flux=summary_table_flux)

#### NP ratios
summary_np_ratios <- make_summary_table_np_ratios()




### 6.2 retranslocation coefficients
### canopy leaf n retranslocation coefficient
leaf_n_retrans_coefficient <- make_canopy_leaf_n_retranslocation_coefficient(df1=canopy_n_concentration,
                                                                             df2=leaflitter_n_concentration)

### understorey leaf n retranslocation coefficient
### assumed value
understorey_n_retrans_coefficient <- make_understorey_n_retrans_coefficient(retrans=0.5)

### fineroot retrans
### assumed value
fineroot_n_retrans_coefficient <- make_fineroot_n_retrans_coefficient(retrans=0.5)

### wood retrans
### assumed value
wood_n_retrans_coefficient <- make_stem_n_retrans_coefficient(retrans=0.5)

### coarseroot retrans
### assumed value
coarseroot_n_retrans_coefficient <- make_stem_n_retrans_coefficient(retrans=0.5)


### 6.3 some summary variables
### vegetation standing N stocks
vegetation_standing_n_stock <- make_vegetation_standing_n_stock(leaf=canopy_n_pool,
                                                                wood=wood_n_pool,
                                                                fineroot=fineroot_n_pool,
                                                                coarseroot=coarseroot_n_pool,
                                                                understorey=understorey_n_pool)

### total plant N requirement flux, retranslocation flux, and uptake flux
### for total retranslocation flux and uptake flux,
### we do not have heartwood data so ignores wood and coarseroot in this calculation!!!!!
total_plant_n_fluxes <- make_total_plant_n_fluxes(sumDF=summary_table_flux)


### N mean residence time in plant
plant_n_MRT <- make_plant_N_mean_residence_time(n_stand=vegetation_standing_n_stock,
                                                n_flux=total_plant_n_fluxes)

### Plant nitrogen use efficiency
plant_n_use_efficiency <- make_plant_N_use_efficiency(c_flux=summary_table_c_flux,
                                                      n_flux=total_plant_n_fluxes)


#### 6.3 N budget summary
### Calculate all N budgeting variables
total_n_budget <- make_total_n_budget()



##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 7. Make plots, based on unnormalized responses
### combine summary tables
#inDF <- rbind(summary_table_total_p_budgets_normalized,
#              summary_table_overstorey_p_budgets_normalized,
#              summary_table_understorey_p_budgets_normalized,
#              summary_table_soil_p_budgets_normalized)


source("programs/plot_scripts/make_n_budget_summary_plots.R")
make_summary_p_budget_plots(inDF=total_n_budget)

### This is based on unnormalized data!
source("programs/plot_scripts/make_summary_p_concentration_plots.R")
make_summary_p_concentration_plots(inDF=summary_table_concentration_by_treatment)


source("programs/plot_scripts/make_summary_p_pools_plots.R")
make_summary_p_pools_plots(inDF=summary_table_pool_by_treatment_normalized)


source("programs/plot_scripts/make_summary_p_fluxes_plots.R")
make_summary_p_fluxes_plots(inDF=summary_table_flux_by_treatment_normalized)


#### Individial rings
source("programs/plot_scripts/make_summary_p_budget_ring_plots.R")
make_summary_p_budget_ring_plots(inDF=summary_table_total_p_budgets_normalized)












###### ---------------- End -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))
options(war=0)

