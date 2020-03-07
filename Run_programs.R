####################################################################################################################
##### ---------------------------------------------------------------------------------------------------------##### 
#####                            Master script for EucFACE Phosphorus Budget                                   #####
##### ---------------------------------------------------------------------------------------------------------##### 
####################################################################################################################
####
#### Written by: Mingkai Jiang (m.jiang@westernsydney.edu.au)
#### 
#### Code structure:
#### 1. Compute phosphorus concentrations for major pools and fluxes
#### 2. Compute biomass pools
#### 3. Generate P pools and fluxes
#### 4. Generate summary tables, based on unnormalized responses
#### 5. Normalize all responses
#### 6. Generate summary tables, figures based on normalized relationships
#### 7. Generate manuscript figures
#### 8. Generate N related pools and fluxes

##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 0: Prepare the repository (clean and read in necessary packages)
#### clear wk space
rm(list=ls(all=TRUE))

#### Source functions and packages
source("programs/prepare.R")

#### turn warnings off globally
# options(warn=-1)



##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 1: Generate P concentrations

#### 1.1: Soil P concentrations 
soil_p_concentration <- make_soil_p_concentration(func=mean)


#### 1.2: Soil phosphate conc, this returns % of P, not % of PO4!
#### Only top 10 cm!
soil_phosphate_concentration <- make_soil_phosphate_concentration(func=mean)

#### 1.3 Microbial P conc.
#### Only top 10 cm!
microbial_p_concentration <- make_microbial_p_concentration(func=mean)


#### 1.4 Canopy P conc.
canopy_p_concentration <- make_canopy_p_concentration(func=mean)

#canopy_p_concentration <- make_canopy_p_concentration_new(func=mean)
#compare_canopy_p_conc_datasets(inDF1=canopy_p_concentration_limited_data, inDF2=canopy_p_concentration)

#canopy_p_concentration <- make_canopy_p_concentration_fitted_with_age()



#### 1.5 Leaf litter P conc. 
leaflitter_p_concentration <- make_leaflitter_p_concentration(func=mean)


#leaflitter_p_concentration <- make_leaflitter_p_concentration_new(func=mean)


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


#source("programs/summary_variables/make_understorey_pool_size_comparison.R")
#make_understorey_pool_size_comparison(understorey_c_pool,
#                                      understorey_c_pool_2,
#                                      plotting = T)

### estimate biomass growth based on cover data
make_understorey_aboveground_growth_estimate(plotting = T)

### estimate % live and % dead
source("programs/summary_variables/unnormalized/make_understorey_percent_live_estimate.R")
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
##### Step 3: Generating P pools and fluxes
#### 3.1 Soil P pool
soil_p_pool <- make_soil_p_pool(p_conc=soil_p_concentration,
                                bk_density=soil_bulk_density)

#### 3.2 Soil phosphate pool
### This is additional to the microbial PO4-P pool
### The microbial pool needs to have one step further to break the cell when extracting
### So this pool is more readily available to plants. 
soil_phosphate_pool <- make_soil_phosphate_pool(p_conc=soil_phosphate_concentration,
                                                bk_density=soil_bulk_density)

#### 3.3 Soil P pool of different bioavailability
soil_p_pool_hedley <- make_soil_p_pool_hedley(p_conc=soil_hedley_p_concentration,
                                                            bk_density=soil_bulk_density)


#### 3.4 Soil P mineralization flux
#### It is assumed that the mineralization data is for top 10 cm only!
soil_p_mineralization <- make_soil_p_mineralization_flux(soil_bulk_density)

#### Soil P leaching rate
soil_p_leaching <- make_soil_p_leaching_flux()

#### 3.5 Microbial P pool 
#### Top 10 cm
microbial_p_pool <- make_microbial_p_pool(p_conc=microbial_p_concentration,
                                          bk_density=soil_bulk_density)

#### 3.6 Canopy P pool - only for green leaves
#canopy_p_pool <- make_canopy_p_pool(p_conc=canopy_p_concentration,
#                                    biom=canopy_biomass_pool)

canopy_p_pool <- make_canopy_p_pool_smoothed(biom=dLEAF_litter_flux)

### this is a new function yet to be written
leaflitter_p_pool <- make_leaflitter_p_pool(p_conc=leaflitter_p_concentration,
                                            c_pool=leaflitter_c_pool,
                                            c_frac=c_fraction)

#### 3.7 Canopy production flux
#canopy_p_flux <- make_canopy_p_production(p_conc=canopy_p_concentration,
#                                          c_flux=canopy_c_production_flux,
#                                          c_frac=c_fraction)

## considered both change in LAI and litterfall
canopy_p_flux <- make_canopy_p_production_new(c_flux=canopy_c_production_flux_new,
                                                  c_frac=c_fraction)


#### 3.8 Litter P production flux 
#### Literfall biomass (not C) will be calculated within the function
#### for data points where we have C but not P, we can create a separte script
#### and gap-fill P concentration based on average values
leaflitter_p_flux <- make_leaflitter_p_flux(p_conc=leaflitter_p_concentration)  
leaflitter_p_flux_gap_fill <- make_leaflitter_p_flux_gap_fill(p_conc=leaflitter_p_concentration) 


#### 3.9 Fine root litter P production
### assuming P retranslocation coefficient for fine root is 50%
### and fine root c production flux is fine root c litter flux
fineroot_litter_p_flux <- make_fineroot_litter_p_production(p_conc=fineroot_p_concentration,
                                                            c_flux=fineroot_c_production_flux,
                                                            p_retrans=0.5)

#### 3.10 Other litterfall
twig_litter_p_flux <- make_twiglitter_p_flux(p_conc=wood_p_concentration, litter_flux=twiglitter_c_production_flux)  
bark_litter_p_flux <- make_barklitter_p_flux(p_conc=wood_p_concentration, litter_flux=barklitter_c_production_flux)  
seed_litter_p_flux <- make_seedlitter_p_flux(p_conc=wood_p_concentration, litter_flux=seedlitter_c_production_flux)  

#### 3.11 Wood P pool   
sapwood_p_pool <- make_wood_p_pool(p_conc=wood_p_concentration,
                                c_pool=wood_c_pool,
                                case_consideration = "sapwood")


wood_p_pool <- make_wood_p_pool(p_conc=wood_p_concentration,
                                c_pool=wood_c_pool,
                                case_consideration = "total")


#### standing dead p pool
standing_dead_p_pool <- make_wood_p_pool(p_conc=wood_p_concentration,
                                         c_pool=standing_dead_c_pool,
                                         case_consideration = "sapwood")

#### 3.12 wood p flux
wood_p_flux <- make_wood_p_production(p_conc=wood_p_concentration,
                                      c_flux=wood_c_production)

#### Standing dead P flux
standing_dead_p_flux <- make_standing_dead_p_flux(p_conc=wood_p_concentration,
                                               c_flux=standing_dead_c_flux)

#### 3.13 Frass P production
#### Used C fraction for frass to convert c production back to frass biomass
#### We have more p conc than c flux so no need to gap fill. 
frass_c_fraction <- make_frass_c_fraction()
frass_p_production <- make_frass_p_production_flux(p_conc=frass_p_concentration,
                                                   c_flux=frass_c_production_flux,
                                                   c_frac=frass_c_fraction)

#### 3.14 Fine root P biomass pool
fineroot_p_pool <- make_fineroot_p_pool(p_conc=fineroot_p_concentration,
                                        c_pool=fineroot_c_pool)

#### 3.15 Fine root P production flux
fineroot_p_production <- make_fineroot_p_production(p_conc=fineroot_p_concentration,
                                                    c_flux=fineroot_c_production_flux)

#### 3.16 Understorey P pool, assume both species contributed equally
#### Also because p_conc and c_pool do not match in time,
#### we are taking the average of p_conc and apply it to c_pool
#### Here we can use Matthias's stereo camera estimate (2) or 
#### Varsha's harvest data (1) to extrapolate for p pool
#### It makes more sense to use harvest at this stage in time!
#### Also, if use Varsha's harvest data, we can use either total or live part of biomass
understorey_p_pool <- make_understorey_p_pool(p_conc=understorey_p_concentration,
                                              p_lit_conc=understorey_litter_p_concentration,
                                              c_pool=understorey_c_pool,
                                              c_frac=c_fraction_ud,
                                              live_or_total = "Total")

#### 3.17 Understorey production flux
#### Here we can use either stereo camera estimate of biomass (2) or
#### Harvest biomass data (1) to calculate p flux
#### Currently, we are using harvest estimate
understorey_p_flux <- make_understorey_p_flux(p_conc=understorey_p_concentration,
                                              c_flux=understorey_c_flux,
                                              c_frac=c_fraction_ud)

understorey_litter_p_flux <- make_understorey_litter_p_flux(p_conc=understorey_p_concentration,
                                              c_flux=understorey_litter_c_flux,
                                              c_frac=c_fraction_ud)

### 3.18 Coarse root P pool
coarse_root_p_pool <- make_coarse_root_p_pool(p_conc=wood_p_concentration,
                                                c_pool=coarse_root_c_pool,
                                                c_frac=c_fraction)


### 3.19 Coarse root P flux
coarse_root_p_flux <- make_coarse_root_p_flux(p_conc=wood_p_concentration,
                                                c_flux=coarse_root_c_flux,
                                                c_frac=c_fraction)


### 3.20 delta P pools
delta_soil_p_pool <- make_yearly_delta_pool_function(inDF=soil_p_pool, var.col=3)

delta_canopy_p_pool <- make_yearly_delta_pool_function(inDF=canopy_p_pool, var.col=3)

delta_wood_p_pool <- make_yearly_delta_pool_function(inDF=wood_p_pool, var.col=3)

delta_fineroot_p_pool <- make_yearly_delta_pool_function(inDF=fineroot_p_pool, var.col=3)

delta_coarse_root_p_pool <- make_yearly_delta_pool_function(inDF=coarse_root_p_pool, var.col=3)

delta_understorey_p_pool <- make_yearly_delta_pool_function(inDF=understorey_p_pool, var.col=3)

delta_microbial_p_pool <- make_yearly_delta_pool_function(inDF=microbial_p_pool, var.col=3)


##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 4: Making P budgeting variables and tables, based on raw data
#### 4.1 Summary Tables
source("programs/summary_tables/unnormalized/make_conc_summary_table_by_treatment.R")
summary_table_concentration_by_treatment <- make_conc_summary_table_by_treatment()

### P pools by treatment and ring
source("programs/summary_tables/unnormalized/make_pool_summary_table_by_treatment.R")
summary_table_pool_by_treatment <- make_pool_summary_table_by_treatment()

### P fluxes by treatment and ring
source("programs/summary_tables/unnormalized/make_flux_summary_table_by_treatment.R")
summary_table_flux_by_treatment <- make_flux_summary_table_by_treatment()

### C pools by treatment and ring
source("programs/summary_tables/unnormalized/make_c_pool_summary_table_by_treatment.R")
summary_table_c_pool_by_treatment <- make_c_pool_summary_table_by_treatment()

### C fluxes by treatment and ring
source("programs/summary_tables/unnormalized/make_c_flux_summary_table_by_treatment.R")
summary_table_c_flux_by_treatment <- make_c_flux_summary_table_by_treatment()



#### 4.2 Summary variables
### Calculate all P budgeting variables
source("programs/summary_variables/unnormalized/make_total_p_budgeting_variables.R")
summary_table_total_p_budgets <- make_total_p_budgeting_variables()

write.csv(summary_table_total_p_budgets, "plots_tables/summary_table_total_p_budgets_unnormalized.csv", row.names=F)

source("programs/summary_variables/unnormalized/make_overstorey_p_budgeting_variables.R")
summary_table_overstorey_p_budgets <- make_overstorey_p_budgeting_variables()

write.csv(summary_table_overstorey_p_budgets, "plots_tables/summary_table_overstorey_p_budgets_unnormalized.csv", row.names=F)


source("programs/summary_variables/unnormalized/make_understorey_p_budgeting_variables.R")
summary_table_understorey_p_budgets <- make_understorey_p_budgeting_variables()

write.csv(summary_table_understorey_p_budgets, "plots_tables/summary_table_understorey_p_budgets_unnormalized.csv", row.names=F)


source("programs/summary_variables/unnormalized/make_soil_p_budgeting_variables.R")
summary_table_soil_p_budgets <- make_soil_p_budgeting_variables()

write.csv(summary_table_soil_p_budgets, "plots_tables/summary_table_soil_p_budgets_unnormalized.csv", row.names=F)

#### 4.3 Generating CP ratios 
source("programs/summary_tables/unnormalized/make_cp_ratios.R")
summary_cp_ratios <- make_cp_ratios(c_pool=summary_table_c_pool_by_treatment,
                                    p_pool=summary_table_pool_by_treatment)

write.csv(summary_cp_ratios, "plots_tables/summary_cp_ratios.csv", row.names=F)


source("programs/check_variables/check_microbial_pool_CP_ratios.R")
summary_microbial_pool_comparison <- check_microbial_pool_CP_ratios(c_pool=summary_table_c_pool_by_treatment,
                                                                    p_pool=summary_table_pool_by_treatment)



##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 5: Normalize all responses to a pretreatment LAI

#### 5.1: Summary table for the stats 
#### All stats for fluxes are based on annual rate
### need to add canopy_litter_p_pool, and understorey_litter_p_pool
source("programs/stats/generate_stats_abs_covariate.R")
generate_stats_abs_covariate()


#### 5.2 re-calculate all variables based on linear mixed effect model 

######## Concentration
### Soil P conc
soil_p_concentration_pred <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_concentration, 
                                                                var.col=3,
                                                                return.outcome="predicted")

### Soil Phosphate conc
soil_phosphate_concentration_pred <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_phosphate_concentration, 
                                                                           var.col=3,
                                                                           return.outcome="predicted")

### hedley P concentrations
#soil_exchangeable_pi_concentration_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
#                                                                                     var.col=3,
#                                                                                     return.outcome="predicted")
#
#soil_exchangeable_po_concentration_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
#                                                                                     var.col=4,
#                                                                                     return.outcome="predicted")
#
#soil_mlabile_po_concentration_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
#                                                                                 var.col=5,
#                                                                                 return.outcome="predicted")
#
#soil_secondary_pi_concentration_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
#                                                                                   var.col=6,
#                                                                                   return.outcome="predicted")
#
#soil_primary_pi_concentration_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
#                                                                                 var.col=7,
#                                                                                 return.outcome="predicted")
#
#soil_occluded_p_concentration_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
#                                                                                 var.col=8,
#                                                                                 return.outcome="predicted")
#
#soil_aqua_p_concentration_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_hedley_p_concentration, 
#                                                                             var.col=9,
#                                                                             return.outcome="predicted")


### Overstorey Leaf P conc
canopy_p_concentration_pred <- make_leafp_conc_treatment_abs_effect_statistics(inDF=canopy_p_concentration, 
                                                                var.col=3,
                                                                return.outcome="predicted")

### Overstorey Leaf litter P conc
leaflitter_p_concentration_pred <- make_leafp_conc_treatment_abs_effect_statistics(inDF=leaflitter_p_concentration, 
                                                                    var.col=3,
                                                                    return.outcome="predicted")

### Wood P conc
### we do not have enough data to perform lmer for wood P concentration

### Fineroot P conc
fineroot_p_concentration_pred <- make_frootp_conc_treatment_abs_effect_statistics(inDF=fineroot_p_concentration, 
                                                               var.col=3,
                                                               return.outcome="predicted")

### Understorey aboveground P conc
understorey_p_concentration_pred <- make_uap_conc_treatment_abs_effect_statistics(inDF=understorey_p_concentration, 
                                                            var.col=3,
                                                            return.outcome="predicted")

### Understorey aboveground litter P conc
### not possible to construct a model due to limited data

### Microbial P conc
microbial_p_concentration_pred <- make_micp_conc_treatment_abs_effect_statistics(inDF=microbial_p_concentration, 
                                                              var.col=3,
                                                              return.outcome="predicted")


### Frass P concentration
frass_p_concentration_pred <- make_frassp_conc_treatment_abs_effect_statistics(inDF=frass_p_concentration, 
                                                                 var.col=3,
                                                                 return.outcome="predicted")


######## P fluxes and stocks
### Soil P pool
soil_p_pool_pred <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool, 
                                                                var.col=3,
                                                                return.outcome="predicted")

### Soil Phosphate pool
soil_phosphate_pool_pred <- make_soilp_conc_treatment_abs_effect_statistics(inDF=soil_phosphate_pool, 
                                                                           var.col=3,
                                                                           return.outcome="predicted")

### hedley P pools
#soil_exchangeable_pi_pool_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
#                                                                                                 var.col=3,
#                                                                                                 return.outcome="predicted")
#
#soil_exchangeable_po_pool_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
#                                                                                                 var.col=4,
#                                                                                                 return.outcome="predicted")
#
#soil_mlabile_po_pool_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
#                                                                                             var.col=5,
#                                                                                             return.outcome="predicted")
#
#soil_secondary_pi_pool_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
#                                                                                               var.col=6,
#                                                                                               return.outcome="predicted")
#
#soil_primary_pi_pool_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
#                                                                                             var.col=7,
#                                                                                             return.outcome="predicted")
#
#soil_occluded_p_pool_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
#                                                                                             var.col=8,
#                                                                                             return.outcome="predicted")
#
#soil_aqua_p_pool_pred <- make_hedley_soilp_conc_treatment_abs_effect_statistics(inDF=soil_p_pool_hedley, 
#                                                                                         var.col=9,
#                                                                                         return.outcome="predicted")

### Overstorey Leaf P pool
canopy_p_pool_pred <- make_leafp_conc_treatment_abs_effect_statistics(inDF=canopy_p_pool, 
                                                                var.col=3,
                                                                return.outcome="predicted")

canopy_litter_p_pool_pred <- make_leafp_conc_treatment_abs_effect_statistics(inDF=leaflitter_p_pool, 
                                                                      var.col=3,
                                                                      return.outcome="predicted")

### Wood P pool
wood_p_pool_pred <- make_woodp_pool_treatment_abs_effect_statistics(inDF=wood_p_pool, 
                                                                var.col=3,
                                                                return.outcome="predicted")

### Fineroot P pool
fineroot_p_pool_pred <- make_frootp_conc_treatment_abs_effect_statistics(inDF=fineroot_p_pool, 
                                                               var.col=3,
                                                               return.outcome="predicted")

### Understorey aboveground P pool
understorey_p_pool_pred <- make_uap_conc_treatment_abs_effect_statistics(inDF=understorey_p_pool, 
                                                            var.col=3,
                                                            return.outcome="predicted")


understorey_litter_p_pool_pred <- make_uap_conc_treatment_abs_effect_statistics(inDF=understorey_p_pool, 
                                                                         var.col=5,
                                                                         return.outcome="predicted")

### Microbial P pool
microbial_p_pool_pred <- make_micp_conc_treatment_abs_effect_statistics(inDF=microbial_p_pool, 
                                                              var.col=3,
                                                              return.outcome="predicted")


### coarse root P pool
coarse_root_p_pool_pred <- make_crootp_pool_treatment_abs_effect_statistics(inDF=coarse_root_p_pool, 
                                                                  var.col=3,
                                                                  return.outcome="predicted")

### Leaf litter P flux
leaflitter_p_flux_pred <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=leaflitter_p_flux, 
                                                                         var.col=5,
                                                                         return.outcome="predicted")

### twig litter flux
twig_litter_p_flux_pred <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=twig_litter_p_flux, 
                                                                         var.col=5,
                                                                         return.outcome="predicted")

### bark litter flux
bark_litter_p_flux_pred <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=bark_litter_p_flux, 
                                                                         var.col=5,
                                                                         return.outcome="predicted")

### seed litter flux
seed_litter_p_flux_pred <- make_leaf_lit_p_flux_treatment_abs_effect_statistics(inDF=seed_litter_p_flux, 
                                                                         var.col=5,
                                                                         return.outcome="predicted")


### Frass P flux
frass_p_production_pred <- make_frassp_conc_treatment_abs_effect_statistics(inDF=frass_p_production, 
                                                                 var.col=5,
                                                                 return.outcome="predicted")

### Canopy P production flux
### model failed to converge  --- check back!!!!!!
canopy_p_flux_pred <- make_canopy_p_flux_treatment_abs_effect_statistics(inDF=canopy_p_flux, 
                                                                      var.col=5,
                                                                      return.outcome="predicted")

### Wood production flux
wood_p_flux_pred <- make_wood_p_flux_treatment_abs_effect_statistics(inDF=wood_p_flux, 
                                                                  var.col=5,
                                                                  return.outcome="predicted") 

### Fineroot production flux
fineroot_p_production_pred <- make_froot_p_flux_treatment_abs_effect_statistics(inDF=fineroot_p_production, 
                                                                    var.col=5,
                                                                    return.outcome="predicted") 

### Coarseroot production
coarse_root_p_flux_pred <- make_croot_p_flux_treatment_abs_effect_statistics(inDF=coarse_root_p_flux, 
                                                                    var.col=5,
                                                                    return.outcome="predicted")

### Understorey aboveground production
understorey_p_flux_pred <- make_und_p_flux_treatment_abs_effect_statistics(inDF=understorey_p_flux, 
                                                                var.col=5,
                                                                return.outcome="predicted")

### Understory litter flux
understorey_litter_p_flux_pred <- make_und_lit_p_flux_treatment_abs_effect_statistics(inDF=understorey_litter_p_flux, 
                                                                        var.col=5,
                                                                        return.outcome="predicted")

### p mineralization flux
soil_p_mineralization_pred <- make_mineralization_p_flux_treatment_abs_effect_statistics(inDF=soil_p_mineralization, 
                                                                                      var.col=3,
                                                                                      return.outcome="predicted")

### p leaching flux
soil_p_leaching_pred <- make_p_leaching_flux_treatment_abs_effect_statistics(inDF=soil_p_leaching, 
                                                                                         var.col=5,
                                                                                         return.outcome="predicted")

### Delta Soil p
delta_soil_p_pool_pred <- make_yearly_delta_pool_function_pred(inDF=soil_p_pool_pred)

### Delta Leaf p
delta_canopy_p_pool_pred <- make_yearly_delta_pool_function_pred(inDF=canopy_p_pool_pred)

### Delta Wood C pool
delta_wood_p_pool_pred <- make_yearly_delta_pool_function_pred(inDF=wood_p_pool_pred)

### Delta Fineroot C pool
delta_fineroot_p_pool_pred <- make_yearly_delta_pool_function_pred(inDF=fineroot_p_pool_pred)

### Delta Coarseroot C pool
delta_coarse_root_p_pool_pred <- make_yearly_delta_pool_function_pred(inDF=coarse_root_p_pool_pred)

### Delta Understorey aboveground C pool
delta_understorey_p_pool_pred <- make_yearly_delta_pool_function_pred(inDF=understorey_p_pool_pred)

### Delta Microbial C pool
delta_microbial_p_pool_pred <- make_yearly_delta_pool_function_pred(inDF=microbial_p_pool_pred)




###### C pools and fluxes
### Frass production
frass_c_production_flux_pred <- make_frass_treatment_abs_effect_statistics(inDF=frass_c_production_flux, 
                                                                        var.col=5,
                                                                        return.outcome="predicted")


### Leaflitter flux
leaflitter_flux_pred <- make_litter_flux_treatment_abs_effect_statistics(inDF=leaflitter_c_production_flux, 
                                                                        var.col=3,
                                                                        return.outcome="predicted")  

### twig litter flux
twiglitter_flux_pred <- make_litter_flux_treatment_abs_effect_statistics(inDF=twiglitter_c_production_flux, 
                                                                        var.col=3,
                                                                        return.outcome="predicted")

### bark litter flux
barklitter_flux_pred <- make_litter_flux_treatment_abs_effect_statistics(inDF=barklitter_c_production_flux, 
                                                                        var.col=3,
                                                                        return.outcome="predicted") 

### Seed litter flux
seedlitter_flux_pred <- make_litter_flux_treatment_abs_effect_statistics(inDF=seedlitter_c_production_flux, 
                                                                        var.col=3,
                                                                        return.outcome="predicted")

### Wood production flux
wood_production_flux_pred <- make_wood_prod_treatment_abs_effect_statistics(inDF=wood_c_production, 
                                                                            var.col=5,
                                                                           return.outcome="predicted") 

### Fineroot production flux
fineroot_production_flux_pred <- make_froot_prod_treatment_abs_effect_statistics(inDF=fineroot_c_production_flux, 
                                                                                 var.col=5,
                                                                                return.outcome="predicted") 

### Coarseroot production
coarse_root_production_flux_pred <- make_croot_prod_treatment_abs_effect_statistics(inDF=coarse_root_c_flux, 
                                                                                    var.col=5,
                                                                                   return.outcome="predicted")

### Understorey aboveground production
understorey_aboveground_production_flux_pred <- make_und_prod_treatment_abs_effect_statistics(inDF=understorey_c_flux, 
                                                                                              var.col=5,
                                                                                             return.outcome="predicted")

### Soil C
soil_c_pool_pred <- make_soilc_treatment_abs_effect_statistics(inDF=soil_c_pool, 
                                                              var.col=3,
                                                              return.outcome="predicted")

### Leaf C
### This function takes a bit of time (1 min) to run!!!!!!!!!!
canopy_biomass_pool_pred <- make_leafc_treatment_abs_effect_statistics(inDF=canopy_biomass_pool, 
                                                              var.col=3,
                                                              return.outcome="predicted")

### Wood C pool
### model failed to converge !!!!!!!
wood_c_pool_pred <- make_woodc_treatment_abs_effect_statistics(inDF=wood_c_pool, 
                                                               var.col=3,
                                                               return.outcome="predicted") 

### Fineroot C pool
fineroot_c_pool_pred <- make_frootc_treatment_abs_effect_statistics(inDF=fineroot_c_pool, 
                                                                   var.col=3,
                                                                   return.outcome="predicted")

### Coarseroot C pool
coarse_root_c_pool_pred <- make_crootc_treatment_abs_effect_statistics(inDF=coarse_root_c_pool, 
                                                                       var.col=3,
                                                                      return.outcome="predicted")

### Understorey aboveground C pool
understorey_aboveground_c_pool_pred <- make_uac_treatment_abs_effect_statistics(inDF=understorey_c_pool, 
                                                                               var.col=5,
                                                                               return.outcome="predicted")

### Microbial C pool
microbial_c_pool_pred <- make_micc_treatment_abs_effect_statistics(inDF=microbial_c_pool, 
                                                                 var.col=3,
                                                                  return.outcome="predicted")

### Mycorrhizal C pool
mycorrhizal_c_pool_pred <- make_mycc_treatment_abs_effect_statistics(inDF=mycorrhizal_c_pool, 
                                                                    var.col=3,
                                                                    return.outcome="predicted")


##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 6: Making P budgeting variables and tables, based on normalized results

#### 6.1 Summary tables for concentrations, fluxes and pools
### P concentration by treatment and ring
source("programs/summary_tables/normalized/make_conc_summary_table_by_treatment_normalized.R")
summary_table_concentration_by_treatment_normalized <- make_conc_summary_table_by_treatment_normalized()

### P pools by treatment and ring
source("programs/summary_tables/normalized/make_pool_summary_table_by_treatment_normalized.R")
summary_table_pool_by_treatment_normalized <- make_pool_summary_table_by_treatment_normalized()

### P fluxes by treatment and ring
source("programs/summary_tables/normalized/make_flux_summary_table_by_treatment_normalized.R")
summary_table_flux_by_treatment_normalized <- make_flux_summary_table_by_treatment_normalized()

### C pools by treatment and ring
source("programs/summary_tables/normalized/make_c_pool_summary_table_by_treatment_normalized.R")
summary_table_c_pool_by_treatment_normalized <- make_c_pool_summary_table_by_treatment_normalized()

### C fluxes by treatment and ring
source("programs/summary_tables/normalized/make_c_flux_summary_table_by_treatment_normalized.R")
summary_table_c_flux_by_treatment_normalized <- make_c_flux_summary_table_by_treatment_normalized()



#### up to this point 12:00 03-04-2020

#### 6.2 summary variables for the budget
### Calculate all P budgeting variables
source("programs/summary_variables/normalized/make_total_p_budgeting_variables_normalized.R")
summary_table_total_p_budgets_normalized <- make_total_p_budgeting_variables_normalized()


source("programs/summary_variables/normalized/make_overstorey_p_budgeting_variables_normalized.R")
summary_table_overstorey_p_budgets_normalized <- make_overstorey_p_budgeting_variables_normalized()

source("programs/summary_variables/normalized/make_understorey_p_budgeting_variables_normalized.R")
summary_table_understorey_p_budgets_normalized <- make_understorey_p_budgeting_variables_normalized()

source("programs/summary_variables/normalized/make_soil_p_budgeting_variables_normalized.R")
summary_table_soil_p_budgets_normalized <- make_soil_p_budgeting_variables_normalized(sumDF=summary_table_pool_by_treatment_normalized)


#source("programs/summary_variables/normalized/make_belowground_p_standing_normalized.R")
#summary_table_belowground_p_standing_normalized <- make_belowground_p_standing_normalized()

#### 6.3 save output
write.csv(summary_table_concentration_by_treatment_normalized,
          "plots_tables/summary_table_concentration_by_treatment_normalized.csv")

write.csv(summary_table_pool_by_treatment_normalized,
          "plots_tables/summary_table_pool_by_treatment_normalized.csv")

write.csv(summary_table_flux_by_treatment_normalized,
          "plots_tables/summary_table_flux_by_treatment_normalized.csv")

write.csv(summary_table_c_pool_by_treatment_normalized,
          "plots_tables/summary_table_c_pool_by_treatment_normalized.csv")

write.csv(summary_table_c_flux_by_treatment_normalized,
          "plots_tables/summary_table_c_flux_by_treatment_normalized.csv")

write.csv(summary_table_total_p_budgets_normalized,
          "plots_tables/summary_table_total_p_budgets_normalized.csv")

### cp ratios
#source("programs/summary_tables/make_cp_ratios.R")
#summary_cp_ratios <- make_cp_ratios(c_pool=summary_table_c_pool_by_treatment_normalized,
#                                    p_pool=summary_table_pool_by_treatment_normalized)
#write.csv(summary_cp_ratios,
#          "plots_tables/summary_table_cp_ratios.csv")



##### ---------------------------------------------------------------------------------------------------------##### 
##### Step 7. Plotting P budget figures

### 7.1 combine summary tables
inDF <- rbind(summary_table_total_p_budgets_normalized,
              summary_table_overstorey_p_budgets_normalized,
              summary_table_understorey_p_budgets_normalized,
              summary_table_soil_p_budgets_normalized)


source("programs/plot_scripts/make_summary_p_budget_plots.R")
make_summary_p_budget_plots(inDF=inDF)

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





######## check later
### 7.2 the plots requested by Kristine
myDF <- rbind(summary_table_total_p_budgets_normalized,
              summary_table_overstorey_p_budgets_normalized,
              summary_table_understorey_p_budgets_normalized,
              summary_table_soil_p_budgets_normalized)

make_overall_summary_comparison_plot(myDF=myDF)


### 7.3 soil P as of hedley frac
### currently the occluded pool is an easy fix
make_summary_soil_p_budget_plots()









##### ---------------------------------------------------------------------------------------------------------##### 
########################### Step 8. Nitrogen variables
################# Concentrations
### Canopy N concentration
canopy_n_concentration <- make_canopy_n_concentration()

### Wood N concentration
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



################# Pools and fluxes
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

### coarseroot N pool
#coarseroot_n_pool <- make_coarseroot_n_pool()

### coarseroot N production
#coarseroot_n_production <- make_coarseroot_n_production()


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

### leaflitter N pool
#leaflitter_n_pool <- make_leaflitter_n_pool()

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







################## N:P ratios
### Canopy N:P ratio
canopy_np_ratio <- make_canopy_np_ratios(n_pool=canopy_n_pool,
                                         p_pool=canopy_p_pool)

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



####### make a output table to store all np ratios
make_summary_table_np_ratios()









###### ---------------- Data checking -------------------- ######
source("programs/check_variables/check_wood_data.R")
check_wood_data()






###### ---------------- End -------------------- ######
#### clear wk space
rm(list=ls(all=TRUE))
options(war=0)

