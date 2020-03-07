### Definitions of global constants


# Biomass is 50% carbon
c_fraction <- 0.5

# need to add fine root specific c fraction (from Juan)
# for now, use c_fraction value!!!
c_fraction_fr <- 0.5

# lerp C content (from Andrew)
c_fraction_lp <- 0.78

# understorey biomass C fraction
c_fraction_ud <- 0.5

# g to mg
g_to_mg <- 1000.0

# cm2 to m2
cm2_to_m2 <- 0.0001

# litter basket area m-2
frass_basket_area <- 0.1979

# ring diameter m
ring_diameter <- 25

# ring ground area m2
FACE_ring_area <- pi * (ring_diameter/2)^2

# Wood density (g cm-3), average of 10 entries in global wood density 
# database of Zanne et al. (SD = 0.1)
wood_density <- 0.827

# number of days in a month
ndays_in_month <- 30.0

#  g to kg
g_to_kg <- 0.001

# cm3 to m3
cm3_to_m3 <- 1e-6

# g/cm3 to ppm
g_per_cm3_to_ppm <- 1000000

# understorey biomass harvest strip area m2
strip_area <- 0.1

# understorey P retranslocation coefficient
retrans_ud <- 0.8

# water fraction
water_frac <- 0.5
