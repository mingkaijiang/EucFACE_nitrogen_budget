#### color palette:
model.names <- c("A_GDAYP",
                 "B_ELMV1",
                 "C_CABLP",
                 "D_LPJGP",
                 "E_OCHDP",
                 "F_QUINC",
                 "G_OCHDX",
                 "H_QUJSM",
                 "I_GDAYN",
                 "J_LPJGN")


obs.color <- c("#000000") # black

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

set3Palette <- brewer.pal(n = 10, name = "Set3")

YlOrRdPalette <- rev(brewer.pal(n = 9, name = "YlOrRd"))

GreensPalette <- rev(brewer.pal(n = 9, name = "Greens"))

SpectralPalette <- brewer.pal(n = 8, name = "Spectral")

Diverge_hsv_Palette <- colorspace::diverge_hcl(8)


col.values <- c("A_GDAYP" = SpectralPalette[1],
                "B_ELMV1" = SpectralPalette[2],
                "C_CABLP" = SpectralPalette[3],
                "D_LPJGP" = SpectralPalette[4],
                "E_OCHDP" = SpectralPalette[5],
                "F_QUINC" = SpectralPalette[6],
                "G_OCHDX" = SpectralPalette[7],
                "H_QUJSM" = SpectralPalette[8],
                "I_GDAYN" = SpectralPalette[1],
                "J_LPJGN" = SpectralPalette[4])


linetype.values <- c("A_GDAYP" = 1,
                     "B_ELMV1" = 1,
                     "C_CABLP" = 1,
                     "D_LPJGP" = 1,
                     "E_OCHDP" = 1,
                     "F_QUINC" = 1,
                     "G_OCHDX" = 1,
                     "H_QUJSM" = 1,
                     "I_GDAYN" = 2,
                     "J_LPJGN" = 2)


model.labels <- c("A_GDAYP" = "GDAYP",
                  "B_ELMV1" = "ELMV1",
                  "C_CABLP" = "CABLP",
                  "D_LPJGP" = "LPJGP",
                  "E_OCHDP" = "OCDHP",
                  "F_QUINC" = "QUINC",
                  "G_OCHDX" = "OCHDX",
                  "H_QUJSM" = "QUJSM",
                  "I_GDAYN" = "GDAYN",
                  "J_LPJGN" = "LPJGN")


#### 2.1. Model list
## CNP model
p.mod.list <- c("CABLP", "ELMV1", 
                "GDAYP", "LPJGP",
                "OCHDP", "OCHDX", 
                "QUINC", "QUJSM")

## CN model
n.mod.list <- c("GDAYN", "LPJGN")





scenario="VAR"
make_time_averaged_data_model_comparison_over_obs_period(eucDF,
                                                         scenario="VAR")


