make_total_n_budget <- function() {
    #### This function calculates all N budgeting variables
    
    ### standing N stock
    ### summarize according to year - this ignores bark and twigs
    source("programs/summary_variables/unnormalized/make_overstorey_standing_n_stock.R")
    overstorey_standing_n_stock <- make_overstorey_standing_p_stock(leaf=canopy_n_pool, 
                                                                    wood=wood_n_pool)
    
    overstorey_standing_n_stock_avg <- summaryBy(total~Ring, data=overstorey_standing_p_stock, 
                                                 FUN=mean, na.rm=T, keep.names=T)
    
    
    source("programs/summary_variables/unnormalized/make_understorey_standing_n_stock.R")
    understorey_standing_n_stock <- make_understorey_standing_p_stock(abg=understorey_n_pool)
    
    source("programs/summary_variables/unnormalized/make_belowground_standing_n_stock.R")
    belowground_standing_n_stock <- make_belowground_standing_n_stock(croot=coarse_root_n_pool, 
                                                                      froot=fineroot_n_pool)
    
    
    belowground_standing_n_stock_avg <- summaryBy(total~Ring, data=belowground_standing_n_stock, 
                                                  FUN=mean, na.rm=T, keep.names=T)
    
    
    ### total n stock
    total_standing_n_stock <- overstorey_standing_n_stock_avg$total + understorey_standing_n_stock$understorey_n_pool +
        belowground_standing_n_stock_avg$total
    
    ### P requirements, i.e. using plant P fluxes 
    source("programs/summary_variables/make_total_p_requirement.R")
    total_p_requirement_table <- make_total_p_requirement_table(summary_table_flux_by_treatment)
    
    ### total P retranslocation, i.e. canopy P - litterfall P + wood P increment + fineroot P - fineroot litter P
    source("programs/summary_variables/make_total_p_retranslocation.R")
    total_p_retranslocation <- make_total_p_retranslocation(under_retrans_calc_method = "Simple", 
                                                            leaf_p_retrans_coefficient=leaf_p_retrans_coefficient,
                                                            understorey_retrans_coef=understorey_p_retranslocation_coefficient,
                                                            sumDF=summary_table_flux_by_treatment)
    
    ### P uptake from soil, i.e. P requirement - P retranslocation
    source("programs/summary_variables/make_p_uptake_from_soil.R")
    total_p_uptake_from_soil <- make_p_uptake_from_soil(p_req=total_p_requirement_table,
                                                        p_retrans=total_p_retranslocation)
    
    
    ### Uptake/requirement
    source("programs/summary_variables/make_p_uptake_over_requirement.R")
    p_uptake_over_requirement <- make_p_uptake_over_requirement(p_up=total_p_uptake_from_soil,
                                                                p_req=total_p_requirement_table)
    
    ### MRT, i.e. Standing P / Uptake
    source("programs/summary_variables/make_p_MRT.R")
    P_mean_residence_time <- make_p_MRT(p_stand=total_standing_p_stock,
                                        p_up=total_p_uptake_from_soil)
    
    ### Standing PUE, i.e. NPP / P Uptake
    source("programs/summary_variables/make_standing_pue.R")
    standing_pue <- make_standing_pue(p_up=total_p_uptake_from_soil)
    
    p_mineralization <- summaryBy(p_mineralization_mg_m2_d~Ring, FUN=mean, keep.names=T, data=soil_p_mineralization)
    p_mineralization$p_mineralization <- with(p_mineralization, p_mineralization_mg_m2_d * 365/1000)
    
    ### out df
    terms <- c("total standing p stock", 
               "total p requirement", 
               "total p retranslocated", 
               "total p uptake from soil", 
               "soil p mineralization",
               "total uptake over requirement",
               "p supply and uptake gap",
               "total P MRT in plant", 
               "total standing PUE",
               "labile Pi stock")
    
    out <- data.frame(terms, NA, NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(out) <- c("terms", "R1", "R2", "R3", "R4", "R5", "R6", "aCO2", "eCO2", "notes")
    
    ### assign values
    out[out$terms == "total standing p stock", 2:7] <- round(total_standing_p_stock,2)
    
    out[out$terms == "total p requirement", 2:9] <- round(total_p_requirement_table[1,],2)
    
    out[out$terms == "total p retranslocated", 2:9] <- round(total_p_retranslocation[1,],2)
    
    out[out$terms == "total p uptake from soil", 2:9] <- round(total_p_uptake_from_soil[1,],2)
    
    out[out$terms == "soil p mineralization", 2:7] <- round(p_mineralization$p_mineralization,2)
    out[out$terms == "soil p mineralization", 8] <- round(mean(p_mineralization$p_mineralization[p_mineralization$Ring%in%c(2,3,6)]),2)
    out[out$terms == "soil p mineralization", 9] <- round(mean(p_mineralization$p_mineralization[p_mineralization$Ring%in%c(1,4,5)]),2)
    
    
    out[out$terms == "p supply and uptake gap", 2:9] <- round(out[out$terms == "total p uptake from soil", 2:9] - out[out$terms == "soil p mineralization", 2:9],2)
    
    out[out$terms == "total uptake over requirement", 2:9] <- round(p_uptake_over_requirement[1,], 2)
    
    out[out$terms == "total P MRT in plant", 2:9] <- round(P_mean_residence_time[1,1:8],2)
    out[out$terms == "total standing PUE", 2:7] <- round(standing_pue[1:6, "NPP_by_PUP"],4)
    
    out[out$terms == "labile Pi stock", 2:7] <- round(summary_table_pool_by_treatment[summary_table_pool_by_treatment$terms=="Exchangeable Pi Pool", 2:7],2)
    
    ### aCO2 and eCO2 averages
    out$aCO2 <- round(rowMeans(data.frame(out$R2, out$R3, out$R6)), 4)
    out$eCO2 <- round(rowMeans(data.frame(out$R1, out$R4, out$R5)) , 4)
    
    
    ### sd
    out$aCO2_sd <- rowSds(as.matrix(subset(out, select=c(R2, R3, R6))), na.rm=T)
    out$eCO2_sd <- rowSds(as.matrix(subset(out, select=c(R1, R4, R5))), na.rm=T)
    
    ### notes
    out[out$terms == "total standing p stock", "notes"] <- "overstorey + understorey"
    
    out[out$terms == "total p requirement", "notes"] <- "NPP by P conc"
    
    out[out$terms == "total p retranslocated", "notes"] <- "guess value for understorey"
    
    out[out$terms == "total p uptake from soil", "notes"] <- "the diff between req and retrans"
    
    out[out$terms == "total uptake over requirement", "notes"] <- "uncertainty in wood and understorey"
    
    out[out$terms == "total P MRT in plant", "notes"] <- "standing stock / uptake"
    
    out[out$terms == "total standing PUE", "notes"] <- "NPP / uptake"
    
    out[out$terms == "labile Pi stock", "notes"] <- "Exchangeable Pi based on Hedley"
    
    
    return(out)
    
}