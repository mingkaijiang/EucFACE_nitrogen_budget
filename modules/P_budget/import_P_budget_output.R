import_P_budget_output <- function() {
    
    ### write script to import all P-relate variables
    if(!dir.exists("output/p_budget")) {
        dir.create("output/p_budget")
    }
    
    
    source.dir <- paste0("/Users/mingkaijiang/Documents/Research/Projects/EucFACE_P_synthesis/EucFACE_P_Git/plots_tables/summary_tables/unnormalized")
    
    dest.dir <- paste0(getwd(), "/output/p_budget")
    
    file.names <- c("summary_cp_ratios.csv",
                    "summary_table_P_concentration_unnormalized.csv",
                    "summary_table_P_flux_unnormalized.csv",
                    "summary_table_P_pool_unnormalized.csv",
                    "total_p_budget_unnormalized.csv")
    
    
    for (i in file.names) {
        command.line <- paste0("cp ", source.dir, "/", i, " ", dest.dir)
        system(command.line)
    }
    
}