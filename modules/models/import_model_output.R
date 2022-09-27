import_model_output <- function() {
    
    ### write script to import all P-relate variables
    if(!dir.exists("output/mip/input")) {
        dir.create("output/mip/input")
    }
    
    
    source.dir <- paste0("/Users/mingkaijiang/Documents/Research/Projects/EucFACE_Modeling/Validation/EucFACE_MIP_validation/output/MIP_output/processed_simulation")
    
    dest.dir <- paste0(getwd(), "/output/mip/input/")
    

    command.line <- paste0("cp -a ", source.dir, "/. ", dest.dir)
    system(command.line)
    
    
}