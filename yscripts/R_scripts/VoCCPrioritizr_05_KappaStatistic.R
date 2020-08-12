# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

kappa_stats <- function(path1, path2, ...) {
  
  # List of pacakges that we will use
    list.of.packages <- c("dplyr", "irr", "psych")
    # If is not installed, install the pacakge
      new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # is the package in MY list of packages?
      if(length(new.packages)) install.packages(new.packages) # if not -> install it
    # Load packages
      lapply(list.of.packages, require, character.only = TRUE)
  
  # think about what would be the output....
  # comparison no-vocc against scenarios
  # comparison the different approaches against each other
      
      
    nv_fq <- read.csv(path1) %>% 
      arrange(planning_unit)
    yv_fq <- read.csv(path2) %>% 
      arrange(planning_unit)
    
    nv_fq$classes <- ifelse(nv_fq$number == 0, 1, 
                            ifelse(nv_fq$number > 0 & nv_fq$number < 25, 2, 
                                   ifelse(nv_fq$number >= 25 & nv_fq$number < 50, 3,
                                          ifelse(nv_fq$number >= 50 & nv_fq$number < 75, 4, 5))))
    
    yv_fq$classes <- ifelse(yv_fq$number == 0, 1, 
                            ifelse(yv_fq$number > 0 & yv_fq$number < 25, 2, 
                                   ifelse(yv_fq$number >= 25 & yv_fq$number < 50, 3,
                                          ifelse(yv_fq$number >= 50 & yv_fq$number < 75, 4, 5))))
    df3 <- cbind(nv_fq$classes, yv_fq$classes)
    irr::kappa2(df3)
    unlist(cohen.kappa(x = df3))[1]
  
}

# define scenarios here!

kappa_stats(path1 = "output_marxan_b/03-cost-fish_feat-sps-trajclass_ssp245/Scenario01/output/output_ssoln.csv",
            path2 = "output_marxan_b/04-cost-fish_feat-sps_blm-vocc_ssp245/Scenario01/output/output_ssoln.csv")

