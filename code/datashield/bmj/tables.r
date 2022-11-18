################################################################################
## Project: traj-ineq
## Script purpose: Produce tables for manuscipt
## Date: 17th August 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################    
library(dsHelper)
conns <- datashield.login(logindata, restore = "mhtraj_11")
################################################################################
# METHODS  
################################################################################
################################################################################
# 1. Participating cohorts  
################################################################################
included_coh <- c(int_coh, ext_coh, adhd_coh, asd_coh, lan_coh, nvi_coh) %>%
  unique

n_coh <- length(included_coh)

################################################################################
# 2. Maximum sample size
################################################################################

## ---- Get dimensions ---------------------------------------------------------
original_n <- ds.dim("mh_df")

exp_out_l_n <- ds.dim("exp_out_df_l")
exp_out_w_n <- ds.dim("exp_out_df_w")

analysis_l_n <- ds.dim("analysis_df_l")
analysis_l_w <- ds.dim("analysis_df_w")

## ---- Left column ------------------------------------------------------------

# Original
o_l_n <- original_n$`dimensions of mh_df in combined studies`[1]

# Some exposures and outcomes
e_o_l_n <- exp_out_l_n$`dimensions of exp_out_df_l in combined studies`[1]
e_o_w_n <- exp_out_w_n$`dimensions of exp_out_df_w in combined studies`[1]

# Final dataset
a_l_n <- analysis_l_n$`dimensions of analysis_df_l in combined studies`[1]
a_w_n <- analysis_l_w$`dimensions of analysis_df_w in combined studies`[1]


## ---- Right column -----------------------------------------------------------
o_l_n - e_o_l_n
#o_l_w - e_o_w_n

e_o_l_n - a_l_n
e_o_w_n - a_w_n

original_n$`dimensions of mh_df in combined studies`[1] -
exp_out_l_n$`dimensions of exp_out_df_l in combined studies`[1]



  



## ---- Some exposures and outcomes --------------------------------------------




## ---- Analysis dataframe -----------------------------------------------------




exp_out_n_sub <- 


ds.ls()



ds.ls()

## ---- Original dataset -------------------------------------------------------
max_n <- original_n$categorical %>%
  dplyr::filter(variable == "sex" & category == 1) %>%
  dplyr::select(cohort, cohort_n)

## ---- Analysis dataset -------------------------------------------------------
cohort_ns <- descriptives$categorical %>%
  dplyr::filter(variable == "sex" & category == 1) %>%
  dplyr::select(cohort, cohort_n) 

## ---- Participants with no eligible data -------------------------------------
max_n %>% dplyr::filter(cohort == "combined") %>% pull(cohort_n) - 
  cohort_ns %>% dplyr::filter(cohort == "combined") %>% pull(cohort_n)


################################################################################
# 3. Available exposures n cohort  
################################################################################

descriptives$continuous %>% print(n = Inf) 
  
  filter(variable == "ndvi_preg")



  
  dh.lmTab(
    model = int_lin.fit, 
    type = "lmer",
    coh_names = coh_lin,
    direction = "long", 
    ci_format = "separate") 



