################################################################################
## Project: traj-ineq
## Script purpose: Produce tables for manuscipt
## Date: 17th August 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################    
library(dsHelper)
conns <- datashield.login(logindata, restore = "mhtraj_9")
################################################################################
# METHODS  
################################################################################
################################################################################
# 1. Participating cohorts  
################################################################################
included_coh <- c(int_coh, ext_coh, adhd_coh, asd_coh, lan_coh, nvi_coh) %>%
  unique

n_coh <- length(included_coh)

not_included_coh <- names(conns)[!names(conns) %in% included_coh]


################################################################################
# 2. Maximum sample size
################################################################################
original_n <- dh.getStats(
  df = "nonrep", 
  vars = "sex",
  conns = conns[included_coh]
)

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



