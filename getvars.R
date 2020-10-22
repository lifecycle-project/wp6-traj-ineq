################################################################################
## Project: wp6-traj-ineq
## Script purpose: Prepare data for analysis    
## Date: 19th June 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(DSI)
library(DSOpal)
library(dsBaseClient)
library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)

library(remotes)
install_github("lifecycle-project/ds-helper", ref = "ds6")
library(dsHelper)

ls("package:dsBaseClient")
ls("package:DSI")
ls("package:DSOpal")
ls("package:opal")


################################################################################
# 1. Define variables and tables
################################################################################

## ---- Non-repeated -----------------------------------------------------------
nonrep.vars <- c(
  "child_id", "sex", "coh_country", "ethn1_m", "ethn2_m", "ethn3_m", 
  "cohort_id", "eusilc_income_quintiles")

## ---- Yearly repeated --------------------------------------------------------
yearrep.vars <- c(
  "child_id", "edu_m_", "edu_f1_", "age_years", "areases_tert_")

## ---- Monthly repeated -------------------------------------------------------
mhrep.vars <- c(
  "child_id", "int_raw_", "ext_raw_", "adhd_raw_", "asd_raw_", "lan_raw_", 
  "nvi_raw_", "int_age_", "ext_age_", "adhd_age_", "asd_age_", "lan_age_", 
  "nvi_age_", "int_instr_", "ext_instr_", "adhd_instr_", "asd_instr_", 
  "lan_instr_", "nvi_instr", "int_eval_", "ext_eval_", "adhd_eval_", 
  "asd_eval_", "lan_eval_", "nvi_eval_", "int_pro_", "ext_pro_", "adhd_pro_", 
  "asd_pro_", "lan_pro_", "nvi_pro", "int_avg_", "ext_avg_", "adhd_avg_", 
  "asd_avg_", "lan_avg_", "nvi_avg", "int_pc_", "ext_pc_", "adhd_pc_", 
  "asd_pc_", "age_years")

## ---- Table names for cohorts ------------------------------------------------
cohorts_tables <- bind_rows(
  tibble(
    opal_name = "ninfea",
    table = c(
      "lc_ninfea_core_2_0.2_0_core_1_0_non_rep",
      "lc_ninfea_core_2_0.2_0_core_1_0_yearly_rep",
      "lc_ninfea_outcome_1_0.1_0_outcome_1_0_yearly_rep")),
  tibble(
    opal_name = "chop",
    table = c(
      "lc_chop_core_2_1.2_1_core_non_rep_mh_traj",
      "lc_chop_core_2_1.2_1_core_yearly_rep_mh_traj",
      "lc_chop_outcome_1_1.1_1_outcome_yearly_rep_mh_traj")),
  tibble(
    opal_name = "moba",
    table = c(
      "lc_moba_core_2_0.2_0_core_non_rep_bmi_poc_study",
      "lc_moba_core_2_0.2_0_core_yearly_rep_bmi_poc_study",
      "lc_moba_outcome_1_0.1_0_outcome_yearly_rep_inequalities_MH_trajectories")),
  tibble(
    opal_name = "raine",
    table = c(
      "lc_raine_core_2_1.2_1_core_1_0_non_rep",
      "lc_raine_core_2_1.2_1_core_1_0_yearly_rep",
      "lc_raine_outcome_1_1.1_1_outcome_1_0_yearly_rep")),
  tibble(
    opal_name = "dnbc",
    table = c(
      "lc_dnbc_core_2_1.2_1_core_non_rep_tcadman_socecoineq_study",
      "lc_dnbc_core_2_1.2_1_core_yearly_rep_tcadman_socecoineq_study",
      "lc_dnbc_outcome_1_1.1_1_outcome_yearly_rep_tcadman_socecoineq_study")),
  tibble(
    opal_name = "inma",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_0_non_rep_200513_1_Inequalities",
      "lc_isglobal_core_2_1.2_1_core_1_0_yearly_rep_200513_1_Inequalities",
      "lc_isglobal_outcome_1_1.1_1_outcome_1_0_yearly_rep_200513_1_Inequalities"))) %>%
  mutate(type = rep(c("nonrep", "yearrep", "mhrep"), 6))

#datashield.tables(opals)

################################################################################
# 2. Assign variables
################################################################################

## ---- Moba -------------------------------------------------------------------
cohorts_tables %>%
  pwalk(function(opal_name, table, type){
    
    datashield.assign(
      conns = opals[opal_name], 
      symbol = type, 
      value = table, 
      variables = eval(parse(text = paste0(type, ".vars"))))
  })


################################################################################
# 3. Check which measures the different cohorts have used  
################################################################################

## 13 = CBCL
## 47 = SDQ
## 61 = Youth self-report

instruments <- dh.getStats("mhrep", "int_instr_")

instruments[[1]] %>%
  filter(value > 0 & cohort != "combined") %>%
  mutate(
    instrument = case_when(
      category == 13 ~ "CBCL",
      category == 47 ~ "SDQ", 
      category == 61 ~ "YSR")) %>%
  select(cohort, instrument, value)


################################################################################
# 4. Create subsets  
################################################################################

## ---- SDQ subset -------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "mhrep",
  V1.name = "mhrep$int_instr_", 
  V2.name = "47",
  Boolean.operator = "==",
  keep.NAs = TRUE, 
  newobj = "sdq_sub"
)

sdq_coh <- c("ninfea", "chop", "inma", "dnbc")

## ---- CBCL subset ------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "mhrep",
  V1.name = "mhrep$int_instr_", 
  V2.name = "13",
  Boolean.operator = "==",
  keep.NAs = TRUE, 
  newobj = "cbcl_sub"
)

cbcl_coh <- c("chop", "raine", "moba", "inma")


## ---- Save workspace ---------------------------------------------------------
datashield.workspace_save(opals, "traj_assigned_30_09_20")

