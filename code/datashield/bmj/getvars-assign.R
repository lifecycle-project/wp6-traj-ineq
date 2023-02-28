################################################################################
## Project: wp6-traj-ineq
## Script purpose: Prepare data for analysis    
## Date: 19th June 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

################################################################################
# 1. Define variables and tables
################################################################################

## ---- Non-repeated -----------------------------------------------------------
nonrep.vars <- c("child_id", "sex", "coh_country", "cohort_id", "eusilc_income",
                 "eusilc_income_quintiles", "eusilc_income_tertiles", 
                 "agebirth_m_y", "ethn3_m")

## ---- Yearly repeated --------------------------------------------------------
yearrep.vars <- c("child_id", "edu_m_", "age_years")

## ---- Monthly repeated -------------------------------------------------------
mhrep.vars <- c("child_id", "age_years", "ext_raw_", "ext_age_", "ext_instr_", 
                "ext_eval_", "ext_pro_", "ext_avg_", "ext_pc_", "int_raw_", 
                "int_age_", "int_instr_", "int_eval_", "int_pro_", "int_avg_", 
                "int_pc_", "adhd_age_", "adhd_instr_", "adhd_eval_", 
                "adhd_pro_", "adhd_avg_", "adhd_pc_", "adhdR_", "adhdR_age_")

## ---- Table names for cohorts ------------------------------------------------
cohorts_tables <- bind_rows(
  tibble(
    conn_name = "alspac",
    table = c(
      "alspac/2_1_core_1_4/non_rep",
      "alspac/2_1_core_1_4/yearly_rep",
      "alspac/1_1_outcome_1_4/yearly_rep")),
  tibble(
    conn_name = "chop",
    table = c(
      "mhtraj/2_2_core_1_0/non_rep",
      "mhtraj/2_2_core_1_0/yearly_rep",
      "mhtraj/1_5_outcome_1_0/yearly_rep")),
  tibble(
    conn_name = "dnbc",
    table = c(
      "lc_dnbc_core_2_2.2_2_core_non_rep_tcadman_2020-lc13",
      "lc_dnbc_core_2_2.2_2_core_yearly_rep_tcadman_2020-lc19",
      "lc_dnbc_outcome_1_2.1_2_outcome_yearly_rep_tcadman_2020-lc13")),
  tibble(
    conn_name = "eden",
    table = c(
      "project1-eden/2_1_core_1_0/non_rep",
      "project1-eden/2_1_core_1_0/yearly_rep",
      "project1-eden/1_1_outcome_1_0/yearly_rep")),
  tibble(
    conn_name = "genr",
    table = c(
      "lc_genr_core_2_2.2_2_core_non_rep_TC _ECCNLC202035",
      "lc_genr_core_2_2.2_2_core_yearly_rep_TC _ECCNLC202035",
      "lc_genr_outcome_1_2.1_2_outcome_yearly_rep_TC _ECCNLC202035")),
  tibble(
    conn_name = "inma",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_0_non_rep_200513_1_Inequalities",
      "lc_isglobal_core_2_1.2_1_core_1_0_yearly_rep_200513_1_Inequalities",
      "lc_isglobal_outcome_1_1.1_1_outcome_1_0_yearly_rep_200513_1_Inequalities")), 
  tibble(
    conn_name = "moba",
    table = c(
      "lc_moba_core_2_1.2_1_core_2022_3_non_rep_early_determinants_adiposity_new",
      "lc_moba_core_2_1.2_1_core_2022_3_yearly_rep_inequalities_MH_trajectories_new",
      "lc_moba_outcome_1_1.1_1_outcome_2022_3_yearly_rep_inequalities_MH_trajectories_new")),
  tibble(
    conn_name = "raine",
    table = c(
      "lc_raine_core_2_1.2_1_core_1_0_non_rep",
      "lc_raine_core_2_1.2_1_core_1_0_yearly_rep",
      "lc_raine_outcome_1_1.1_1_outcome_1_0_yearly_rep")),
  tibble(
    conn_name = "rhea",
    table = c(
      "lc_rhea_core_2_1.tcadman_nr",
      "lc_rhea_core_2_1.tcadman_y",
      "lc_rhea_outcome_1_1.tcadman_y"))) %>%
  mutate(type = rep(c("nonrep", "yearrep", "mhrep"), 9))

################################################################################
# 2. Assign variables
################################################################################
cohorts_tables %>%
  dplyr::filter(type = "nonrep") %>%
  pwalk(function(conn_name, table, type){
    
    datashield.assign(
      conns = conns[conn_name], 
      symbol = type, 
      value = table, 
      variables = eval(parse(text = paste0(type, ".vars"))))
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")


