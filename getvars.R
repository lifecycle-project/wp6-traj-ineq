################################################################################
## Project: wp6-traj-ineq
## Script purpose: Prepare data for analysis    
## Date: 19th June 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

################################################################################
# 1. Load packages  
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

#library(remotes)
#install_github("lifecycle-project/ds-helper", ref = "maintenance")
library(dsHelper)


################################################################################
# 2. Define variables and tables
################################################################################

## ---- Non-repeated -----------------------------------------------------------
nonrep.vars <- c("child_id", "sex", "coh_country", "cohort_id")

## ---- Yearly repeated --------------------------------------------------------
yearrep.vars <- c("child_id", "edu_m_", "age_years")

## ---- Monthly repeated -------------------------------------------------------
mhrep.vars <- c("child_id", "age_years", "ext_raw_", "ext_age_", "ext_instr_", "ext_eval_", 
                "ext_pro_", "ext_avg_", "ext_pc_", "int_raw_", "int_age_", 
                "int_instr_", "int_eval_", "int_pro_", "int_avg_", "int_pc_", 
                "asd_raw_", "asd_age_", "asd_instr_", "asd_eval_", "asd_pro_", 
                "asd_avg_", "asd_pc_", "adhd_raw_", "adhd_age_", "adhd_instr_", 
                "adhd_eval_", "adhd_pro_", "adhd_avg_", "adhd_pc_", "nvi_raw_", 
                "nvi_age_", "nvi_instr_", "nvi_eval_", "nvi_pro_", "nvi_avg_", 
                "nvi_pc_", "lan_raw_", "lan_age_", "lan_instr_", "lan_eval_", 
                "lan_pro_", "lan_avg_", "lan_pc_")

## ---- Table names for cohorts ------------------------------------------------
cohorts_tables <- bind_rows(
  tibble(
    conn_name = "ninfea",
    table = c(
      "lc_ninfea_core_2_0.2_0_core_1_0_non_rep",
      "lc_ninfea_core_2_0.2_0_core_1_0_yearly_rep",
      "lc_ninfea_outcome_1_0.1_0_outcome_1_0_yearly_rep")),
  tibble(
    conn_name = "chop",
    table = c(
      "lc_chop_core_2_1.2_1_core_non_rep_bmi_earlylife_poc",
      "lc_chop_core_2_1.2_1_core_yearly_rep_mh_traj",
      "lc_chop_outcome_1_1.1_1_outcome_yearly_rep_mh_traj")),
  tibble(
    conn_name = "moba",
    table = c(
      "lc_moba_core_2_0.2_0_core_non_rep_bmi_poc_study",
      "lc_moba_core_2_0.2_0_core_yearly_rep_bmi_poc_study",
      "lc_moba_outcome_1_0.1_0_outcome_yearly_rep_inequalities_MH_trajectories")),
  tibble(
    conn_name = "raine",
    table = c(
      "lc_raine_core_2_1.2_1_core_1_0_non_rep",
      "lc_raine_core_2_1.2_1_core_1_0_yearly_rep",
      "lc_raine_outcome_1_1.1_1_outcome_1_0_yearly_rep")),
  tibble(
    conn_name = "dnbc",
    table = c(
      "lc_dnbc_core_2_1.2_1_core_non_rep_tcadman_2020-lc19",
      "lc_dnbc_core_2_1.2_1_core_yearly_rep_tcadman_2020-lc19",
      "lc_dnbc_outcome_1_1.1_1_outcome_yearly_rep_tcadman_socecoineq_study")),
  tibble(
    conn_name = "inma",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_0_non_rep_200513_1_Inequalities",
      "lc_isglobal_core_2_1.2_1_core_1_0_yearly_rep_200513_1_Inequalities",
      "lc_isglobal_outcome_1_1.1_1_outcome_1_0_yearly_rep_200513_1_Inequalities")), 
  tibble(
    conn_name = "sws",
    table = c(
      "lc_sws_core_2_1.2_1_core_1_1_non_rep",
      "lc_sws_core_2_1.2_1_core_1_1_yearly_rep",
      "lc_sws_outcome_1_1.1_1_outcome_1_1_yearly_rep")),
  tibble(
    conn_name = "elfe",
    table = c(
      "lc_elfe_core_2_1.Project1_WP6_non_rep",
      "lc_elfe_core_2_1.Project1_WP6_yearly_rep",
      "lc_elfe_outcome_1_1.Project1_WP6_yearly_rep")),
  tibble(
    conn_name = "nfbc86",
    table = c(
      "lc_nfbc86_core_2_1.p0650_nfbc86_2_1_core_non_rep",
      "lc_nfbc86_core_2_1.p0650_nfbc86_2_1_core_yearly_rep",
      "lc_nfbc86_outcome_1_1.p0650_nfbc86_1_1_outcome_yearly_rep"))) %>%
  mutate(type = rep(c("nonrep", "yearrep", "mhrep"), 9))


################################################################################
# 2. Assign variables
################################################################################
cohorts_tables %>%
  filter(conn_name %in% c("chop", "moba", "raine", "dnbc", "inma", "elfe")) %>%
  pwalk(function(conn_name, table, type){
    
    datashield.assign(
      conns = conns[conn_name], 
      symbol = type, 
      value = table, 
      variables = eval(parse(text = paste0(type, ".vars"))))
  })

datashield.workspace_save(conns, "mhtraj_1")
conns <- datashield.login(logindata, restore = "mhtraj_1")

################################################################################
# 3. Sort out blank variables  
################################################################################
ds.dataFrameFill("mhrep", "mhrep")

datashield.workspace_save(conns, "mhtraj_2")
conns <- datashield.login(logindata, restore = "mhtraj_2")


################################################################################
# 4. Derive maternal education variable
################################################################################

## ---- Subset to keep only measurements from first year -----------------------
ds.dataFrameSubset(
  df.name = "yearrep", 
  V1.name = "yearrep$age_years",
  V2.name = "0",
  Boolean.operator = "==",
  newobj = "baseline_vars")

## ---- Reshape ----------------------------------------------------------------
ds.reShape(
  data.name = "baseline_vars",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = "edu_m_", 
  direction = "wide", 
  newobj = "baseline_wide")

## ---- Rename education variable ----------------------------------------------
dh.renameVars(
  df = "baseline_wide", 
  names = tibble(oldvar = "edu_m_.0", newvar = "edu_m"), 
  conns = conns)

## ---- Tidy dataframe ---------------------------------------------------------
dh.dropCols(
  df = "baseline_wide",
  vars = c("edu_m_.0"), 
  type = "remove",
  comp_var = "child_id",
  new_df_name = "baseline_wide",
  conns = conns
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_3")
conns <- datashield.login(logindata, restore = "mhtraj_3")

################################################################################
# 5. Create rank maternal education variable  
################################################################################

## ---- Get proporstions -------------------------------------------------------
mat_prop <- dh.getStats(
  df = "baseline_wide",
  vars = "edu_m",
  conns = conns
)

## ---- Make tibble with values to assign --------------------------------------
rank <- mat_prop[[1]] %>%
  mutate(
    rank_val = case_when(
      category == 1 ~ valid_perc/2,
      category == 2 ~ (valid_perc + lag(valid_perc, 1)) / 2,
      category == 3 ~ 100 - (valid_perc/2)),
    rank_val = rank_val / 100) %>%
  filter(cohort != "combined") %>%
  group_by(cohort) %>%
  select(cohort, rank_val) %>%
  group_split %>%
  map(function(x){
    
    mutate(x, rank_vec = paste(rank_val, collapse = ",")) %>%
      head(1) %>%
      select(-rank_val)
  }) %>%
  bind_rows %>%
  mutate_at(vars(cohort), as.character)
    
## ---- Now recode education variable ------------------------------------------
rank %>%
  pmap(function(cohort, rank_vec){
    ds.recodeLevels(
    x = "baseline_wide$edu_m", 
    newCategories = unlist(str_split(rank_vec, ",")),
    datasources = conns[cohort], 
    newobj = "edu_rank")
  })

ds.asNumeric("edu_rank", newobj = "edu_rank_num")

ds.cbind(
  c("baseline_wide", "edu_rank_num"),
   newobj = "baseline_wide")

ds.summary("baseline_wide$edu_rank_num")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_4")
conns <- datashield.login(logindata, restore = "mhtraj_4")


################################################################################
# 6. Merge datasets  
################################################################################
ds.merge(
  x.name = "nonrep",
  y.name = "mhrep",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  all.y = TRUE,
  newobj = "analysis_df"
)

ds.merge(
  x.name = "analysis_df", 
  y.name = "baseline_wide", 
  by.x.names = "child_id", 
  by.y.names = "child_id", 
  all.x = TRUE,
  all.y = TRUE,
  newobj = "analysis_df"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_5")
conns <- datashield.login(logindata, restore = "mhtraj_5")


################################################################################
# 7. Create integer version of id variable  
################################################################################
ds.asInteger("analysis_df$child_id", "child_id_int")

ds.dataFrame(
  x = c("analysis_df", "child_id_int"), 
  newobj = "analysis_df"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_6")
conns <- datashield.login(logindata, restore = "mhtraj_6")


################################################################################
# 8. Make polynomial transformations of age term
################################################################################

## ---- Normal age terms -------------------------------------------------------
dh.makeAgePolys(
  df = "analysis_df", 
  agevars = c("ext_age_", "int_age_"),
  conns = conns
)


################################################################################
# 10. Save progress  
################################################################################
datashield.workspace_save(conns, "mhtraj_7")
conns <- datashield.login(logindata, restore = "mhtraj_7")

