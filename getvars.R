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

ls("package:dsBaseClient")
################################################################################
# 2. Define variables and tables
################################################################################

## ---- Non-repeated -----------------------------------------------------------
nonrep.vars <- c("child_id", "sex", "coh_country", "cohort_id", "eusilc_income", 
                 "eusilc_income_quintiles")

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
  filter(conn_name %in% c("chop", "moba", "raine", "dnbc", "inma", "elfe", "sws")) %>%
  pwalk(function(conn_name, table, type){
    
    datashield.assign(
      conns = conns[conn_name], 
      symbol = type, 
      value = table, 
      variables = eval(parse(text = paste0(type, ".vars"))))
  })


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_1")
conns <- datashield.login(logindata, restore = "mhtraj_1")

################################################################################
# 3. Sort out blank variables  
################################################################################
ds.dataFrameFill("nonrep", "nonrep")
#ds.dataFrameFill("yearrep", "yearrep")
ds.dataFrameFill("mhrep", "mhrep")


## ---- Save progress ----------------------------------------------------------
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
      category == 2 ~ (lag(valid_perc, 1) + (lag(valid_perc, 1) + valid_perc)) / 2,
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

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_4")
conns <- datashield.login(logindata, restore = "mhtraj_4")

################################################################################
# 6. Create rank income variable  
################################################################################

## ---- Get proporstions -------------------------------------------------------
income_prop <- dh.getStats(
  df = "nonrep",
  vars = "eusilc_income_quintiles",
  conns = conns
)

## ---- Make tibble with values to assign --------------------------------------
## not correct

income_rank <- income_prop[[1]] %>%
  mutate(
    rank_val = case_when(
      category == 1 ~ valid_perc/2,
      category == 2 ~ (valid_perc + lag(valid_perc, 1)) / 2,
      category == 3 ~ (valid_perc + lag(valid_perc, 1)) / 2,
      category == 4 ~ (valid_perc + lag(valid_perc, 1)) / 2,
      category == 5 ~ 100 - (valid_perc/2)),
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
income_rank %>%
  pmap(function(cohort, rank_vec){
    ds.recodeLevels(
      x = "nonrep$eusilc_income_quintiles", 
      newCategories = unlist(str_split(rank_vec, ",")),
      datasources = conns[cohort], 
      newobj = "income_rank")
  })

ds.asNumeric("income_rank", newobj = "income_rank")

ds.cbind(c("nonrep", "income_rank"), newobj = "nonrep")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_5")
conns <- datashield.login(logindata, restore = "mhtraj_5")

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
datashield.workspace_save(conns, "mhtraj_6")
conns <- datashield.login(logindata, restore = "mhtraj_6")


################################################################################
# 7. Create different versions of id variable  
################################################################################

## ---- Integer ----------------------------------------------------------------
ds.asInteger("analysis_df$child_id", "child_id_int")

## ---- Factor -----------------------------------------------------------------
#ds.asFactor("analysis_df$child_id", "child_id_fac")

## ---- Merge back in ----------------------------------------------------------
ds.dataFrame(
  x = c("analysis_df", "child_id_int"), 
  newobj = "analysis_df"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_7")
conns <- datashield.login(logindata, restore = "mhtraj_7")


################################################################################
# 8. Make polynomial transformations of age term
################################################################################

## ---- Normal age terms -------------------------------------------------------
dh.makeAgePolys(
  df = "analysis_df", 
  agevars = c("ext_age_", "int_age_"),
  conns = conns
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_8")
conns <- datashield.login(logindata, restore = "mhtraj_8")

################################################################################
# 9. Describe instruments used   
################################################################################

## As I'm going to work with the raw scores, I need to identify which cohorts
## have what measure at how many time points

## ---- Get descriptives -------------------------------------------------------
instruments <- dh.getStats(
  df = "mhrep", 
  vars = c("ext_instr_", "int_instr_"),
  conns = conns)

mh_available <- instruments[[1]] %>%  mutate(
  instrument = case_when(
    category == 13 ~ "CBCL",
    category == 47 ~ "SDQ"))
  
## ---- Create vectors of cohort names with these measures ---------------------  
ext_pc_avail = mh_available %>% 
  filter(variable == "ext_instr_" & value >0 & cohort != "combined")

int_pc_avail = mh_available %>% 
  filter(variable == "int_instr_" & value >0 & cohort != "combined")

ext_cbcl_avail <- mh_available %>% 
  filter(variable == "ext_instr_" & category == 13 & value >0 & cohort != "combined")
  
ext_sdq_avail <- mh_available %>% 
  filter(variable == "ext_instr_" & category == 47 & value >0 & cohort != "combined")
  
int_cbcl_avail <- mh_available %>% 
  filter(variable == "int_instr_" & category == 13 & value >0 & cohort != "combined")

int_sdq_avail <- mh_available %>% 
  filter(variable == "int_instr_" & category == 47 & value >0 & cohort != "combined")
  
ext_cbcl_tmp <- ext_cbcl_avail %>% pull(cohort) %>% as.character %>% unique
ext_sdq_tmp <- ext_sdq_avail %>% pull(cohort) %>% as.character %>% unique
int_cbcl_tmp <- int_cbcl_avail %>% pull(cohort) %>% as.character %>% unique  
int_sdq_tmp <- int_sdq_avail %>% pull(cohort) %>% as.character %>% unique  

ext_pc_coh_lin <- c("chop", "dnbc", "inma", "moba", "raine")
int_pc_coh_lin <- c("chop", "inma", "moba", "raine")

################################################################################
# 10. Create subsets
################################################################################

## Now we create subsets based on available instruments

## ---- Externalising CBCL -----------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$ext_instr_", 
  V2.name = "13",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "ext_cbcl_sub", 
  datasources = conns[ext_cbcl_tmp]
)

## ---- Externalising SDQ ------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$ext_instr_", 
  V2.name = "47",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "ext_sdq_sub", 
  datasources = conns[ext_sdq_tmp]
)

## ---- Internalising CBCL -----------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$int_instr_", 
  V2.name = "13",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "int_cbcl_sub", 
  datasources = conns[int_cbcl_tmp]
)

## ---- Internalising SDQ ------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$int_instr_", 
  V2.name = "47",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "int_sdq_sub", 
  datasources = conns[int_sdq_tmp]
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_9")
conns <- datashield.login(logindata, restore = "mhtraj_9")


################################################################################
# 11. Visualise data  
################################################################################

## I think the only way to work out how many measurement occasions there are
## currently is to look at the data. Once tapplyassign is fixed it should be 
## possible to get summary data.

## ---- Full dataset externalising ---------------------------------------------
ds.scatterPlot(
  x = "analysis_df$ext_age_", 
  y = "analysis_df$ext_pc_", 
  datasources = conns)

## ---- Full dataset internalising ---------------------------------------------
ds.scatterPlot(
  x = "analysis_df$int_age_", 
  y = "analysis_df$int_pc_", 
  datasources = conns)

## ---- Externalising CBCL -----------------------------------------------------
ds.scatterPlot(
  x = "ext_cbcl_sub$ext_age_", 
  y = "ext_cbcl_sub$ext_raw_", 
  datasources = conns[ext_cbcl_tmp])

## ---- Externalising SDQ ------------------------------------------------------
ds.scatterPlot(
  x = "ext_sdq_sub$ext_age_", 
  y = "ext_sdq_sub$ext_raw_", 
  datasources = conns[ext_sdq_tmp])

## ---- Internalising CBCL -----------------------------------------------------
ds.scatterPlot(
  x = "int_cbcl_sub$ext_age_", 
  y = "int_cbcl_sub$ext_raw_", 
  datasources = conns[int_cbcl_tmp])

## ---- Internalising SDQ ------------------------------------------------------
ds.scatterPlot(
  x = "int_sdq_sub$int_age_", 
  y = "int_sdq_sub$int_raw_", 
  datasources = conns[int_sdq_tmp])

################################################################################
# 12. Define final cohort vectors  
################################################################################

## At the moment this has to be done manually based on inspecting the scatter 
## plot

ext_cbcl_coh <- c("moba", "raine")
ext_sdq_coh <- "dnbc"
int_cbcl_coh <- c("moba", "raine")
int_sdq_coh <- "dnbc"


################################################################################
# 13. Save progress  
################################################################################
datashield.workspace_save(conns, "mhtraj_10")
conns <- datashield.login(logindata, restore = "mhtraj_10")

