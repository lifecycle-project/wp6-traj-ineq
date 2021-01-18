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

library(remotes)
install_github("sidohaakma/ds-helper", ref = "fix/do-not-use-opals-global-object")
library(dsHelper)



################################################################################
# 1. Define variables and tables
################################################################################

## ---- Non-repeated -----------------------------------------------------------
nonrep.vars <- c("child_id", "sex", "coh_country", "cohort_id")

## ---- Yearly repeated --------------------------------------------------------
yearrep.vars <- c("child_id", "edu_m_", "age_years")

## ---- Monthly repeated -------------------------------------------------------
mhrep.vars <- c("child_id", "ext_raw_", "ext_age_", "ext_instr_", "ext_eval_", 
                "ext_pro_", "ext_avg_", "ext_pc_")

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
      "lc_chop_core_2_1.2_1_core_non_rep_bmi_earlylife_poc",
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
      "lc_dnbc_core_2_1.2_1_core_non_rep_tcadman_2020-lc19",
      "lc_dnbc_core_2_1.2_1_core_yearly_rep_tcadman_2020-lc19",
      "lc_dnbc_outcome_1_1.1_1_outcome_yearly_rep_tcadman_socecoineq_study")),
  tibble(
    opal_name = "inma",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_0_non_rep_200513_1_Inequalities",
      "lc_isglobal_core_2_1.2_1_core_1_0_yearly_rep_200513_1_Inequalities",
      "lc_isglobal_outcome_1_1.1_1_outcome_1_0_yearly_rep_200513_1_Inequalities")), 
  tibble(
    opal_name = "sws",
    table = c(
      "",
      "",
      "")),
  tibble(
    opal_name = "elfe",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_0_non_rep_200513_1_Inequalities",
      "lc_isglobal_core_2_1.2_1_core_1_0_yearly_rep_200513_1_Inequalities",
      "lc_isglobal_outcome_1_1.1_1_outcome_1_0_yearly_rep_200513_1_Inequalities")),
  tibble(
    opal_name = "nfbc86",
    table = c(
      "",
      "",
      ""))
  
  
  
  ) %>%
  mutate(type = rep(c("nonrep", "yearrep", "mhrep"), 6))


################################################################################
# 2. Assign variables
################################################################################
cohorts_tables %>%
  filter(opal_name != "ninfea") %>%
  pwalk(function(opal_name, table, type){
    
    datashield.assign(
      conns = opals[opal_name], 
      symbol = type, 
      value = table, 
      variables = eval(parse(text = paste0(type, ".vars"))))
  })

################################################################################
# 3. Check for class problems  
################################################################################
dh.classDescrepancy("nonrep")
dh.classDescrepancy("yearrep")
dh.classDescrepancy("mhrep")

# All fine

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(opals, "mhtraj_1")
opals <- datashield.login(logindata, restore = "mhtraj_1")




ds.colnames("nonrep", datasources = opals["raine"])

old_new <- tibble(
  oldvar = "child_id", 
  newvar = "testtest"
)

dsHelper::dh.renameVars(
  df = "nonrep" ,
  names = old_new, 
  conns = opals["raine"]
)


################################################################################
# 4. Derive maternal education variable
################################################################################

## ---- Create vector to use in subsetting -------------------------------------
ds.make(toAssign = "yearrep$age_years-yearrep$age_years", newobj = "zeros") 

## ---- Subset to keep only measurements from first year -----------------------
ds.dataFrameSubset(
  df.name = "yearrep", 
  V1.name = "yearrep$age_years",
  V2.name = "zeros",
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
  names = tibble(oldvar = "edu_m_.0", newvar = "edu_m"))

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(opals, "mhtraj_4")
opals <- datashield.login(logindata, restore = "mhtraj_4")


################################################################################
# 5. Check which measures the different cohorts have used  
################################################################################

## 13 = CBCL
## 47 = SDQ
## 61 = Youth self-report

instruments <- dh.getStats("mhrep", "ext_instr_")

instruments[[1]] %>%
  filter(value > 0 & cohort != "combined") %>%
  mutate(
    instrument = case_when(
      category == 13 ~ "CBCL",
      category == 47 ~ "SDQ", 
      category == 61 ~ "YSR")) %>%
  select(cohort, instrument, value)


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
datashield.workspace_save(opals, "mhtraj_6")
opals <- datashield.login(logindata, restore = "mhtraj_6")


################################################################################
# 7. Create externalising subsets  
################################################################################
ds.dataFrameSubset(
  df.name = "analysis_df",
  V1.name = "analysis_df$ext_raw_", 
  V2.name = "0",
  Boolean.operator = ">=",
  keep.NAs = FALSE, 
  newobj = "analysis_df"
)

## ---- SDQ subset -------------------------------------------------------------
sdq_opals <- c("chop", "inma", "dnbc")

ds.dataFrameSubset(
  df.name = "analysis_df",
  V1.name = "analysis_df$ext_instr_", 
  V2.name = "47",
  Boolean.operator = "==",
  keep.NAs = TRUE, 
  newobj = "sdq_sub", 
  datasources = opals[sdq_opals]
)

## ---- Rename variable with too long a name -----------------------------------
ds.assign(
  toAssign = "sdq_sub$ext_raw_",
  newobj = "e_r", 
  datasources = opals[sdq_opals]
)

ds.cbind(
  x = c("sdq_sub", "e_r"), 
  newobj = "sdq_sub", 
  datasources = opals[sdq_opals])

ds.dataFrameSubset(
  df.name = "sdq_sub",
  V1.name = "sdq_sub$sex", 
  V2.name = "1",
  Boolean.operator = "==",
  keep.NAs = TRUE, 
  newobj = "sdq_sub_m", 
  datasources = opals[sdq_opals]
)

ds.dataFrameSubset(
  df.name = "sdq_sub",
  V1.name = "sdq_sub$sex", 
  V2.name = "2",
  Boolean.operator = "==",
  keep.NAs = TRUE, 
  newobj = "sdq_sub_f", 
  datasources = opals[sdq_opals]
)


## ---- CBCL subset ------------------------------------------------------------
cbcl_opals <- c("chop", "moba", "inma", "raine")

ds.dataFrameSubset(
  df.name = "analysis_df",
  V1.name = "analysis_df$ext_instr_", 
  V2.name = "13",
  Boolean.operator = "==",
  keep.NAs = TRUE, 
  newobj = "cbcl_sub", 
  datasources = opals[cbcl_opals])

ds.dataFrameSubset(
  df.name = "cbcl_sub",
  V1.name = "cbcl_sub$sex", 
  V2.name = "1",
  Boolean.operator = "==",
  keep.NAs = TRUE, 
  newobj = "cbcl_sub_m", 
  datasources = opals[cbcl_opals]
)

ds.dataFrameSubset(
  df.name = "cbcl_sub",
  V1.name = "cbcl_sub$sex", 
  V2.name = "2",
  Boolean.operator = "==",
  keep.NAs = TRUE, 
  newobj = "cbcl_sub_f", 
  datasources = opals[cbcl_opals]
)


################################################################################
# 8. Save final dataset  
################################################################################
datashield.workspace_save(opals, "traj_assigned_23_10_20")

