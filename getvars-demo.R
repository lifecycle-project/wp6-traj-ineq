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

#library(remotes)
#install_github("lifecycle-project/ds-helper", ref = "ds6")
library(dsHelper)

ls("package:dsBaseClient")
ls("package:DSI")
ls("package:DSOpal")
ls("package:opal")


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
      "lc_isglobal_outcome_1_1.1_1_outcome_1_0_yearly_rep_200513_1_Inequalities"))) %>%
  mutate(type = rep(c("nonrep", "yearrep", "mhrep"), 6))

#datashield.tables(opals)

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

ed_props <- dh.getStats(
  df = "analysis_df", 
  vars = "edu_m"
)

## Get the rank scores to convert to
sii_vals <- ed_props[[1]] %>%
  group_by(cohort) %>%
  mutate(rank = case_when(
    category == 1 ~ valid_perc/2,
    category == 2 ~ valid_perc + lag(valid_perc) / 2, 
    category == 3 ~ 100 - valid_perc / 2)) %>%
  ungroup() %>% 
  arrange(cohort) %>%
  filter(cohort != "combined") %>%
  select(category, cohort, rank) %>%
  group_split(category)
  
## Now we take three subsets of the dataset and assign the correct values
ds.asInteger("analysis_df$child_id", "id_int")
ds.asInteger("analysis_df$edu_m", "edu_int")

ds.cbind(
  x = c("analysis_df", "id_int", "edu_int"), 
  newobj = "analysis_df"
)

ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "edu_int", 
  V2.name = "1", 
  keep.NAs = FALSE,
  Boolean.operator = ">=", 
  newobj = "mat_ed_sub"
)

c("1", "2", "3") %>%
  map(
    ~ds.dataFrameSubset(
      df.name = "mat_ed_sub",
      V1.name = "mat_ed_sub$edu_m", 
      V2.name = ., 
      Boolean.operator = "==",
      newobj = paste0("edu_sub_", .)
    )
)

sii_vals[[1]] %>%
  pmap(function(cohort, rank, ...){
    
    ds.assign(
      toAssign = paste0("edu_sub_1$id_int-edu_sub_1$id_int+1*", rank),
      newobj = "mat_ed_rank_1", 
      datasources = opals[cohort]
    )
  })
#

ds.assign(
  toAssign = "edu_sub_1$id_int-edu_sub_1$id_int+1*34.15",
  newobj = "mat_ed_rank_1", 
  datasources = opals["moba"])

ds.assign(
  toAssign = "edu_sub_1$id_int-edu_sub_1$id_int+1*26.06",
  newobj = "mat_ed_rank_1", 
  datasources = opals["dnbc"])


sii_vals[[2]] %>%
  pmap(function(cohort, rank, ...){
    
    ds.assign(
      toAssign = paste0("edu_sub_2$id_int-edu_sub_2$id_int+1*", rank),
      newobj = "mat_ed_rank_2", 
      datasources = opals[cohort]
    )
  })

ds.assign(
  toAssign = "edu_sub_2$id_int-edu_sub_2$id_int+1*63.9",
  newobj = "mat_ed_rank_2", 
  datasources = opals["moba"])

ds.assign(
  toAssign = "edu_sub_2$id_int-edu_sub_2$id_int+1*47.8",
  newobj = "mat_ed_rank_2", 
  datasources = opals["dnbc"])



sii_vals[[3]] %>%
  pmap(function(cohort, rank, ...){
    
    ds.assign(
      toAssign = paste0("edu_sub_3$id_int-edu_sub_3$id_int+1*", rank),
      newobj = "mat_ed_rank_3", 
      datasources = opals[cohort]
    )
  })

ds.assign(
  toAssign = "edu_sub_3$id_int-edu_sub_3$id_int+1*99",
  newobj = "mat_ed_rank_3", 
  datasources = opals["moba"])

ds.assign(
  toAssign = "edu_sub_3$id_int-edu_sub_3$id_int+1*86.9",
  newobj = "mat_ed_rank_3", 
  datasources = opals["dnbc"])

ds.rbind(
  x = c("mat_ed_rank_1", "mat_ed_rank_2", "mat_ed_rank_3"), 
    newobj = "mat_ed_rank")

ds.cbind(
  x = c("mat_ed_sub", "mat_ed_rank"), 
  newobj = "mat_ed_sub"
)

dh.renameVars(
  df = "mat_ed_sub", 
  names = tibble(oldvar = "mat_ed_rank_1", newvar = "mat_ed_rank")
)

ds.assign(
  toAssign = "mat_ed_sub$mat_ed_rank*mat_ed_sub$ext_age_", 
  newobj = "mat_ed_age"
)

ds.cbind(
  x = c("mat_ed_sub", "mat_ed_age"),
  newobj = "mat_ed_sub"
)

ds.dataFrameSubset(
  df.name = "mat_ed_sub", 
  V1.name = "mat_ed_sub$sex", 
  V2.name = "1", 
  Boolean.operator = "==", 
  newobj = "mat_ed_sub_m"
)

ds.dataFrameSubset(
  df.name = "mat_ed_sub", 
  V1.name = "mat_ed_sub$sex", 
  V2.name = "2", 
  Boolean.operator = "==", 
  newobj = "mat_ed_sub_f"
)

datashield.workspace_save(opals, "created_sii_df")

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

## ---- Save workspace ---------------------------------------------------------
datashield.workspace_save(opals, "traj_assigned_23_10_20")

