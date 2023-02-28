################################################################################
## Project: traj-ineq
## Script purpose: Define valid cases
## Date: 4th January 2023
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

conns <- datashield.login(logindata, restore = "mh_traj")
################################################################################
# Some exposures
################################################################################
dh.defineCases(
  df = "baseline_df_l", 
  vars = c("edu_rank", "income_rank"),
  type = "any", 
  new_obj = "any_exp")

## ---- Create long subset -----------------------------------------------------
ds.dataFrameSubset(
  df.name = "baseline_df_l", 
  V1.name = "any_exp", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "any_exp_df_l")

## ---- Create wide subset -----------------------------------------------------
ds.reShape(
  data.name = "any_exp_df_l",
  timevar.name = "age",
  idvar.name = "child_id",
  v.names = c("int_raw_", "ext_raw_", "adhd_raw_"),
  direction = "wide", 
  newobj = "any_exp_df_w")

datashield.workspace_save(conns, "mh_traj")

################################################################################
# Some outcomes  
################################################################################
dh.defineCases(
  df = "any_exp_df_l", 
  vars = c("int_raw_", "ext_raw_","adhd_raw_"),
  type = "any", 
  new_obj = "any_out")

## ---- Create long subset -----------------------------------------------------
ds.dataFrameSubset(
  df.name = "any_exp_df_l", 
  V1.name = "any_out", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "any_out_df_l")

## ---- Create wide subset -----------------------------------------------------
ds.reShape(
  data.name = "any_out_df_l",
  timevar.name = "age",
  idvar.name = "child_id",
  v.names = c("int_raw_", "ext_raw_", "adhd_raw_"),
  direction = "wide", 
  newobj = "any_out_df_w")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Non-missing sex
################################################################################
dh.defineCases(
  df = "any_out_df_l", 
  vars = "sex", 
  type = "any", 
  new_obj = "any_cov")

## ---- Create long subset -----------------------------------------------------
ds.dataFrameSubset(
  df.name = "any_out_df_l", 
  V1.name = "any_cov", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "analysis_df_l")

## ---- Create wide subset -----------------------------------------------------
ds.reShape(
  data.name = "analysis_df_l",
  timevar.name = "age",
  idvar.name = "child_id",
  v.names = c("int_raw_", "ext_raw_", "adhd_raw_"),
  direction = "wide", 
  newobj = "analysis_df_w")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Define excluded DFs  
################################################################################
dt.makeExcludedDf(
  original_df = "baseline_df_w", 
  final_df = "analysis_df_w",
  new_obj = "excluded_df_w")

#dt.makeExcludedDf(
#  original_df = "baseline_df_l", 
#  final_df = "analysis_df_l",
#  type = "long",
#  new_obj = "excluded_df_l")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Make complete case subsets
################################################################################
out_stem <- c("int_", "ext_", "adhd_")
exp_cov.vars <- c("child_id", "edu_rank", "sex")

ds.colnames("baseline_df_l")

## ---- Define subsets ---------------------------------------------------------
miss.ref <- tibble(
  out_stem = out_stem,
  outcome = paste0(out_stem, "raw_"), 
  age_var = "age", 
  new_vec = paste0(out_stem, "valid"),
  inc_long = paste0(out_stem, "sub_l_inc"),
  inc_wide = paste0(out_stem, "sub_w_inc"),
  exc_long = paste0(out_stem, "sub_l_exc"),
  exc_wide = paste0(out_stem, "sub_w_exc"),
  cohort = c(
    list(int.coh), 
    list(ext.coh), 
    list(adhd.coh)),
  vars = c(
    list(c("int_t_z", "age", exp_cov.vars)), 
    list(c("ext_t_z", "age", exp_cov.vars)), 
    list(c("adhd_t_z", "age", exp_cov.vars))))

save.image()

## ---- Drop variables ---------------------------------------------------------
miss.ref %>%
  pmap(function(new_vec, vars, cohort, ...){
    
    dh.defineCases(
      df = "baseline_df_l", 
      vars = unlist(vars),
      type = "all", 
      new_obj = new_vec,
      checks = FALSE, 
      conns = conns[cohort])
    
  })

datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

## ---- Create included subset, long -------------------------------------------  
miss.ref %>%
  pmap(function(new_vec, inc_long, cohort, ...){
    
    ds.dataFrameSubset(
      df.name = "baseline_df_l", 
      V1.name = new_vec, 
      V2.name = "1",
      Boolean.operator = "==",
      keep.NAs = FALSE, 
      newobj = inc_long, 
      datasources = conns[unlist(cohort)])
    
  }) 

datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

## ---- Create excluded subset, long -------------------------------------------
miss.ref %>%
  pmap(function(new_vec, exc_long, cohort, ...){
    
    ds.dataFrameSubset(
      df.name = "baseline_df_l", 
      V1.name = new_vec, 
      V2.name = "0",
      Boolean.operator = "==",
      keep.NAs = FALSE, 
      newobj = exc_long, 
      datasources = conns[cohort])
    
  }) 

datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

## ---- Create included subset, wide -------------------------------------------
miss.ref %>%
  pmap(function(inc_long, age_var, outcome, inc_wide, cohort, ...){
    
    ds.reShape(
      data.name = inc_long,
      timevar.name = age_var,
      idvar.name = "child_id",
      v.names = outcome,
      direction = "wide", 
      newobj = inc_wide, 
      datasources = conns[cohort])
    
  }) 

datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

## ---- Create excluded subset, wide -------------------------------------------
miss.ref %>%
  pmap(function(exc_long, age_var, outcome, exc_wide, cohort, ...){
    
    ds.reShape(
      data.name = exc_long,
      timevar.name = age_var,
      idvar.name = "child_id",
      v.names = outcome,
      direction = "wide", 
      newobj = exc_wide, 
      datasources = conns[cohort])
    
  }) 

datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Create Male and Female subsets  
################################################################################

## ---- Male -------------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df_l", 
  V1.name = "analysis_df_l$sex", 
  V2.name = "1",
  Boolean.operator = "==", 
  newobj = "analysis_df_l_m")

## ---- Female -----------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df_l", 
  V1.name = "analysis_df_l$sex", 
  V2.name = "2",
  Boolean.operator = "==", 
  newobj = "analysis_df_l_f")

datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")


