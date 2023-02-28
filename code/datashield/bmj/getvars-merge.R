################################################################################
## Project: traj-ineq
## Script purpose: Merge datasets
## Date: 14.12.22
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# MAKE DATASETS FOR EXPOSURES, OUTCOMES and COVARIATES  
################################################################################
################################################################################
# Exposures
################################################################################
ds.dataFrame(
  c("nonrep$child_id", "edu_rank", "income_rank", "mat_ed_df$edu_m", 
    "nonrep$eusilc_income_quintiles"), 
  newobj = "exposures_w")

################################################################################
# Outcomes
################################################################################

## ---- Reference for joins ----------------------------------------------------
mh_merge.ref <- meas.ref %>%
  group_by(outcome) %>%
  group_split %>%
  map(function(x){
    
    tibble(
      outcome = paste0(x$outcome[1], "_sub_t_z"),
      cohorts = list(unique(x$cohort)))
    
  }) %>%
  bind_rows

## ---- Merges -----------------------------------------------------------------
ds.merge(
  x.name = "int_sub_t_z_l",
  y.name = "ext_sub_t_z_l",
  by.x.names = c("child_id", "age", "time"),
  by.y.names = c("child_id", "age", "time"),
  all.x =  T,
  all.y = T,
  newobj = "mh_tmp_l")

ds.merge(
  x.name = "mh_tmp_l",
  y.name = "adhd_sub_t_z_l",
  by.x.names = c("child_id", "age", "time"),
  by.y.names = c("child_id", "age", "time"),
  all.x =  T,
  all.y = T,
  newobj = "mh_df_l", 
  datasources = conns[names(conns) != "genr"])

ds.assign("mh_tmp_l", "mh_df_l", datasources = conns["genr"])

datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Covariates
################################################################################
ds.dataFrame(
  x = c("nonrep$child_id", "nonrep$sex", "nonrep$eusilc_income"), 
  newobj = "covariates_w")

ds.dataFrame(
  x = c("covariates_w", "eth_bin"), 
  newobj = "covariates_w", 
  datasources = conns[eth_coh])

################################################################################
# CREATE MERGED DATASET  
################################################################################

## ---- Exposures and covariates -----------------------------------------------
ds.merge(
  x.name = "exposures_w",
  y.name = "covariates_w",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  all.y = TRUE,
  newobj = "exp_cov_w")

## ---- Now with outcomes ------------------------------------------------------
ds.merge(
  x.name = "mh_df_l", 
  y.name = "exp_cov_w", 
  by.x.names = "child_id", 
  by.y.names = "child_id", 
  all.x = TRUE,
  all.y = TRUE,
  newobj = "baseline_df_l")

## ---- Fill -------------------------------------------------------------------
ds.dataFrameFill("baseline_df_l", "baseline_df_l")

## ---- Make wide format of baseline dataset -----------------------------------
ds.reShape(
  data.name = "baseline_df_l",
  timevar.name = "age",
  idvar.name = "child_id",
  v.names = c("int_raw_", "ext_raw_", "adhd_raw_"),
  direction = "wide", 
  newobj = "baseline_df_w")

## ---- Fix class of ethnicity -------------------------------------------------
dh.columnCast(
  df = "baseline_df_l", 
  target_vars = "eth_bin",
  target_class = "factor")

dh.columnCast(
  df = "baseline_df_w", 
  target_vars = "eth_bin",
  target_class = "factor")

datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Make polynomial age terms   
################################################################################
age_vars <- c("int_age_", "ext_age_", "adhd_age_")

## ---- Add a small amount to the age term -------------------------------------
ds.assign(
  toAssign = paste0("baseline_df_l$age+0.01"), 
  newobj = "age_c")

ds.dataFrame(
  x = c("baseline_df_l", "age_c"), 
  newobj = "baseline_df_l")

dh.makeAgePolys(
  df = "baseline_df_l", 
  age_var = "age_c")

datashield.workspace_save(conns, "mh_traj")
