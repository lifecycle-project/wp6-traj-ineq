################################################################################
## Project: traj-inequality
## Script purpose: Analysis for manuscript
## Date: 18th October 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################    

library(dsHelper)

conns <- datashield.login(logindata, restore = "mhtraj_15")

################################################################################
# 1. MISSING DATA  
################################################################################
################################################################################
# Create subsets to compare missingness at different age periods  
################################################################################

## ---- Reference table --------------------------------------------------------
miss_band.ref <- tibble(
  var = rep(
    c("int_age_", "ext_age_", "adhd_age_", "asd_age_", "nvi_age_", "lan_age_"),
    each = 4),
  outcome = rep(
    c("int_raw_", "ext_raw_", "adhd_raw_", "asd_raw_", "nvi_raw_", "lan_raw_"),
    each = 4),
  
  new_obj = rep(
    c("int_band", "ext_band", "adhd_band", "asd_band", "nvi_band_", "lan_band"), 
    each = 4),
  value_1 = rep(c(0, 4, 8, 14), 6),
  op_1 = rep(">=", 24), 
  value_2 = rep(c(3, 7, 13, 17), 6),
  op_2 = rep(rep("<="), 24), 
  new_df = paste0(var, "b_", value_1))

## ---- Define subsets ---------------------------------------------------------
miss_band.ref %>%
  pmap(function(var, new_obj, value_1, op_1, value_2, op_2, ...){
    
    .BooleTwoConditions(
      df = "analysis_df_l", 
      var = var,
      newobj = new_obj, 
      value_1 = value_1,
      op_1 = op_1,
      value_2 = value_2, 
      op_2 = op_2, 
      conns = conns)
 
  })

## ---- Create subsets ---------------------------------------------------------
miss_band.ref %>%
  pmap(function(var, new_obj, value_1, op_1, value_2, op_2, new_df, ...){
    
    ds.dataFrameSubset(
      df = "analysis_df_l", 
      V1.name = new_obj,
      V2.name = "1",
      Boolean.operator = "==",
      keep.NAs = TRUE, 
      newobj = new_df)
    
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_14")
conns <- datashield.login(logindata, restore = "mhtraj_14")

save.image()

################################################################################
# 2. MODELS  
################################################################################
################################################################################
# Internalising  
################################################################################
var_suf <-  c("_age_", "_age__m_2", "_age__m_1", "_age__m_0_5", "_age__log", 
              "_age__0_5", "_age__2", "_age__3")

## ---- Make formulae ----------------------------------------------------------
int.mod <- dh.makeLmerForm(
  outcome = "int_pc_", 
  id_var = "child_id_int", 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num", 
  age_vars = paste0("int", var_suf),
  random = "intercept")

## ---- Run models -------------------------------------------------------------
int.fit <- dh.lmeMultPoly(
  df = "analysis_df_l_m",
  formulae = int.mod$formula, 
  poly_names = int.mod$polys, 
  conns = conns[int_coh])

save.image()

################################################################################
# Externalising  
################################################################################

## ---- Make formulae ----------------------------------------------------------
ext.mod <- dh.makeLmerForm(
  outcome = "ext_pc_", 
  id_var = "child_id_int", 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num", 
  age_vars = paste0("ext", var_suf),
  random = "intercept")

## ---- Run models -------------------------------------------------------------
ext.fit <- dh.lmeMultPoly(
  df = "analysis_df_l",
  formulae = ext.mod$formula, 
  poly_names = ext.mod$polys, 
  conns = conns[ext_coh])

save.image()

################################################################################
# ADHD  
################################################################################

## ---- Make formulae ----------------------------------------------------------
adhd.mod <- dh.makeLmerForm(
  outcome = "adhd_pc_", 
  id_var = "child_id_int", 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num", 
  age_vars = paste0("adhd", var_suf),
  random = "intercept")

## ---- Run models -------------------------------------------------------------
adhd.fit <- dh.lmeMultPoly(
  df = "analysis_df_l",
  formulae = adhd.mod$formula, 
  poly_names = adhd.mod$polys, 
  conns = conns[adhd_coh[adhd_coh != "ninfea"]])

## Not working for NINFEA - issue with converting child id to numeric

save.image()

################################################################################
# ASC  
################################################################################

## ---- Make formulae ----------------------------------------------------------
asd.form <- dh.makeLmerForm(
  outcome = "asd_pc_", 
  id_var = "child_id_int", 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num", 
  age_vars = paste0("asd", var_suf),
  random = "intercept")

## ---- Run models -------------------------------------------------------------
asd.fit <- dh.lmeMultPoly(
  df = "analysis_df_l",
  formulae = asd.form$formula, 
  poly_names = asd.form$polys, 
  conns = conns["moba"])

save.image()

## Looks like INMA doesn't have rm data on this

################################################################################
# Language  
################################################################################

## ---- Make formulae ----------------------------------------------------------
lan.form <- dh.makeLmerForm(
  outcome = "lan_pc_", 
  id_var = "child_id_int", 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num", 
  age_vars = paste0("lan", var_suf),
  random = "intercept")

## ---- Run models -------------------------------------------------------------
lan.fit <- dh.lmeMultPoly(
  df = "analysis_df_l",
  formulae = lan.form$formula, 
  poly_names = lan.form$polys, 
  conns = conns[lan_coh])

save.image()

################################################################################
# NVI  
################################################################################

## ---- Make formulae ----------------------------------------------------------
nvi.form <- dh.makeLmerForm(
  outcome = "nvi_pc_", 
  id_var = "child_id_int", 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num", 
  age_vars = paste0("nvi", var_suf),
  random = "intercept")

## ---- Run models -------------------------------------------------------------
nvi.fit <- dh.lmeMultPoly(
  df = "analysis_df_l",
  formulae = nvi.form$formula, 
  poly_names = nvi.form$polys, 
  conns = conns[nvi_coh])

save.image()






################################################################################
# Final models  
################################################################################

## ---- Internalising ----------------------------------------------------------
ext.fit <- ds.lmerSLMA(
  dataName = "analysis_df_l",
  formula = "int_pc_ ~ 1 + edu_rank_num + sex + int_age__m_1 + int_age__m_2 +
  edu_rank_num*int_age__m_1 + edu_rank_num*int_age__m_2 + sex+
  
  
  (1|child_id_int)", 
  datasources = conns[!names(conns) == "moba"])

## ---- Externalising ----------------------------------------------------------


## ---- ADHD -------------------------------------------------------------------



## ---- ASD --------------------------------------------------------------------



## ---- Language ---------------------------------------------------------------



## ---- NVI --------------------------------------------------------------------

ext_lin.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + edu_rank_num + ext_age_ + sex + (1|child_id_int)", 
  datasources = conns[!names(conns) == "moba"])

mod_vars <- dh.getStats(
  df = "analysis_df", 
  vars = c("ext_pc_", "edu_rank_num", "ext_age_", "sex", "child_id_int"))

newdata <- tibble(
  edu_rank_num = rep(c(1, 2), 50), 
  ext_age_ = seq(1, 100, 1))

test <- dh.predictLmer(
  model = test.fit, 
  new_data = newdata, 
  coh_names = c("alspac", "moba"))




str(test.fit)

test.fit$output.summary$study1$devcomp$dims[["N"]]

test.fit <- ds.lmerSLMA(
  dataName = "analysis_df_l",
  formula = "ext_pc_ ~ 1 + edu_rank_num + ext_age_ + (1|child_id_int)", 
  datasources = conns[c("alspac", "moba")])

model = test.fit

test.fit2 <- ds.glmSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + edu_rank_num + ext_age_", 
  family = "gaussian",
  datasources = conns[c("alspac", "moba")])








  
  str(model[[1]])


