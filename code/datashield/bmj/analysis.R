################################################################################
## Project: traj-inequality
## Script purpose: Analysis for manuscript
## Date: 18th October 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################    

library(dsHelper)

conns <- datashield.login(logindata, restore = "mh_traj")
################################################################################
# 1. MISSING DATA  
################################################################################


################################################################################
# 2. MODELS  
################################################################################
################################################################################
# Internalising  
################################################################################
age.vars <-  c("age_c", "age_c_m_2", "age_c_m_1", "age_c_m_0_5", "age_c_log", 
              "age_c_0_5", "age_c_2", "age_c_3")

## ---- Make formulae ----------------------------------------------------------
int.mod <- dh.makeLmerForm(
  outcome = "int_t_z", 
  id_var = "child_id_int", 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num", 
  age_vars = age.vars,
  random = "intercept")

## ---- Run models -------------------------------------------------------------
int.fit <- dh.lmeMultPoly(
  df = "analysis_df_l",
  formulae = int.mod$formula, 
  poly_names = int.mod$polys, 
  conns = conns[int.coh])

save.image()

ds.colnames("analysis_df_l")
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
# Additional adjustments 
################################################################################
################################################################################
# Define models  
################################################################################
conns <- datashield.login(logindata, restore = "mhtraj_15")

eth_coh <- c("alspac", "genr", "inma", "raine")

sens.mod <- fit.tab %>%
  separate(
    col = model, 
    into = c("poly_1", "poly_2"), 
    sep = ",") %>%
  mutate(outcome_var = c(
    "int_pc_", "ext_pc_", "adhd_pc_", "asd_pc_", "lan_pc_", "nvi_pc_")) %>%
  mutate(
    form = paste(
      paste0(outcome_var, "~1"), 
      poly_1, poly_2, "edu_rank_num",
      paste0("edu_rank_num*", poly_1), paste0("edu_rank_num*", poly_2), 
      "(1|child_id_int)", sep = "+"),
    eth_form = paste(
      paste0(outcome_var, "~1"), 
      poly_1, poly_2, "edu_rank_num",
      paste0("edu_rank_num*", poly_1), paste0("edu_rank_num*", poly_2), 
      "eth_bin", "(1|child_id_int)", sep = "+")) %>%
  dplyr::select(outcome, poly_1, poly_2, form, eth_form) %>%
  mutate(
    cohort =  list(int_coh, ext_coh, adhd_coh, asd_coh, lan_coh, nvi_coh),
    eth_coh = list(
    int_coh[int_coh %in% eth_coh], 
    ext_coh[ext_coh %in% eth_coh],  
    adhd_coh[adhd_coh %in% eth_coh],  
    asd_coh[asd_coh %in% eth_coh],  
    lan_coh[lan_coh %in% eth_coh],  
    nvi_coh[nvi_coh %in% eth_coh]))

#restrict to cohorts with ethnicity information. Think about whether we need 
#to rerun original model on restricted sample. Maybe start by not?

################################################################################
# Ethnicity analysis  
################################################################################
eth.fit <- sens.mod %>%
  dplyr::filter(outcome != "ASD") %>%
  pmap(function(eth_form, eth_coh, ...){
    
    ds.lmerSLMA(
      dataName = "analysis_df_l",
      formula = eth_form,
      datasources = conns[unlist(cohort)])
    
  })

################################################################################
# Additional adjustment for child sex  
################################################################################

## ---- Males ------------------------------------------------------------------
males.fit <- sens.mod %>%
  pmap(function(form, cohort, ...){
    
    ds.lmerSLMA(
      dataName = "analysis_df_l_m",
      formula = form,
      datasources = conns[unlist(cohort)])
    
  })

## ---- Females ----------------------------------------------------------------
females.fit <- sens.mod %>%
  pmap(function(form, cohort, ...){
    
    ds.lmerSLMA(
      dataName = "analysis_df_l_f",
      formula = form,
      datasources = conns[unlist(cohort)])
    
  })

save.image()




eth.mod$cohort



)

sex.mod$cohort

sex.mod$eth_form

int.fit$fit

"int_pc_~1+int_age__m1+int_age__m_2+edu_rank_num+edu_rank_num*int_age_+edu_rank_num*int_age__m_2+(1|child_id_int)"


fit.tab$model

int.mod$formula[1]








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


