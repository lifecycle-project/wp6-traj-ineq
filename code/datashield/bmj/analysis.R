################################################################################
## Project: traj-inequality
## Script purpose: Analysis for manuscript
## Date: 18th October 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################    

library(dsHelper)

conns <- datashield.login(logindata, restore = "mhtraj_9")
################################################################################
# ADHD  
################################################################################

## ---- Make formulae ----------------------------------------------------------
adhd.form <- dh.makeLmerForm(
  outcome = "adhd_pc_", 
  idvar = "child_id_int", 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num", 
  agevars = c("adhd_age_", "adhd_age_m_2", "adhd_age_m_1", "adhd_age_m_0_5", 
              "logadhd_age_", "adhd_age_0_5", "adhd_age_2", "adhd_age_3"), 
  random = "intercept")

## ---- Run models -------------------------------------------------------------
adhd_coh_rev <-  adhd_coh[!adhd_coh %in% c("ninfea", "elfe", "rhea")]
## Convergence problems with these cohorts - will address later                        

adhd.fit <- dh.lmeMultPoly(
  df = "analysis_df",
  formulae = adhd.form$formulae, 
  poly_names = adhd.form$polys, 
  conns = conns[adhd_coh_rev])

################################################################################
# ASC  
################################################################################

## ---- Make formulae ----------------------------------------------------------
asd.form <- dh.makeLmerForm(
  outcome = "asd_pc_", 
  idvar = "child_id_int", 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num", 
  agevars = c("asd_age_", "asd_age_m_2", "asd_age_m_1", "asd_age_m_0_5", 
              "logasd_age_", "asd_age_0_5", "asd_age_2", "asd_age_3"), 
  random = "intercept")

## ---- Run models -------------------------------------------------------------
asd_coh_rev <-  asd_coh[!asd_coh %in% c("elfe", "inma")]
## Convergence problems with these cohorts - will address later                        

asd.fit <- dh.lmeMultPoly(
  df = "analysis_df",
  formulae = asd.form$formulae, 
  poly_names = asd.form$polys, 
  conns = conns["moba"])

################################################################################
# Language  
################################################################################
lan_coh_rev <- lan_coh[lan_coh %in% c("rhea", "inma")]

## ---- Make formulae ----------------------------------------------------------
lan.form <- dh.makeLmerForm(
  outcome = "lan_pc_", 
  idvar = "child_id_int", 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num", 
  agevars = c("lan_age_", "lan_age_m_2", "lan_age_m_1", "lan_age_m_0_5", 
              "loglan_age_", "lan_age_0_5", "lan_age_2", "lan_age_3"), 
  random = "intercept")

## ---- Run models -------------------------------------------------------------
lan.fit <- dh.lmeMultPoly(
  df = "analysis_df",
  formulae = lan.form$formulae, 
  poly_names = lan.form$polys, 
  conns = conns[lan_coh_rev])

################################################################################
# NVI  
################################################################################

## ---- Make formulae ----------------------------------------------------------
nvi.form <- dh.makeLmerForm(
  outcome = "nvi_pc_", 
  idvar = "child_id_int", 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num", 
  agevars = c("nvi_age_", "nvi_age_m_2", "nvi_age_m_1", "nvi_age_m_0_5", 
              "lognvi_age_", "nvi_age_0_5", "nvi_age_2", "nvi_age_3"), 
  random = "intercept")

## ---- Run models -------------------------------------------------------------
nvi.fit <- dh.lmeMultPoly(
  df = "analysis_df",
  formulae = nvi.form$formulae, 
  poly_names = nvi.form$polys, 
  conns = conns[nvi_coh])


################################################################################
# Final models  
################################################################################



