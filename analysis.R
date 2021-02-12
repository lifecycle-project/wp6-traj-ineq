################################################################################
## Project: wp6-traj-ineq
## Script purpose: Test MLM and write related functions 
## Date: 30th September 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(DSI)
library(DSOpal)
library(dsBaseClient)
library(tidyr)
library(dsHelper)
library(dplyr)
library(purrr)
library(stringr)
library(magrittr)

################################################################################
# 1. Load workspace  
################################################################################
conns <- datashield.login(logindata, restore = "mhtraj_10")

ds.summary("analysis_df")
################################################################################
# 2. Linear models  
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_lin.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + edu_rank_num + ext_age_ + edu_rank_num*ext_age_ + 
            (1|child_id_int)",
  datasources = conns[ext_coh])


## ---- Internalising ----------------------------------------------------------
int_lin.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "int_pc_ ~ 1 + edu_rank_num + int_age + edu_rank_num*int_age + 
  (1|child_id_int)",
  datasources = conns[int_coh])


################################################################################
# Non-linear models  
################################################################################
################################################################################
# 3. Make formulas
################################################################################

## ---- Internalising ----------------------------------------------------------
int_form <- dh.makeLmerForm(
  outcome = "int_pc_", 
  idvar = "child_id_int", 
  agevars = c("int_age_", "int_age_m_2", "int_age_m_1", "int_age_m_0_5", 
              "int_age_log", "int_age_0_5", "int_age_2", "int_age_3"),
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num"
)  

## ---- Externalising ----------------------------------------------------------
ext_form <- dh.makeLmerForm(
  outcome = "ext_pc_", 
  idvar = "child_id_int", 
  agevars = c("ext_age_", "ext_age_m_2", "ext_age_m_1", "ext_age_m_0_5", 
              "ext_age_log", "ext_age_0_5", "ext_age_2", "ext_age_3"),
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num"
)  

################################################################################
# 4. Run models  
################################################################################
ext_coh_tmp <- ext_coh[which(ext_coh != "moba")]
int_coh_tmp <- int_coh[which(int_coh != "moba")]

ext_poly_in_moba <- ext_poly.fit

## ---- Externalising ----------------------------------------------------------
ext_poly.fit <- dh.lmeMultPoly(
  df = "analysis_df", 
  formulae = ext_form, 
  conns = conns[ext_coh])

## ---- Internalising ----------------------------------------------------------
int_poly.fit <- dh.lmeMultPoly(
  df = "analysis_df", 
  formulae = int_form,
  conns = conns[int_coh])

# Raine didn't converge. Improve the function later to solve this, but in the
# meantime just redo model that works

int_best.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "int_pc_ ~ 1 + int_age_m_2 + int_age_m_1 + edu_rank_num +
edu_rank_num*int_age_m_2 + edu_rank_num*int_age_m_1 + (1|child_id_int)",
  datasources = conns[int_coh])


################################################################################
# 5. Coefficients for best-fitting models  
################################################################################

## ---- Choose best fitting model ----------------------------------------------
ext_best.fit <- ext_poly.fit$models[[which(ext_poly.fit$fit$log_rank == 1)]]
#int_best.fit <- int_poly.fit$models[[which(int_poly.fit$fit$log_rank == 1)]]


## ---- Make table of coefficients ---------------------------------------------
ext_best.tab <- dh.lmerTab(ext_best.fit)
colnames(ext_best.tab) <- c("coefficient", ext_coh, "combined")

int_best.tab <- dh.lmerTab(int_best.fit)
colnames(int_best.tab) <- c("coefficient", int_coh_tmp, "combined")


################################################################################
# 6. Predicted values  
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_best.pred <- dh.predPoly(
  df = "analysis_df",
  fixed = "edu_rank_num",
  core_agevar = "ext_age_",
  coeftab = ext_best.tab,
  conns = conns[ext_coh], 
  type = "nonlinear")

ds.colnames("analysis_df", datasources = conns[ext_coh])
## ---- Internalising ----------------------------------------------------------
int_best.pred <- dh.predPoly(
  df = "analysis_df",
  fixed = "edu_rank_num",
  core_agevar = "int_age_",
  coeftab = int_best.tab,
  conns = conns[int_coh_tmp], 
  type = "nonlinear")


###############################################################################
# 7. Plot full models  
################################################################################

## ---- Externalising ----------------------------------------------------------
ggplot() + 
  geom_line(data = ext_best.pred$full_model, aes(x = age, y = predicted, colour = factor)) +
  facet_wrap(~cohort) +
  scale_x_continuous(limit = c(2, 18), breaks = seq(2, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(20, 80), breaks = seq(20, 80, 20), expand = c(0, 0))

## ---- Internalising ----------------------------------------------------------
ggplot() + 
  geom_line(data = int_best.pred$full_mode, aes(x = age, y = predicted, colour = factor)) +
  facet_wrap(~cohort)  +
  scale_x_continuous(limit = c(2, 18), breaks = seq(2, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(20, 80), breaks = seq(20, 80, 20), expand = c(0, 0))


################################################################################
# 8. Plot SII  
################################################################################

## ---- Externalising ----------------------------------------------------------
ggplot() + 
  geom_line(data = ext_best.pred$sii, aes(x = age, y = sii, colour = cohort)) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-100, 100), breaks = seq(-100, 100, 20), expand = c(0, 0))


## ---- Internalising ----------------------------------------------------------
ggplot() + 
  geom_line(data = int_best.pred$sii, aes(x = age, y = sii, colour = cohort)) +
  scale_x_continuous(limit = c(2, 18), breaks = seq(2, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-100, 100), breaks = seq(-100, 100, 50), expand = c(0, 0))


################################################################################
# 9. Abstract  
################################################################################

## ---- Cohort ages ------------------------------------------------------------
ages_min_max$continuous %>%
  filter(cohort != "Combined") %>%
  group_by(cohort) %>%
  slice(1) %>%
  select(perc_5, perc_95)
  
## ---- Cohort numbers ---------------------------------------------------------
included_n <- paste0("study", c(1:4)) %>%
  map(function(x){

    ext_best.fit$output.summary[[x]]$ngrps    
    
  })

names(included_n) <- ext_coh


## ---- Marginal means and standard errors for each cohort ---------------------
results <- bind_rows(
  ext_best.pred$sii %>% filter(cohort == "chop" & age == 5.48),
  ext_best.pred$sii %>% filter(cohort == "inma" & age == 7.71),
  ext_best.pred$sii %>% filter(cohort == "dnbc" & age == 7),
  ext_best.pred$sii %>% filter(cohort == "raine" & age == 5)) %>%
  mutate(
    study = paste0("study", seq(1, 4, 1)),
    age_m_2 = age^-2,
    age_m_1 = age^-1
  )
  
se_vec <- results %>%
  pmap(function(age, study, age_m_2, age_m_1, ...){
    
    vcov <- ext_best.fit$output.summary[[study]]$vcov
    C <- c(0, 0, 0, 1, age_m_2, age_m_1)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

results %<>% mutate(
  se = se_vec, 
  low_ci = sii - 1.96*se, 
  upper_ci = sii + 1.96*se) %>%
  select(cohort, age, sii, low_ci, upper_ci, everything())


## ---- Marginal means in the combined model -----------------------------------

## Weighted average of vcov matrix
overall_n <- sum(unlist(included_n))

vcov_weight <- paste0("study", c(1:4)) %>%
  map(function(x){
    
    ext_best.fit$output.summary[[x]]$ngrps/overall_n*ext_best.fit$output.summary[[x]]$vcov    
    
  })

vcov_av <- vcov_weight[[1]] + vcov_weight[[2]] + vcov_weight[[3]] + vcov_weight[[4]]
ext_best.fit$output.summary[["study4"]]$vcov    


combined <- bind_rows(
  ext_best.pred$sii %>% filter(cohort == "combined" & age == 7),
  ext_best.pred$sii %>% filter(cohort == "combined" & age == 10)) %>%
  mutate(age_m_2 = age^-2,
         age_m_1 = age^-1)



se_combined <- combined %>%
  pmap(function(age_m_2, age_m_1, ...){
    
    C <- c(0, 0, 0, 1, age_m_2, age_m_1)
    std.er <- sqrt(t(C) %*% vcov_av %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

combined %<>% mutate(
  se = se_combined, 
  low_ci = sii - 1.96*se, 
  upper_ci = sii + 1.96*se) %>%
  select(cohort, age, sii, low_ci, upper_ci, everything())


ext_best.pred


metafor package in r: predicted values + standard errors



str(ext_best.fit[[1]]$study1)

str(ext_best.fit[[1]]$study1$varcor$child_id_int)

data.frame(ext_best.fit[[1]]$study1$varcor$child_id_int)



################################################################################
# CONFERENCE PRESENTATION
################################################################################
################################################################################
# Age only models  
################################################################################
################################################################################
# 1. Make formulas
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_form <- dh.makeLmerForm(
  outcome = "ext_raw_", 
  idvar = "child_id_int", 
  agevars = c("ext_age_", "ext_age_m_2", "ext_age_m_1", "ext_age_m_0_5", 
              "ext_age_log", "ext_age_0_5", "ext_age_2", "ext_age_3"), 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num")


## ---- Internalising ----------------------------------------------------------
int_form <- dh.makeLmerForm(
  outcome = "int_raw_", 
  idvar = "child_id_int", 
  agevars = c("int_age_", "int_age_m_2", "int_age_m_1", "int_age_m_0_5", 
              "int_age_log", "int_age_0_5", "int_age_2", "int_age_3"),
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num"
)  



################################################################################
# 2. Fit all polynomials  
################################################################################

## First we find out what the best fitting model is

## ---- Externalising CBCL -----------------------------------------------------
ext_cbcl_poly.fit <- dh.lmeMultPoly(
  df = "ext_cbcl_sub", 
  formulae = ext_form, 
  conns = conns[ext_cbcl_coh])


## ---- Externalising SDQ ------------------------------------------------------
ext_sdq_poly.fit <- dh.lmeMultPoly(
  df = "ext_sdq_sub", 
  formulae = ext_form, 
  conns = conns[ext_sdq_coh])


## ---- Internalising CBCL -----------------------------------------------------
int_cbcl_poly.fit <- dh.lmeMultPoly(
  df = "int_cbcl_sub", 
  formulae = int_form,
  conns = conns[int_cbcl_coh])


## ---- Internalising SDQ ------------------------------------------------------
int_sdq_poly.fit <- dh.lmeMultPoly(
  df = "int_sdq_sub", 
  formulae = int_form,
  conns = conns[int_sdq_coh])


################################################################################
# Summarise best fitting models  
################################################################################

## ---- CBCL -------------------------------------------------------------------

## This is simple as all done in datashield
ext_cbcl_poly.fit$models[[which(ext_cbcl_poly.fit$fit$log_rank == 1)]]
int_cbcl_poly.fit$models[[which(int_cbcl_poly.fit$fit$log_rank == 1)]]


## ---- SDQ --------------------------------------------------------------------

## First load the .csv files with info from the ALSPAC models


## Now combine with the fit tables for DNBC

ext_sdq_poly.fit$fit
int_sdq_poly.fit$fit



ext_simp.tab <- dh.lmerTab(ext_simp.fit)
colnames(ext_simp.tab) <- c("coefficient", ext_coh, "combined")



################################################################################
# Age only model
################################################################################

## ---- Fit model --------------------------------------------------------------
ext_simp.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_raw_ ~ 1 + ext_age_m_2 + ext_age_m_1 + (1|child_id_int)",
  datasources = conns[ext_coh])


## ---- Get coefficients -------------------------------------------------------
ext_simp.tab <- dh.lmerTab(ext_simp.fit)
colnames(ext_simp.tab) <- c("coefficient", ext_coh, "combined")

## ---- Get predicted values ---------------------------------------------------




ext_raw.pred <- dh.predPoly(
  df = "analysis_df",
  fixed = "edu_rank_num",
  core_agevar = "ext_age_",
  coeftab = ext_simp.tab,
  conns = conns[ext_coh], 
  type = "nonlinear")

################################################################################
# 1. Compare trajectories using only one measure to those using combination  
################################################################################

## ---- Create a CBCL subset ---------------------------------------------------


## ---- Fit model with raw scores ----------------------------------------------
ext_raw.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_raw_ ~ 1 + ext_age_m_2 + ext_age_m_1 + edu_rank_num +
  edu_rank_num*ext_age_m_2 + edu_rank_num*ext_age_m_1 + (1|child_id_int)",
  datasources = conns[ext_coh])


## ---- Get coefficients -------------------------------------------------------
ext_raw.tab <- dh.lmerTab(ext_raw.fit)
colnames(ext_raw.tab) <- c("coefficient", ext_coh, "combined")

## ---- Get predicted values ---------------------------------------------------
ext_raw.pred <- dh.predPoly(
  df = "analysis_df",
  fixed = "edu_rank_num",
  core_agevar = "ext_age_",
  coeftab = ext_raw.tab,
  conns = conns[ext_coh], 
  type = "nonlinear")


## ---- Plot full model --------------------------------------------------------
ggplot() + 
  geom_line(data = ext_raw.pred$full_model, aes(x = age, y = predicted, colour = factor)) +
  facet_wrap(~cohort) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(0, 30), breaks = seq(0, 30, 5), expand = c(0, 0))

## ---- Plot SII ---------------------------------------------------------------
ggplot() + 
  geom_line(data = ext_raw.pred$sii, aes(x = age, y = sii, colour = cohort)) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-20, 20), breaks = seq(-20, 20, 5), expand = c(0, 0)) +
  facet_wrap(~cohort)

## ---- Plot RII ---------------------------------------------------------------
ggplot() + 
  geom_line(data = ext_raw.pred$rii, aes(x = age, y = rii, colour = cohort)) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(0, 2), breaks = seq(0, 2, 0.5), expand = c(0, 0)) +
  facet_wrap(~cohort)


################################################################################
# 2. Internalising using raw scores  
################################################################################


## Trim extremes. Maybe for now just present SII for simplicity?
## Look at income and area deprivation as exposures? One more day for core analysis
## then begin making slides
## Confidence intervals around plots would be nice, though not sure I can do this
## with the combined



## For some reason the only cohorts it works with are raine and moba

## ---- Fit main effect models using percentiles -------------------------------
ext_pc.fit <- ds.lmerSLMA(
  dataName = "cbcl_sub",
  formula = "ext_pc_ ~ 1 + ext_age_m_2 + ext_age_m_1 + (1|child_id_int)", 
  datasources = conns[cbcl_coh])

pc_pred <- tibble(
  age = seq(4, 10, 0.01),
  pred = 44.21 + (-20.56*(age^-2)) + (15.27*(age^-1)),
  type = "percentile"
)

## ---- Fit main effect models using raw scores --------------------------------
ext_raw.fit <- ds.lmerSLMA(
  dataName = "cbcl_sub",
  formula = "ext_raw_ ~ 1 + ext_age_m_2 + ext_age_m_1 + (1|child_id_int)", 
  datasources = conns[cbcl_coh])

raw_pred <- tibble(
  age = seq(4, 10, 0.01),
  pred = 5.60 + (-78.10*(age^-2)) + (57.44*(age^-1)),
  type = "raw"
)

test_pred <- bind_rows(pc_pred, raw_pred)

library(ggplot2)

ggplot() + 
  geom_line(data = pc_pred, aes(x = age, y = pred)) +
  scale_x_continuous(limit = c(2, 18), breaks = seq(2, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(0, 100), breaks = seq(-100, 100, 50), expand = c(0, 0))

ggplot() + 
  geom_line(data = raw_pred, aes(x = age, y = pred)) +
  scale_x_continuous(limit = c(2, 18), breaks = seq(2, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(0, 100), breaks = seq(-100, 100, 50), expand = c(0, 0))


plot()


raw_pred <- tibble(
  
  
)


ds.scatterPlot(
  x = "cbcl_sub$ext_age_", 
  y = "cbcl_sub$ext_pc_", 
  datasources = conns[cbcl_coh])



ds.summary("cbcl_sub", datasources = conns[cbcl_coh])

ds.colnames("analysis_df")

ext_coh_mult <- c("")



+ edu_rank_num +
edu_rank_num*int_age_m_2 + edu_rank_num*int_age_m_1 
  

## ---- Identify cohorts using mixture of measures -----------------------------





## Also, try doing this with ALSPAC data and ALSPAC LC data - compare eg for
## 3 people of different categories of maternal education what trajectories
## look like
################################################################################
# 2. Compare percentiles to raw scores in terms of variability  
################################################################################

raw_perc <- dh.getStats(
  df = "analysis_df", 
  vars = c("ext_raw_", "ext_pc_"),
  conns = conns
)

raw_perc$continuous

ds.level("analysis_df$ext_raw_")
ds.table("analysis_df$ext_pc_")


################################################################################
# 3. Compare trajectories using income instead of education  
################################################################################




