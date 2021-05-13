################################################################################
## Project: johanna-analysis
## Script purpose: Simple trajectories for WP6.1
## Date: 26.04.21
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################    

1. Internalising trajectories SDQ
2. Internalising trajectories CBCL
3. Externalising trajectories SDQ
4. Externalising trajecotories CBCL
5. Binary exposure with raw scores vs percentiles


library(DSI)
library(DSOpal)
library(dsBaseClient)
library(tidyr)
library(dsHelper)
library(dplyr)
library(purrr)
library(stringr)
library(magrittr)
library(ggplot2)

source('~/useful-code-r/code/themes/forest-theme.R')

conns <- datashield.login(logindata, restore = "mhtraj_10")

################################################################################
# 1. View data by measure  
################################################################################

## ---- CBCL -------------------------------------------------------------------
ds.scatterPlot(
  x = "ext_cbcl_sub$ext_age_", 
  y = "ext_cbcl_sub$ext_raw_", 
  datasources = conns[ext_cbcl_tmp])

## ---- SDQ --------------------------------------------------------------------
ds.scatterPlot(
  x = "ext_sdq_sub$ext_age_", 
  y = "ext_sdq_sub$ext_raw_", 
  datasources = conns[ext_sdq_tmp])

## ---- Define cohort vectors --------------------------------------------------
cbcl_nl <- c("moba", "raine")
cbcl_l <- ("inma")

sdq_nl <- c("alspac")
sdq_l <- c("chop", "dnbc")

################################################################################
# 2. Linear models  
################################################################################
sdq_lin.fit <- ds.lmerSLMA(
  dataName = "ext_sdq_sub",
  formula = "ext_raw_ ~ 1 + ext_age_ + sex + ext_age_*sex + (1|child_id_int)",
  datasources = conns[sdq_l])

################################################################################
# 3. Make polynomial models
################################################################################
raw_form <- dh.makeLmerForm(
  outcome = "ext_raw_", 
  idvar = "child_id_int", 
  agevars = c("ext_age_", "ext_age_m_2", "ext_age_m_1", "ext_age_m_0_5", 
              "ext_age_log", "ext_age_0_5", "ext_age_2", "ext_age_3"))

################################################################################
# 3. Find best-fitting model   
################################################################################

## ---- CBCL -------------------------------------------------------------------
cbcl_nl.fit <- dh.lmeMultPoly(
  df = "ext_cbcl_sub", 
  formulae = raw_form, 
  conns = conns[cbcl_nl])

## ---- SDQ --------------------------------------------------------------------
sdq_nl.fit <- dh.lmeMultPoly(
  df = "ext_sdq_sub", 
  formulae = raw_form,
  conns = conns[sdq_nl])

ext_cbcl.fit$fit %>% arrange(av_rank)
ext_sdq.fit$fit %>% arrange(av_rank)

################################################################################
# 4. Rerun best fitting models with sex as covariate  
################################################################################

## ---- CBCL -------------------------------------------------------------------
cbcl_best.fit <- ds.lmerSLMA(
  dataName = "ext_cbcl_sub",
  formula = "ext_raw_ ~ 1 + ext_age_m_0_5 + ext_age_0_5 + sex + 
             sex*ext_age_m_0_5 + sex*ext_age_0_5 + (1|child_id_int)",
  datasources = conns[cbcl_nl])

## ---- SDQ --------------------------------------------------------------------
sdq_best.fit <- ds.lmerSLMA(
  dataName = "ext_sdq_sub",
  formula = "ext_raw_ ~ 1 + ext_age_m_2 + ext_age_m_1 + sex +
             sex*ext_age_m_2 + sex*ext_age_m_1 + (1|child_id_int)",
  datasources = conns[sdq_nl])


################################################################################
# 5. Get reference table for available ages  
################################################################################
ages_sdq_l <- dh.getStats(
  df = "ext_sdq_sub",
  vars = "ext_age_",
  conns = conns[sdq_l]
)

ages_cbcl_nl <- dh.getStats(
  df = "ext_cbcl_sub",
  vars = "ext_age_",
  conns = conns[cbcl_nl]
)

ages_sdq_nl <- dh.getStats(
  df = "ext_sdq_sub",
  vars = "ext_age_",
  conns = conns[sdq_nl]
)

################################################################################
# 7. SDQ linear predicted values  
################################################################################
sdq_lin.tab <- dh.lmTab(
  model = sdq_lin.fit, 
  type = "lmer",
  coh_names = sdq_l,
  direction = "long", 
  ci_format = "separate") %>%
  filter(coefficient == "est") %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  dplyr::rename("ext_age_sex" = "ext_age_:sex2")
    

## ---- Predicted values across these reference ages ---------------------------
sdq_lin.pred <- sdq_lin.tab %>%
  pmap(
    function(intercept, cohort, ext_age_, sex2, ext_age_sex, ...) {
      
      pred_m <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        sex = 0,
        predicted = intercept + age*ext_age_ + sex*sex2 + sex*age*ext_age_sex
      )
      
      pred_f <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        sex = 1,
        predicted = intercept + age*ext_age_ + sex*sex2 + sex*age*ext_age_sex
      )
      
      pred <- bind_rows(pred_m, pred_f)
      
      return(pred)
    }
  ) %>% bind_rows() 

age_min_max_sdq_l <- ages_sdq_l$continuous %>% 
  select(cohort_ref = cohort, perc_5, perc_95) %>%
  mutate(perc_95 = ifelse(cohort_ref == "combined", 17.1, perc_95))

sdq_lin.pred <- age_min_max_sdq_l %>%
  pmap(function(cohort_ref, perc_5, perc_95, ...){
    
    sdq_lin.pred %>%
      filter(cohort == cohort_ref & between(age, perc_5, perc_95))
  }) %>% bind_rows()

## ---- Now we get standard errors ---------------------------------------------
sdq_lin_pred_coh <- sdq_lin.pred %>% 
  filter(cohort != "combined") %>%
  mutate(study_ref = case_when(
    cohort == "chop" ~ "study1",
    cohort == "dnbc" ~ "study2"))
    
# First we do for cohorts separately
sdq_lin_pred_coh_se <- sdq_lin_pred_coh %>%
  pmap(function(study_ref, sex, age, ...){
    
    vcov <- sdq_lin.fit$output.summary[[study_ref]]$vcov
    C <- c(1, age, sex, sex*age)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

sdq_lin.pred <- sdq_lin_pred_coh %>% 
  mutate(se = sdq_lin_pred_coh_se) %>%
  mutate(low_ci = predicted - 1.96*se, 
         upper_ci = predicted + 1.96*se)

################################################################################
# 8. CBCL non-linear predicted values  
################################################################################
cbcl_nl.tab <- dh.lmTab(
  model = cbcl_best.fit, 
  type = "lmer",
  coh_names = cbcl_nl,
  direction = "long", 
  ci_format = "separate") %>%
  filter(coefficient == "est") %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  dplyr::rename(
    "ext_age_0_5_sex" = 'ext_age_0_5:sex2', 
    "ext_age_m_0_5_sex" = 'ext_age_m_0_5:sex2')


## ---- Predicted values across these reference ages ---------------------------
cbcl_nl.pred <- cbcl_nl.tab %>%
  pmap(
    function(cohort, intercept, ext_age_0_5, ext_age_0_5_sex, ext_age_m_0_5, 
             ext_age_m_0_5_sex, sex2, ...) {
      
      pred_m <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        age_m_0_5 = age^-0.5,
        age_0_5 = age^0.5,
        sex = 0,
        predicted = intercept + age_m_0_5*ext_age_m_0_5 + age_0_5*ext_age_0_5 +
          sex*sex2 + age_m_0_5*sex*ext_age_m_0_5_sex + age_0_5*sex*ext_age_0_5_sex
      )
      
      pred_f <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        age_m_0_5 = age^-0.5,
        age_0_5 = age^0.5,
        sex = 1,
        sex_m_0_5 = age_m_0_5*ext_age_m_0_5_sex,
        sex_0_5 = age_0_5*ext_age_0_5_sex,
        predicted = intercept + age_m_0_5*ext_age_m_0_5 + age_0_5*ext_age_0_5 +
          sex*sex2 + age_m_0_5*sex*ext_age_m_0_5_sex + age_0_5*sex*ext_age_0_5_sex
      )
      
        pred <- bind_rows(pred_m, pred_f)
        
      return(pred)
    }
  ) %>% bind_rows()

## ---- Reference ages ---------------------------------------------------------
age_cbcl_nl_min_max <- ages_cbcl_nl$continuous %>% 
  select(cohort_ref = cohort, perc_5, perc_95) %>%
  mutate(perc_95 = ifelse(cohort_ref == "combined", 17.1, perc_95))

cbcl_nl.pred <- age_cbcl_nl_min_max %>%
  filter(cohort_ref %in% c(cbcl_nl, "combined")) %>%
  pmap(function(cohort_ref, perc_5, perc_95, ...){
    
    cbcl_nl.pred %>%
      filter(cohort == cohort_ref & between(age, perc_5, perc_95))
  }) %>% bind_rows()


## ---- Standard errors --------------------------------------------------------
cbcl_nl.pred_coh <- cbcl_nl.pred %>% 
  filter(cohort != "combined") %>%
  mutate(study_ref = case_when(
    cohort == "moba" ~ "study1", 
    cohort == "raine" ~ "study2"))

# First we do for cohorts separately
cbcl_nl.pred_coh_se <- cbcl_nl.pred_coh %>%
  pmap(function(study_ref, age_0_5, age_m_0_5, sex, ...){
    
    vcov <- cbcl_best.fit$output.summary[[study_ref]]$vcov
    C <- c(1, age_m_0_5, age_0_5, sex, age_m_0_5*sex, age_0_5*sex)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

cbcl.pred <- cbcl_nl.pred_coh %<>% 
  mutate(se = cbcl_nl.pred_coh_se) %>%
  mutate(low_ci = predicted - 1.96*se, 
         upper_ci = predicted + 1.96*se)

################################################################################
# 9. SDQ non-linear predicted values  
################################################################################
sdq_nl.tab <- dh.lmTab(
  model = sdq_best.fit, 
  type = "lmer",
  coh_names = sdq_nl,
  direction = "long", 
  ci_format = "separate") %>%
  filter(coefficient == "est") %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  dplyr::rename(
    "ext_age_m_2_sex" = 'ext_age_m_2:sex2', 
    "ext_age_m_1_sex" = 'ext_age_m_1:sex2')


## ---- Predicted values across these reference ages ---------------------------
sdq_nl.pred <- sdq_nl.tab %>%
  pmap(
    function(cohort, intercept, ext_age_m_2, ext_age_m_2_sex, ext_age_m_1, 
             ext_age_m_1_sex, sex2, ...) {
      
      pred_m <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        age_m_2 = age^-2,
        age_m_1 = age^-1,
        sex = 0,
        predicted = intercept + age_m_2*ext_age_m_2 + age_m_1*ext_age_m_1 +
          sex*sex2 + sex*age_m_2*ext_age_m_2_sex + sex*age_m_1*ext_age_m_1_sex
      )
      
      pred_f <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        age_m_2 = age^-2,
        age_m_1 = age^-1,
        sex = 1,
        predicted = intercept + age_m_2*ext_age_m_2 + age_m_1*ext_age_m_1 +
          sex*sex2 + sex*age_m_2*ext_age_m_2_sex + sex*age_m_1*ext_age_m_1_sex
      )
      
      pred <- bind_rows(pred_m, pred_f)
      
      return(pred)
    }
  ) %>% bind_rows()

## ---- Reference ages ---------------------------------------------------------
age_sdq_nl_min_max <- ages_sdq_nl$continuous %>% 
  select(cohort_ref = cohort, perc_5, perc_95) %>%
  mutate(perc_95 = ifelse(cohort_ref == "combined", 17.1, perc_95))

sdq_nl.pred <- age_sdq_nl_min_max %>%
  filter(cohort_ref %in% c(sdq_nl, "combined")) %>%
  pmap(function(cohort_ref, perc_5, perc_95, ...){
    
    sdq_nl.pred %>%
      filter(cohort == cohort_ref & between(age, perc_5, perc_95))
  }) %>% bind_rows()


## ---- Standard errors --------------------------------------------------------
sdq_nl.pred_coh <- sdq_nl.pred %>% 
  filter(cohort != "combined") %>%
  mutate(study_ref = case_when(
    cohort == "alspac" ~ "study1"))

# First we do for cohorts separately
sdq_nl.pred_coh_se <- sdq_nl.pred_coh %>%
  pmap(function(study_ref, age_m_2, age_m_1, sex, ...){
    
    vcov <- sdq_best.fit$output.summary[[study_ref]]$vcov
    C <- c(1, age_m_2, age_m_1, sex, age_m_2*sex, age_m_1*sex)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

sdq_nl.pred <- sdq_nl.pred_coh %<>% 
  mutate(se = sdq.pred_coh_se) %>%
  mutate(low_ci = predicted - 1.96*se, 
         upper_ci = predicted + 1.96*se)

sdq.pred <- bind_rows(sdq_lin.pred, sdq_nl.pred)

