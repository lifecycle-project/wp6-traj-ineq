################################################################################
## Project: wp6-traj-ineq
## Script purpose: Analysis for LS conference presentation
## Date: 17th February 2021
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
library(metafor)
library(readr)
library(ggplot2)

source('~/useful-code-r/code/themes/forest-theme.R')

conns <- datashield.login(logindata, restore = "mhtraj_10")

################################################################################
# 1. MATERNAL EDUCATION STATS
################################################################################

## ---- Get stats --------------------------------------------------------------
mat_ed_stats <- dh.getStats(
  df = "analysis_df", 
  vars = "edu_m", 
  conns = conns[coh_lin]
)

################################################################################
# 2. AVAILABLE DATA AT DIFFERENT TIME POINTS
################################################################################
################################################################################
# Create subets by age band
################################################################################

## ---- CHOP -------------------------------------------------------------------
dsHelper::dh.makeOutcome(
  df = "analysis_df", 
  outcome = "ext_pc_", 
  age_var = "ext_age_", 
  bands = c(2, 6, 7, 9, 10, 13), 
  mult_action = "earliest",
  conns = conns["chop"]
)

datashield.workspace_save(conns, "mhtraj_12a")
conns <- datashield.login(logindata, restore = "mhtraj_12a")

## ---- DNBC -------------------------------------------------------------------
dsHelper::dh.makeOutcome(
  df = "analysis_df", 
  outcome = "ext_pc_", 
  age_var = "ext_age_", 
  bands = c(5, 8, 8, 14), 
  mult_action = "earliest",
  conns = conns["dnbc"]
)

datashield.workspace_save(conns, "mhtraj_12b")
conns <- datashield.login(logindata, restore = "mhtraj_12b")

## ---- INMA -------------------------------------------------------------------
dsHelper::dh.makeOutcome(
  df = "analysis_df", 
  outcome = "ext_pc_", 
  age_var = "ext_age_", 
  bands = c(7, 10, 10, 12), 
  mult_action = "earliest",
  conns = conns["inma"]
)

datashield.workspace_save(conns, "mhtraj_12c")
conns <- datashield.login(logindata, restore = "mhtraj_12c")

## ---- MoBa -------------------------------------------------------------------
dsHelper::dh.makeOutcome(
  df = "analysis_df", 
  outcome = "ext_pc_", 
  age_var = "ext_age_", 
  bands = c(0, 2, 2, 4, 4, 7), 
  mult_action = "earliest",
  conns = conns["moba"]
)

datashield.workspace_save(conns, "mhtraj_12d")
conns <- datashield.login(logindata, restore = "mhtraj_12d")

## ---- Raine ------------------------------------------------------------------
dsHelper::dh.makeOutcome(
  df = "analysis_df", 
  outcome = "ext_pc_", 
  age_var = "ext_age_", 
  bands = c(0, 5, 5, 7, 7, 10, 10, 12, 12, 15, 15, 18), 
  mult_action = "earliest",
  conns = conns["raine"]
)

datashield.workspace_save(conns, "mhtraj_12e")
conns <- datashield.login(logindata, restore = "mhtraj_12e")

################################################################################
# 3. LINEAR MODELS
################################################################################
################################################################################
# Run models   
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_lin.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + edu_rank_num + ext_age_ + sex + edu_rank_num*ext_age_ +
  agebirth_m_y + (1|child_id_int)",
  datasources = conns[coh_lin])

ds.summary("analysis_df$agebirth_m_y")

## ---- Internalising ----------------------------------------------------------
int_lin.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "int_pc_ ~ 1 + edu_rank_num + int_age_ + sex + edu_rank_num*int_age_ +
  agebirth_m_y + (1|child_id_int)",
  datasources = conns[coh_lin])

################################################################################
# Get reference table for available ages  
################################################################################
ages_ref <- dh.getStats(
  df = "analysis_df",
  vars = "ext_age_",
  conns = conns[coh_lin]
)

################################################################################
# Externalising predicted values
################################################################################
ext_lin.tab <- dh.lmTab(
  model = ext_lin.fit, 
  type = "lmer",
  coh_names = coh_lin,
  direction = "long", 
  ci_format = "separate") %>%
  filter(coefficient == "est") %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  dplyr::rename("ext_age_ed" = `edu_rank_num:ext_age_`)

## ---- Predicted values across these reference ages ---------------------------
ext_lin.pred <- ext_lin.tab %>%
  pmap(
    function(cohort, edu_rank_num, ext_age_ed, ...) {
      
      pred <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        sii = edu_rank_num + age*ext_age_ed
      )
      
      return(pred)
    }
  ) %>% bind_rows() 

age_min_max <- ages_ref$continuous %>% 
  select(cohort_ref = cohort, perc_5, perc_95) %>%
  mutate(perc_95 = ifelse(cohort_ref == "combined", 17.1, perc_95))

ext_lin.pred <- age_min_max %>%
  pmap(function(cohort_ref, perc_5, perc_95, ...){
    
ext_lin.pred %>%
      filter(cohort == cohort_ref & between(age, perc_5, perc_95))
  }) %>% bind_rows()

## ---- Now we get standard errors ---------------------------------------------
ext_lin_pred_coh <- ext_lin.pred %>% 
  filter(cohort != "combined") %>%
  mutate(study_ref = case_when(
    cohort == "alspac" ~ "study1",
    cohort == "chop" ~ "study2",
    cohort == "dnbc" ~ "study3",
    cohort == "inma" ~ "study4",
    cohort == "moba" ~ "study5", 
    cohort == "raine" ~ "study6"))

# First we do for cohorts separately
ext_lin_pred_coh_se <- ext_lin_pred_coh %>%
  pmap(function(study_ref, age, ...){
    
    vcov <- ext_lin.fit$output.summary[[study_ref]]$vcov
    C <- c(0, 1, 0, 0, age)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

ext_lin_pred_coh %<>% 
  mutate(se = ext_lin_pred_coh_se) %>%
  mutate(low_ci = sii - 1.96*se, 
         upper_ci = sii + 1.96*se)

ext_lin_comb.pred <- ext_lin.pred %>%
  filter(cohort == 'combined') %>%
  mutate(low_ci = sii, upper_ci = sii)

ext_lin_plot.pred <- bind_rows(ext_lin_pred_coh, ext_lin_comb.pred)

################################################################################
# Internalising predicted values  
################################################################################
int_lin.tab <- dh.lmTab(
  model = int_lin.fit, 
  type = "lmer",
  coh_names = coh_lin,
  direction = "long", 
  ci_format = "separate") %>%
  filter(coefficient == "est") %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  dplyr::rename("int_age_ed" = `edu_rank_num:int_age_`)

## ---- Predicted values across these reference ages ---------------------------
int_lin.pred <- int_lin.tab %>%
  pmap(
    function(cohort, edu_rank_num, int_age_ed, ...) {
      
      pred <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        sii = edu_rank_num + age*int_age_ed
      )
      
      return(pred)
    }
  ) %>% bind_rows() 

age_min_max <- ages_ref$continuous %>% 
  select(cohort_ref = cohort, perc_5, perc_95) %>%
  mutate(perc_95 = ifelse(cohort_ref == "combined", 17.1, perc_95))

int_lin.pred <- age_min_max %>%
  pmap(function(cohort_ref, perc_5, perc_95, ...){
    
    int_lin.pred %>%
      filter(cohort == cohort_ref & between(age, perc_5, perc_95))
  }) %>% bind_rows()

## ---- Now we get standard errors ---------------------------------------------
int_lin_pred_coh <- int_lin.pred %>% 
  filter(cohort != "combined") %>%
  mutate(study_ref = case_when(
    cohort == "alspac" ~ "study1",
    cohort == "chop" ~ "study2",
    cohort == "dnbc" ~ "study3",
    cohort == "inma" ~ "study4",
    cohort == "moba" ~ "study5", 
    cohort == "raine" ~ "study6"))

# First we do for cohorts separately
int_lin_pred_coh_se <- int_lin_pred_coh %>%
  pmap(function(study_ref, age, ...){
    
    vcov <- int_lin.fit$output.summary[[study_ref]]$vcov
    C <- c(0, 1, 0, 0, age)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

int_lin_pred_coh %<>% 
  mutate(se = int_lin_pred_coh_se) %>%
  mutate(low_ci = sii - 1.96*se, 
         upper_ci = sii + 1.96*se) %>%
  bind_rows(., )

int_lin_comb.pred <- int_lin.pred %>%
  filter(cohort == 'combined') %>%
  mutate(low_ci = sii, upper_ci = sii)

int_lin_plot.pred <- bind_rows(int_lin_pred_coh, int_lin_comb.pred)

################################################################################
# 4. NON-LINEAR MODELS  
################################################################################
conns <- datashield.login(logindata, restore = "mhtraj_10")

################################################################################
# Make formulas
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_form_pc <- dh.makeLmerForm(
  outcome = "ext_pc_", 
  idvar = "child_id_int", 
  agevars = c("ext_age_", "ext_age_m_2", "ext_age_m_1", "ext_age_m_0_5", 
              "ext_age_log", "ext_age_0_5", "ext_age_2", "ext_age_3"), 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num")


## ---- Internalising ----------------------------------------------------------
int_form_pc <- dh.makeLmerForm(
  outcome = "int_pc_", 
  idvar = "child_id_int", 
  agevars = c("int_age_", "int_age_m_2", "int_age_m_1", "int_age_m_0_5", 
              "int_age_log", "int_age_0_5", "int_age_2", "int_age_3"),
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num"
)  

################################################################################
# Run models  
################################################################################
nl_coh <- c("alspac", "chop", "moba", "raine")

## ---- Externalising ----------------------------------------------------------
ext_poly_pc.fit <- dh.lmeMultPoly(
  df = "analysis_df", 
  formulae = ext_form_pc, 
  conns = conns[nl_coh])

## ---- Internalising ----------------------------------------------------------
int_poly_pc.fit <- dh.lmeMultPoly(
  df = "analysis_df", 
  formulae = int_form_pc[-12, ],
  conns = conns[nl_coh])

## note one model wouldn't converge

################################################################################
# Display best fitting model  
################################################################################
ext_poly_pc.fit$models[[which.min(ext_poly_pc.fit$fit$av_rank)]]
int_poly_pc.fit$models[[which.min(int_poly_pc.fit$fit$av_rank)]]

ext_poly_pc.fit$fit %>% arrange(av_rank)
int_poly_pc.fit$fit %>% arrange(av_rank)


################################################################################
# Rerun best fitting models  
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_pc.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + ext_age_m_1 + ext_age_m_0_5 + edu_rank_num + sex + 
  edu_rank_num*ext_age_m_1 + edu_rank_num*ext_age_m_0_5 + (1|child_id_int)",
  datasources = conns[nl_coh])

## ---- Internalising ----------------------------------------------------------
int_pc.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "int_pc_ ~ 1 + int_age_m_2 + int_age_m_1 + edu_rank_num + sex + 
  edu_rank_num*int_age_m_2 + edu_rank_num*int_age_m_1 + (1|child_id_int)",
  datasources = conns[nl_coh])


################################################################################
# Non-linear predicted values: externalising
################################################################################
ext_nl.tab <-  dh.lmTab(
  model = ext_pc.fit, 
  type = "lmer",
  coh_names = nl_coh,
  direction = "long", 
  ci_format = "separate") %>%
  filter(coefficient == "est") %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  dplyr::rename(
    "ext_age_m_1_ed" = 'ext_age_m_1:edu_rank_num', 
    "ext_age_m_0_5_ed" = 'ext_age_m_0_5:edu_rank_num')
  

## ---- Predicted values across these reference ages ---------------------------
ext_nl.pred <- ext_nl.tab %>%
  pmap(
    function(cohort, ext_age_m_1_ed, ext_age_m_0_5_ed, edu_rank_num, ...) {
      
      pred <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        age_m_1 = age^-1,
        age_m_0_5 = age^-0.5,
        sii = edu_rank_num + age_m_1*ext_age_m_1_ed + age_m_0_5*ext_age_m_0_5_ed
      )
      
      return(pred)
    }
  ) %>% bind_rows()

## ---- Reference ages ---------------------------------------------------------
age_min_max <- ages_ref$continuous %>% 
  select(cohort_ref = cohort, perc_5, perc_95) %>%
  mutate(perc_95 = ifelse(cohort_ref == "combined", 17.1, perc_95))

ext_nl.pred <- age_min_max %>%
  filter(cohort_ref %in% c("alspac", "chop", "moba", "raine", "combined")) %>%
  pmap(function(cohort_ref, perc_5, perc_95, ...){
    
    ext_nl.pred %>%
      filter(cohort == cohort_ref & between(age, perc_5, perc_95))
  }) %>% bind_rows()


## ---- Standard errors --------------------------------------------------------
ext_nl_pred_coh <- ext_nl.pred %>% 
  filter(cohort != "combined") %>%
  mutate(study_ref = case_when(
    cohort == "alspac" ~ "study1", 
    cohort == "chop" ~ "study2", 
    cohort == "moba" ~ "study3", 
    cohort == "raine" ~ "study4"))

# First we do for cohorts separately
ext_nl_pred_coh_se <- ext_nl_pred_coh %>%
  pmap(function(study_ref, age_m_1, age_m_0_5, ...){
    
    vcov <- ext_pc.fit$output.summary[[study_ref]]$vcov
    C <- c(0, 0, 0, 1, 0, age_m_1, age_m_0_5)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

ext_nl_pred_coh %<>% 
  mutate(se = ext_nl_pred_coh_se) %>%
  mutate(low_ci = sii - 1.96*se, 
         upper_ci = sii + 1.96*se)

ext_nl_comb.pred <- ext_nl.pred %>%
  filter(cohort == 'combined') %>%
  mutate(low_ci = sii, upper_ci = sii)

ext_nl_plot.pred <- bind_rows(ext_nl_pred_coh, ext_nl_comb.pred)

################################################################################
# Non-linear predicted values: internalising
################################################################################
int_nl.tab <-  dh.lmTab(
  model = int_pc.fit, 
  type = "lmer",
  coh_names = nl_coh,
  direction = "long", 
  ci_format = "separate") %>%
  filter(coefficient == "est") %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  dplyr::rename(
    "int_age_m_2_ed" = 'int_age_m_2:edu_rank_num', 
    "int_age_m_1_ed" = 'int_age_m_1:edu_rank_num')

## ---- Predicted values across these reference ages ---------------------------
int_nl.pred <- int_nl.tab %>%
  pmap(
    function(cohort, int_age_m_2_ed, int_age_m_1_ed, edu_rank_num, ...) {
      
      pred <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        age_m_2 = age^-2,
        age_m_1 = age^-1,
        sii = edu_rank_num + age_m_2*int_age_m_2_ed + age_m_1*int_age_m_1_ed
      )
      
      return(pred)
    }
  ) %>% bind_rows()

## ---- Reference ages ---------------------------------------------------------
age_min_max <- ages_ref$continuous %>% 
  select(cohort_ref = cohort, perc_5, perc_95) %>%
  mutate(perc_95 = ifelse(cohort_ref == "combined", 17.1, perc_95))

int_nl.pred <- age_min_max %>%
  filter(cohort_ref %in% c("alspac", "chop", "moba", "raine", "combined")) %>%
  pmap(function(cohort_ref, perc_5, perc_95, ...){
    
    int_nl.pred %>%
      filter(cohort == cohort_ref & between(age, perc_5, perc_95))
  }) %>% bind_rows()


## ---- Standard errors --------------------------------------------------------
int_nl_pred_coh <- int_nl.pred %>% 
  filter(cohort != "combined") %>%
  mutate(study_ref = case_when(
    cohort == "alspac" ~ "study1"
    cohort == "chop" ~ "study2", 
    cohort == "moba" ~ "study3", 
    cohort == "raine" ~ "study4"))

# First we do for cohorts separately
int_nl_pred_coh_se <- int_nl_pred_coh %>%
  pmap(function(study_ref, age_m_2, age_m_1, ...){
    
    vcov <- int_pc.fit$output.summary[[study_ref]]$vcov
    C <- c(0, 0, 0, 1, 0, age_m_2, age_m_1)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

int_nl_pred_coh %<>% 
  mutate(se = int_nl_pred_coh_se) %>%
  mutate(low_ci = sii - 1.96*se, 
         upper_ci = sii + 1.96*se)

int_nl_comb.pred <- int_nl.pred %>%
  filter(cohort == 'combined') %>%
  mutate(low_ci = sii, upper_ci = sii)

int_nl_plot.pred <- bind_rows(int_nl_pred_coh, int_nl_comb.pred)







