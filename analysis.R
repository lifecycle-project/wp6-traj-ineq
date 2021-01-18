################################################################################
## Project: wp6-traj-ineq
## Script purpose: Test MLM and write related functions 
## Date: 30th September 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

################################################################################
# 1. Load workspace  
################################################################################
conns <- datashield.login(logindata, restore = "mhtraj_7")


################################################################################
# 2. Linear models  
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_lin.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + edu_rank_num + ext_age + edu_rank_num*ext_age + 
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
  conns = conns[ext_coh_tmp])

## ---- Internalising ----------------------------------------------------------
int_poly.fit <- dh.lmeMultPoly(
  df = "analysis_df", 
  formulae = int_form,
  conns = conns[int_coh_tmp])

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
colnames(ext_best.tab) <- c("coefficient", ext_coh_tmp, "combined")

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
  conns = conns[ext_coh_tmp], 
  type = "nonlinear")

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


str(ext_best.fit)

## ---- Marginal means and standard errors -------------------------------------
results <- bind_rows(
  ext_best.pred$sii %>% filter(cohort == "chop" & age == 5.48),
  ext_best.pred$sii %>% filter(cohort == "inma" & age == 7.71),
  ext_best.pred$sii %>% filter(cohort == "dnbc" & age == 7),
  ext_best.pred$sii %>% filter(cohort == "raine" & age == 2.05)) %>%
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


## ---- Predicted means in the combined model ----------------------------------
combined <- bind_rows(
  ext_best.pred$sii %>% filter(cohort == "combined" & age == 5),
  ext_best.pred$sii %>% filter(cohort == "combined" & age == 10),
  ext_best.pred$sii %>% filter(cohort == "combined" & age == 15))


