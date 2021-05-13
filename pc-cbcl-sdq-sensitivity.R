################################################################################
## Project: wp6 trajectories
## Script purpose: Compare percentiles vs raw scores
## Date: 6th May 2021
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
# 1. Sort out dataset  
################################################################################
coh_sens_pc <- c("alspac", "dnbc", "moba", "raine")
coh_sens_cbcl <- c("moba", "raine")
coh_sens_sdq <- c("alspac", "dnbc")

# Raine actually includes two measures: cbcl and ysr. I want to drop the ysr
# for now
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$ext_instr_", 
  V2.name = "61",
  Boolean.operator = "!=",
  keep.NAs = FALSE,
  newobj = "analysis_df", 
  datasources = conns[coh_sens_pc]
)

ds.summary("ext_cbcl_sub$ext_age_", datasources = conns["raine"])
ds.summary("analysis_df$ext_age_", datasources = conns["raine"])


################################################################################
# 1. Linear models  
################################################################################

## ---- Percentiles ------------------------------------------------------------
sens_pc.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + sex + ext_age_ + sex*ext_age_ + (1|child_id_int)",
  datasources = conns[coh_sens_pc])

## ---- SDQ --------------------------------------------------------------------
sens_cbcl.fit <- ds.lmerSLMA(
  dataName = "ext_cbcl_sub",
  formula = "ext_raw_ ~ 1 + sex + ext_age_ + sex*ext_age_ + (1|child_id_int)",
  datasources = conns[coh_sens_cbcl])

## ---- CBCL -------------------------------------------------------------------
sens_sdq.fit <- ds.lmerSLMA(
  dataName = "ext_sdq_sub",
  formula = "ext_raw_ ~ 1 + sex + ext_age_ + sex*ext_age_ + (1|child_id_int)",
  datasources = conns[coh_sens_sdq])


################################################################################
# 2. Get reference table for available ages  
################################################################################
ages_pc <- dh.getStats(
  df = "analysis_df",
  vars = "ext_age_",
  conns = conns[coh_sens_pc]
)

ages_cbcl <- dh.getStats(
  df = "ext_cbcl_sub",
  vars = "ext_age_",
  conns = conns[coh_sens_cbcl]
)

ages_sdq <- dh.getStats(
  df = "ext_sdq_sub",
  vars = "ext_age_",
  conns = conns[coh_sens_sdq]
)

################################################################################
# 3. Predicted values percentiles  
################################################################################
sens_pc.tab <- dh.lmTab(
  model = sens_pc.fit, 
  type = "lmer",
  coh_names = coh_sens_pc,
  direction = "long", 
  ci_format = "separate") %>%
  filter(coefficient == "est") %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  dplyr::rename("sex_age" = "sex2:ext_age_")


## ---- Predicted values across these reference ages ---------------------------
sens_pc.pred <- sens_pc.tab %>%
  pmap(
    function(intercept, cohort, sex2, ext_age_, sex_age, ...) {
      
      pred_m <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        sex = 0,
        predicted = intercept + sex*sex2 + age*ext_age_ + sex*age*sex_age
      )
      
      pred_f <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        sex = 1,
        predicted = intercept + sex*sex2 + age*ext_age_ + sex*age*sex_age
      )
      
      pred <- bind_rows(pred_m, pred_f)
      
      return(pred)
    }
  ) %>% bind_rows() 

age_min_max_sens_pc <- ages_pc$continuous %>% 
  select(cohort_ref = cohort, perc_5, perc_95) %>%
  mutate(perc_95 = ifelse(cohort_ref == "combined", 17.1, perc_95))

sens_pc.pred <- age_min_max_sens_pc %>%
  pmap(function(cohort_ref, perc_5, perc_95, ...){
    
    sens_pc.pred %>%
      filter(cohort == cohort_ref & between(age, perc_5, perc_95))
  }) %>% bind_rows()

## ---- Now we get standard errors ---------------------------------------------
sens_pc.pred_coh <- sens_pc.pred %>% 
  filter(cohort != "combined") %>%
  mutate(study_ref = case_when(
    cohort == "alspac" ~ "study1",
    cohort == "dnbc" ~ "study2",
    cohort == "moba" ~ "study3",
    cohort == "raine" ~ "study4"))

# First we do for cohorts separately
sens_pc.pred_coh_se <- sens_pc.pred_coh %>%
  pmap(function(study_ref, sex, ext_age_, ...){
    
    vcov <- sens_pc.fit$output.summary[[study_ref]]$vcov
    C <- c(1, sex, age, sex*age)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

sens_pc.pred <- sens_pc.pred_coh %>% 
  mutate(se = sens_pc.pred_coh_se) %>%
  mutate(low_ci = predicted - 1.96*se, 
         upper_ci = predicted + 1.96*se)


################################################################################
# 4. Predicted values CBCL 
################################################################################
sens_cbcl.tab <- dh.lmTab(
  model = sens_cbcl.fit, 
  type = "lmer",
  coh_names = coh_sens_cbcl,
  direction = "long", 
  ci_format = "separate") %>%
  filter(coefficient == "est") %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  dplyr::rename("sex_age" = "sex2:ext_age_")


## ---- Predicted values across these reference ages ---------------------------
sens_cbcl.pred <- sens_cbcl.tab %>%
  pmap(
    function(intercept, cohort, sex2, ext_age_, sex_age, ...) {
      
      pred_m <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        sex = 0,
        predicted = intercept + sex*sex2 + age*ext_age_ + sex*age*sex_age
      )
      
      pred_f <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        sex = 1,
        predicted = intercept + sex*sex2 + age*ext_age_ + sex*age*sex_age
      )
      
      pred <- bind_rows(pred_m, pred_f)
      
      return(pred)
    }
  ) %>% bind_rows() 

age_min_max_sens_cbcl <- ages_cbcl$continuous %>% 
  select(cohort_ref = cohort, perc_5, perc_95) %>%
  mutate(perc_95 = ifelse(cohort_ref == "combined", 17.1, perc_95))

sens_cbcl.pred <- age_min_max_sens_cbcl %>%
  pmap(function(cohort_ref, perc_5, perc_95, ...){
    
    sens_cbcl.pred %>%
      filter(cohort == cohort_ref & between(age, perc_5, perc_95))
  }) %>% bind_rows()

## ---- Now we get standard errors ---------------------------------------------
sens_cbcl.pred_coh <- sens_cbcl.pred %>% 
  filter(cohort != "combined") %>%
  mutate(study_ref = case_when(
    cohort == "moba" ~ "study1",
    cohort == "raine" ~ "study2"))

# First we do for cohorts separately
sens_cbcl.pred_coh_se <- sens_cbcl.pred_coh %>%
  pmap(function(study_ref, sex, ext_age_, ...){
    
    vcov <- sens_cbcl.fit$output.summary[[study_ref]]$vcov
    C <- c(1, sex, age, sex*age)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

sens_cbcl.pred <- sens_cbcl.pred_coh %>% 
  mutate(se = sens_cbcl.pred_coh_se) %>%
  mutate(low_ci = predicted - 1.96*se, 
         upper_ci = predicted + 1.96*se)


################################################################################
# 5. Predicted values SDQ  
################################################################################
sens_sdq.tab <- dh.lmTab(
  model = sens_sdq.fit, 
  type = "lmer",
  coh_names = coh_sens_sdq,
  direction = "long", 
  ci_format = "separate") %>%
  filter(coefficient == "est") %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  dplyr::rename("sex_age" = "sex2:ext_age_")


## ---- Predicted values across these reference ages ---------------------------
sens_sdq.pred <- sens_sdq.tab %>%
  pmap(
    function(intercept, cohort, sex2, ext_age_, sex_age, ...) {
      
      pred_m <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        sex = 0,
        predicted = intercept + sex*sex2 + age*ext_age_ + sex*age*sex_age
      )
      
      pred_f <- tibble(
        age = seq(1, 18, by = 0.01),
        cohort = cohort,
        sex = 1,
        predicted = intercept + sex*sex2 + age*ext_age_ + sex*age*sex_age
      )
      
      pred <- bind_rows(pred_m, pred_f)
      
      return(pred)
    }
  ) %>% bind_rows() 

age_min_max_sens_sdq <- ages_sdq$continuous %>% 
  select(cohort_ref = cohort, perc_5, perc_95) %>%
  mutate(perc_95 = ifelse(cohort_ref == "combined", 17.1, perc_95))

sens_sdq.pred <- age_min_max_sens_sdq %>%
  pmap(function(cohort_ref, perc_5, perc_95, ...){
    
    sens_sdq.pred %>%
      filter(cohort == cohort_ref & between(age, perc_5, perc_95))
  }) %>% bind_rows()

## ---- Now we get standard errors ---------------------------------------------
sens_sdq.pred_coh <- sens_sdq.pred %>% 
  filter(cohort != "combined") %>%
  mutate(study_ref = case_when(
    cohort == "alspac" ~ "study1",
    cohort == "dnbc" ~ "study2"))

# First we do for cohorts separately
sens_sdq.pred_coh_se <- sens_sdq.pred_coh %>%
  pmap(function(study_ref, sex, ext_age_, ...){
    
    vcov <- sens_sdq.fit$output.summary[[study_ref]]$vcov
    C <- c(1, sex, age, sex*age)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist

sens_sdq.pred <- sens_sdq.pred_coh %>% 
  mutate(se = sens_sdq.pred_coh_se) %>%
  mutate(low_ci = predicted - 1.96*se, 
         upper_ci = predicted + 1.96*se)

################################################################################
# 6. Prepare data for plotting  
################################################################################
pc_labs <- c("ALSPAC", "DNBC", "MoBa", "Raine")

sens_pc.plotdata <- sens_pc.pred %>% mutate(type = "Percentiles")
sens_cbcl.plotdata <- sens_cbcl.pred %>% mutate(type = "Raw CBCL")
sens_sdq.plotdata <- sens_sdq.pred %>% mutate(type = "Raw SDQ")

sens.plotdata <- bind_rows(sens_pc.plotdata, sens_cbcl.plotdata, sens_sdq.plotdata) %>%
  mutate(
    cohort = case_when(
      cohort == "alspac" ~ "ALSPAC",
      cohort == "dnbc" ~ "DNBC",
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine"),
    sex = factor(sex, labels = c("Male", "Female"), ordered = TRUE), 
    cohort = factor(cohort, labels = pc_labs, levels = pc_labs, ordered = TRUE), 
    ymin = case_when(
      type == "Percentiles" ~ 20,
      type == "Raw CBCL" ~ 0,
      type == "Raw SDQ" ~ 0),
    ymax = case_when(
      type == "Percentiles" ~ 60,
      type == "Raw CBCL" ~ 30,
      type == "Raw SDQ" ~ 15)
    )
    

################################################################################
# 7. ALSPAC plots  
################################################################################
sens_alspac <- sens.plotdata %>% filter(cohort == "ALSPAC")

sens_alspac.plot <- ggplot() + 
  geom_line(data = sens_alspac, aes(x = age, y = predicted, colour = sex), size = 0.8) +
  geom_ribbon(data = sens_alspac %>% filter(sex == "Male"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  geom_ribbon(data = sens_alspac %>% filter(sex == "Female"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  theme_jn +
  xlab("Child age") +
  ylab("Predicted percentile score") +
  labs(colour = "Child sex") + 
  scale_colour_manual(values = ineq_palette) + 
  geom_blank(data = sens_alspac, aes(y = ymin)) +
  geom_blank(data = sens_alspac, aes(y = ymax)) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 4), expand = c(0, 0))

################################################################################
# 8. DNBC plots  
################################################################################
sens_dnbc <- sens.plotdata %>% filter(cohort == "DNBC")

sens_dnbc.plot <- ggplot() + 
  geom_line(data = sens_dnbc, aes(x = age, y = predicted, colour = sex), size = 0.8) +
  geom_ribbon(data = sens_dnbc %>% filter(sex == "Male"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  geom_ribbon(data = sens_dnbc %>% filter(sex == "Female"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  theme_jn +
  xlab("Child age") +
  ylab("Predicted percentile score") +
  labs(colour = "Child sex") + 
  scale_colour_manual(values = ineq_palette) + 
  geom_blank(data = sens_dnbc, aes(y = ymin)) +
  geom_blank(data = sens_dnbc, aes(y = ymax)) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 4), expand = c(0, 0))


################################################################################
# 9. MoBa  
################################################################################
sens_moba <- sens.plotdata %>% filter(cohort == "MoBa")

sens_moba.plot <- ggplot() + 
  geom_line(data = sens_moba, aes(x = age, y = predicted, colour = sex), size = 0.8) +
  geom_ribbon(data = sens_moba %>% filter(sex == "Male"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  geom_ribbon(data = sens_moba %>% filter(sex == "Female"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  theme_jn +
  xlab("Child age") +
  ylab("Predicted percentile score") +
  labs(colour = "Child sex") + 
  scale_colour_manual(values = ineq_palette) + 
  geom_blank(data = sens_moba, aes(y = ymin)) +
  geom_blank(data = sens_moba, aes(y = ymax)) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 4), expand = c(0, 0))


################################################################################
# 10. Raine  
################################################################################
sens_raine <- sens.plotdata %>% filter(cohort == "Raine")

sens_raine.plot <- ggplot() + 
  geom_line(data = sens_raine, aes(x = age, y = predicted, colour = sex), size = 0.8) +
  geom_ribbon(data = sens_raine %>% filter(sex == "Male"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  geom_ribbon(data = sens_raine %>% filter(sex == "Female"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  theme_jn +
  xlab("Child age") +
  ylab("Predicted percentile score") +
  labs(colour = "Child sex") + 
  scale_colour_manual(values = ineq_palette) + 
  geom_blank(data = sens_raine, aes(y = ymin)) +
  geom_blank(data = sens_raine, aes(y = ymax)) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 4), expand = c(0, 0))

################################################################################
# 11. Save plots  
################################################################################
ggsave(
  filename="./figures/sens/alspac.png", 
  plot = sens_alspac.plot,
  h = 12, w = 20, units="cm", dpi=1200,
  device="png")

ggsave(
  filename="./figures/sens/dnbc.png", 
  plot = sens_dnbc.plot,
  h = 12, w = 20, units="cm", dpi=1200,
  device="png")

ggsave(
  filename="./figures/sens/moba.png", 
  plot = sens_moba.plot,
  h = 12, w = 20, units="cm", dpi=1200,
  device="png")

ggsave(
  filename="./figures/sens/raine.png", 
  plot = sens_raine.plot,
  h = 12, w = 20, units="cm", dpi=1200,
  device="png")
