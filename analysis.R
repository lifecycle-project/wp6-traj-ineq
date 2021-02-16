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
library(metafor)
library(readr)

ls("package:dsBaseClient")
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

conns <- datashield.login(logindata, restore = "mhtraj_10")

################################################################################
# 1. Make formulas
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
# 2. Run models  
################################################################################
ext_pc_coh <- c("dnbc", "moba", "raine")
int_pc_coh <- c("moba", "raine")

## ---- Externalising ----------------------------------------------------------
ext_poly_pc.fit <- dh.lmeMultPoly(
  df = "analysis_df", 
  formulae = ext_form_pc, 
  conns = conns[ext_pc_coh])

## ---- Internalising ----------------------------------------------------------
int_poly_pc.fit <- dh.lmeMultPoly(
  df = "analysis_df", 
  formulae = int_form_pc,
  conns = conns[int_pc_coh])


################################################################################
# 3. Display best fitting model  
################################################################################
ext_poly_pc.fit$models[[which.min(ext_poly_pc.fit$fit$av_rank)]]
int_poly_pc.fit$models[[which.min(int_poly_pc.fit$fit$av_rank)]]

ext_poly_pc.fit$fit %>% arrange(av_rank)
int_poly_pc.fit$fit %>% arrange(av_rank)


################################################################################
# 4. Explicitly run best fitting models  
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_pc.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + ext_age_m_1 + ext_age_m_0_5 + edu_rank_num + sex + 
  edu_rank_num*ext_age_m_1 + edu_rank_num*ext_age_m_0_5 + (1|child_id_int)",
  datasources = conns[ext_pc_coh])


## ---- Internalising ----------------------------------------------------------
int_pc.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "int_pc_ ~ 1 + int_age_m_2 + int_age_m_1 + edu_rank_num + sex + 
  edu_rank_num*int_age_m_2 + edu_rank_num*int_age_m_1 + (1|child_id_int)",
  datasources = conns[int_pc_coh])


################################################################################
# 5. Coefficients for best-fitting models  
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_pc.tab <- dh.lmerTab(ext_pc.fit)
colnames(ext_pc.tab) <- c("coefficient", ext_pc_coh, "combined")

## ---- Internalising ----------------------------------------------------------
int_pc.tab <- dh.lmerTab(int_pc.fit)
colnames(int_pc.tab) <- c("coefficient", int_pc_coh, "combined")


################################################################################
# 6. Function to get ages for prediction
################################################################################
dh.predRefFull <- function(df, core_agevar, coeftab, fixed, conns){
  
  
  ## ---- Make factor version of rank variable -----------------------------------
  fix_fac <- paste0(fixed, "_fac")
  
  ds.asFactor(paste0(df, "$", fixed), newobj = fix_fac, datasources = conns)
  ds.cbind(c(df, fix_fac), newobj = df, datasources = conns)
  
  ## ---- Get age info for each cohort -------------------------------------------
  ages_ref <- dh.getStats(
    df = df,
    vars = c(core_agevar, fix_fac),
    conns = conns
  )
  
  coefs <- as_tibble(cbind(cohort = names(coeftab), t(coeftab))) %>%
    filter(cohort != "coefficient")
  
  ages <- ages_ref$continuous %>%
    select(cohort, perc_5, perc_95) %>%
    filter(cohort != "combined")
  
  comb_ages <- tibble(
    cohort = "combined",
    perc_5 = min(ages$perc_5, na.rm = TRUE),
    perc_95 = max(ages$perc_95, na.rm = TRUE)
  )
  
  fixed <- ages_ref$categorical %>%
    filter(value != 0 & cohort != "combined") %>%
    group_by(cohort) %>%
    mutate(fact_levels = paste(category, collapse = ",")) %>%
    slice(1) %>%
    select(cohort, fact_levels)
  
  ages <- ages_ref$continuous %>%
    select(cohort, perc_5, perc_95) %>%
    filter(cohort != "combined")
  
  comb_ages <- tibble(
    cohort = "combined",
    perc_5 = min(ages$perc_5, na.rm = TRUE),
    perc_95 = max(ages$perc_95, na.rm = TRUE)
  )
  
  ages <- bind_rows(ages, comb_ages)
  
  predref <- left_join(coefs, fixed, by = "cohort") %>%
    left_join(., ages, by = "cohort")
  
  colnames(predref) <- c("cohort", "intercept", coeftab$coefficient[2:5], 
                         "poly_1_ed", "poly_2_ed", "fact_levels", "low_age", "high_age")
  
  predref %<>% mutate_at(vars(intercept, coeftab$coefficient[2:5], poly_1_ed, poly_2_ed), as.numeric)
  
  
  return(predref)
}


################################################################################
# 7. Calculate SII   
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_pc_ref <- dh.predRefFull(
  df = "analysis_df",
  core_agevar = "ext_age_",
  coeftab = ext_pc.tab,
  fixed = "edu_rank_num",
  conns = conns[ext_pc_coh])

ext_pc.pred <- ext_pc_ref %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_1 = age^-1,
        age_m_0_5 = age^-0.5,
        sii = edu_rank_num + age_m_1*poly_1_ed + age_m_0_5*poly_2_ed
      )
      
      return(pred)
    }
  )

ext_pc.pred <- bind_rows(ext_pc.pred) 

## ---- Internalising ----------------------------------------------------------
int_pc_ref <- dh.predRefFull(
  df = "analysis_df",
  core_agevar = "int_age_",
  coeftab = int_pc.tab,
  fixed = "edu_rank_num",
  conns = conns[int_pc_coh])

int_pc.pred <- int_pc_ref %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_2 = age^-2,
        age_m_1 = age^-1,
        sii = edu_rank_num + age_m_2*poly_1_ed + age_m_1*poly_2_ed
      )
      
      return(pred)
    }
  )

int_pc.pred <- bind_rows(int_pc.pred) 


################################################################################
# 8. Plot SII  
################################################################################

## ---- Theme ------------------------------------------------------------------
theme_traj <- theme(
  plot.background = element_rect(fill =scales::alpha("#CCCCCC", 0.3)),  #Background outside of plot
  panel.background = element_rect(fill="white"),  #Background for plot
  panel.grid.major=element_line(colour="grey"), #Major and minor gridlines
  panel.grid.minor=element_line(colour="white"), 
  panel.spacing = unit(1, "lines"),
  plot.title = element_text(hjust = 0.5, vjust=0, size=16, face="bold"), #Plot title, thought don't tend to use
  text=element_text(size=9), #General text 
  axis.title.y = element_text(size=14, margin = margin(t = 0, r = 10, b = 0, l = 0)), #Axis labels
  axis.title.x = element_text(size=14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.text.x = element_text(size=11, margin = margin(t = 4, r=0, b=0, l=0), colour="black"), #Axis text
  axis.text.y = element_text(size=11, margin = margin(t = 0, r=4, b=0, l=0), colour="black"),
  axis.ticks.length=unit(0.3, "cm"),
  axis.ticks = element_line(colour = "grey"),
  strip.text.x = element_text(size=11),
  strip.background = element_blank(),
  legend.background= element_rect(fill=scales::alpha("#CCCCCC", 0.03)), #Legend background colour
  legend.title=element_text(size=8, face="bold"), #Legend title
  legend.text=element_text(size=8), #Legend text
  legend.position="right", #Legend position
  legend.direction="vertical", #Legend stacking
  legend.justification = "left", #Left justify legend
  legend.key.width = unit(3, "line"), #Make amount of line displayed in legend longer
  legend.margin=margin(t=0.2, r=0, b=0.2, l=0, unit="cm"), #Margin around legend
  plot.margin = unit(c(0.5, 0.5, 0.2, 0.5),"cm"),
  panel.grid.minor.y=element_blank(),
  panel.grid.major.y=element_blank())
  

palette_ext <- c(rep("#005690", 3), "#ff2600")
palette_int <- c(rep("#005690", 2), "#ff2600")


## ---- Arrange cohorts better -------------------------------------------------
ext_labs <- c("DNBC", "MoBa", "Raine", "Combined")

ext_pc.pred %<>%
  mutate(
    cohort = case_when(
      cohort == "dnbc" ~ "DNBC",
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined")) %>%
  mutate(
    cohort = factor(cohort, labels = ext_labs, levels = ext_labs, ordered = TRUE)
  )

int_pc.pred %<>%
  mutate(
    cohort = case_when(
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined")) %>%
  mutate(
    cohort = factor(cohort, labels = ext_labs, levels = ext_labs, ordered = TRUE)
  )
   

## ---- Externalising ----------------------------------------------------------
ext_pc.plot <- ggplot() + 
  geom_line(data = ext_pc.pred, aes(x = age, y = sii, colour = cohort)) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 40), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  geom_hline(yintercept = -20, alpha = 0.05) +
  scale_colour_manual(values = palette_ext)


## ---- Internalising ----------------------------------------------------------
int_pc.plot <- ggplot() + 
  geom_line(data = int_pc.pred, aes(x = age, y = sii, colour = cohort)) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 40), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  geom_hline(yintercept = -20, alpha = 0.05) +
  scale_colour_manual(values = palette_int)


## ---- Save plots --------------------------------------------------------------
ggsave(
  filename="./figures/ext_pc.png", 
  plot = ext_pc.plot,
  h = 18, w = 25, units="cm", dpi=1200,
  device="png")

ggsave(
  filename="./figures/int_pc.png", 
  plot = int_pc.plot,
  h = 25, w = 15.92, units="cm", dpi=1200,
  device="png")




################################################################################
# 9. Repeat analyses stratified by sex  
################################################################################

## Hard to formally test for this within datashield, at least without doing a
## lot of work. Let's do it by eye to start

################################################################################
# Create subsets  
################################################################################

## ---- Males ------------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$sex", 
  V2.name = "1",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "analysis_df_m")

## ---- Females ----------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$sex", 
  V2.name = "2",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "analysis_df_f")


################################################################################
# Revised models 
################################################################################

## ---- Externalising males ----------------------------------------------------
ext_pc_m.fit <- ds.lmerSLMA(
  dataName = "analysis_df_m",
  formula = "ext_pc_ ~ 1 + ext_age_m_1 + ext_age_m_0_5 + edu_rank_num + 
  edu_rank_num*ext_age_m_1 + edu_rank_num*ext_age_m_0_5 + (1|child_id_int)",
  datasources = conns[ext_pc_coh])

ext_pc_m.tab <- dh.lmerTab(ext_pc_m.fit)
colnames(ext_pc_m.tab) <- c("coefficient", ext_pc_coh, "combined")


## ---- Externalising females ----------------------------------------------------
ext_pc_f.fit <- ds.lmerSLMA(
  dataName = "analysis_df_f",
  formula = "ext_pc_ ~ 1 + ext_age_m_1 + ext_age_m_0_5 + edu_rank_num + 
  edu_rank_num*ext_age_m_1 + edu_rank_num*ext_age_m_0_5 + (1|child_id_int)",
  datasources = conns[ext_pc_coh])

ext_pc_f.tab <- dh.lmerTab(ext_pc_f.fit)
colnames(ext_pc_f.tab) <- c("coefficient", ext_pc_coh, "combined")


## ---- Internalising males ----------------------------------------------------
int_pc_m.fit <- ds.lmerSLMA(
  dataName = "analysis_df_m",
  formula = "int_pc_ ~ 1 + int_age_m_2 + int_age_m_1 + edu_rank_num +
  edu_rank_num*int_age_m_2 + edu_rank_num*int_age_m_1 + (1|child_id_int)",
  datasources = conns[int_pc_coh])

int_pc_m.tab <- dh.lmerTab(int_pc_m.fit)
colnames(int_pc_m.tab) <- c("coefficient", int_pc_coh, "combined")


## ---- Internalising females ----------------------------------------------------
int_pc_f.fit <- ds.lmerSLMA(
  dataName = "analysis_df_f",
  formula = "int_pc_ ~ 1 + int_age_m_2 + int_age_m_1 + edu_rank_num +
  edu_rank_num*int_age_m_2 + edu_rank_num*int_age_m_1 + (1|child_id_int)",
  datasources = conns[int_pc_coh])

int_pc_f.tab <- dh.lmerTab(int_pc_f.fit)
colnames(int_pc_f.tab) <- c("coefficient", int_pc_coh, "combined")



################################################################################
# Define new function 
################################################################################

dh.predRefSex <- function(df, core_agevar, coeftab, fixed, conns){
  
  
  ## ---- Make factor version of rank variable -----------------------------------
  fix_fac <- paste0(fixed, "_fac")
  
  ds.asFactor(paste0(df, "$", fixed), newobj = fix_fac, datasources = conns)
  ds.cbind(c(df, fix_fac), newobj = df, datasources = conns)
  
  ## ---- Get age info for each cohort -------------------------------------------
  ages_ref <- dh.getStats(
    df = df,
    vars = c(core_agevar, fix_fac),
    conns = conns
  )
  
  coefs <- as_tibble(cbind(cohort = names(coeftab), t(coeftab))) %>%
    filter(cohort != "coefficient")
  
  ages <- ages_ref$continuous %>%
    select(cohort, perc_5, perc_95) %>%
    filter(cohort != "combined")
  
  comb_ages <- tibble(
    cohort = "combined",
    perc_5 = min(ages$perc_5, na.rm = TRUE),
    perc_95 = max(ages$perc_95, na.rm = TRUE)
  )
  
  fixed <- ages_ref$categorical %>%
    filter(value != 0 & cohort != "combined") %>%
    group_by(cohort) %>%
    mutate(fact_levels = paste(category, collapse = ",")) %>%
    slice(1) %>%
    select(cohort, fact_levels)
  
  ages <- ages_ref$continuous %>%
    select(cohort, perc_5, perc_95) %>%
    filter(cohort != "combined")
  
  comb_ages <- tibble(
    cohort = "combined",
    perc_5 = min(ages$perc_5, na.rm = TRUE),
    perc_95 = max(ages$perc_95, na.rm = TRUE)
  )
  
  ages <- bind_rows(ages, comb_ages)
  
  predref <- left_join(coefs, fixed, by = "cohort") %>%
    left_join(., ages, by = "cohort")
  
  colnames(predref) <- c("cohort", "intercept", coeftab$coefficient[2:4], 
                         "poly_1_ed", "poly_2_ed", "fact_levels", "low_age", "high_age")
  
  predref %<>% mutate_at(vars(intercept, coeftab$coefficient[2:4], poly_1_ed, poly_2_ed), as.numeric)
  
  
  return(predref)
}

################################################################################
# Sex-stratified SII  
################################################################################

## ---- Externalising males ----------------------------------------------------
ext_pc_ref_m <- dh.predRefSex(
  df = "analysis_df_m",
  core_agevar = "ext_age_",
  coeftab = ext_pc_m.tab,
  fixed = "edu_rank_num",
  conns = conns[ext_pc_coh])

ext_pc_m.pred <- ext_pc_ref_m %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_1 = age^-1,
        age_m_0_5 = age^-0.5,
        sii = edu_rank_num + age_m_1*poly_1_ed + age_m_0_5*poly_2_ed
      )
      
      return(pred)
    }
  )

ext_pc_m.pred <- bind_rows(ext_pc_m.pred) %>% mutate(sex = "male")

## ---- Externalising females --------------------------------------------------
ext_pc_ref_f <- dh.predRefSex(
  df = "analysis_df_f",
  core_agevar = "ext_age_",
  coeftab = ext_pc_f.tab,
  fixed = "edu_rank_num",
  conns = conns[ext_pc_coh])

ext_pc_f.pred <- ext_pc_ref_f %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_f_1 = age^-1,
        age_f_0_5 = age^-0.5,
        sii = edu_rank_num + age_f_1*poly_1_ed + age_f_0_5*poly_2_ed
      )
      
      return(pred)
    }
  )

ext_pc_f.pred <- bind_rows(ext_pc_f.pred) %>% mutate(sex = "female")


## ---- Internalising males ----------------------------------------------------
int_pc_ref_m <- dh.predRefSex(
  df = "analysis_df_m",
  core_agevar = "int_age_",
  coeftab = int_pc_m.tab,
  fixed = "edu_rank_num",
  conns = conns[int_pc_coh])

int_pc_m.pred <- int_pc_ref_m %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_2 = age^-2,
        age_m_1 = age^-1,
        sii = edu_rank_num + age_m_2*poly_1_ed + age_m_1*poly_2_ed
      )
      
      return(pred)
    }
  )

int_pc_m.pred <- bind_rows(int_pc_m.pred) %>% mutate(sex = "male")


## ---- Internalising females ----------------------------------------------------
int_pc_ref_f <- dh.predRefSex(
  df = "analysis_df_f",
  core_agevar = "int_age_",
  coeftab = int_pc_f.tab,
  fixed = "edu_rank_num",
  conns = conns[int_pc_coh])

int_pc_f.pred <- int_pc_ref_f %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_f_2 = age^-2,
        age_f_1 = age^-1,
        sii = edu_rank_num + age_f_2*poly_1_ed + age_f_1*poly_2_ed
      )
      
      return(pred)
    }
  )

int_pc_f.pred <- bind_rows(int_pc_f.pred) %>% mutate(sex = "female")


## ---- Combine male and female estimates --------------------------------------
ext_pc_sex.pred <- bind_rows(ext_pc_m.pred, ext_pc_f.pred)
int_pc_sex.pred <- bind_rows(int_pc_m.pred, int_pc_f.pred)


################################################################################
# Plot sex-stratified SII  
################################################################################

## ---- Externalising ----------------------------------------------------------
ggplot() + 
  geom_line(data = ext_pc_sex.pred, aes(x = age, y = sii, colour = sex)) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 40), breaks = seq(-40, 40, 20), expand = c(0, 0))


## ---- Internalising ----------------------------------------------------------
ggplot() + 
  geom_line(data = int_pc_sex.pred, aes(x = age, y = sii, colour = sex)) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 40), breaks = seq(-40, 40, 20), expand = c(0, 0))



# I think first thing is to make slides based on these results
# Then I can add things like (i) confidence bands, (ii) predicted differences


## ---- Cohort numbers ---------------------------------------------------------
included_n <- paste0("study", c(1:3)) %>%
  map(function(x){
    
    ext_pc.fit$output.summary[[x]]$ngrps    
    
  })

names(included_n) <- ext_pc_coh






## ---- Externalising ----------------------------------------------------------
ext_best.pred <- dh.predPoly(
  df = "analysis_df",
  fixed = "edu_rank_num",
  core_agevar = "ext_age_",
  coeftab = ext_pc.tab,
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












################################################################################
# Age only models  
################################################################################
################################################################################
# 1. Make formulas
################################################################################

## ---- Raw scores -------------------------------------------------------------

## Externalising
ext_form_raw <- dh.makeLmerForm(
  outcome = "ext_raw_", 
  idvar = "child_id_int", 
  agevars = c("ext_age_", "ext_age_m_2", "ext_age_m_1", "ext_age_m_0_5", 
              "ext_age_log", "ext_age_0_5", "ext_age_2", "ext_age_3"), 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num")


## Internalising
int_form_raw <- dh.makeLmerForm(
  outcome = "int_raw_", 
  idvar = "child_id_int", 
  agevars = c("int_age_", "int_age_m_2", "int_age_m_1", "int_age_m_0_5", 
              "int_age_log", "int_age_0_5", "int_age_2", "int_age_3"),
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num"
)  






################################################################################
# 2. Fit all polynomials using datashield: raw scores
################################################################################

## ---- Externalising CBCL -----------------------------------------------------
ext_cbcl_poly.fit <- dh.lmeMultPoly(
  df = "ext_cbcl_sub", 
  formulae = ext_form_raw, 
  conns = conns[ext_cbcl_coh])

## ---- Externalising SDQ ------------------------------------------------------
ext_sdq_poly.fit <- dh.lmeMultPoly(
  df = "ext_sdq_sub", 
  formulae = ext_form_raw, 
  conns = conns[ext_sdq_coh])

## ---- Internalising CBCL -----------------------------------------------------
int_cbcl_poly.fit <- dh.lmeMultPoly(
  df = "int_cbcl_sub", 
  formulae = int_form_raw,
  conns = conns[int_cbcl_coh])

## ---- Internalising SDQ ------------------------------------------------------
int_sdq_poly.fit <- dh.lmeMultPoly(
  df = "int_sdq_sub", 
  formulae = int_form_raw,
  conns = conns[int_sdq_coh])


################################################################################
# 3. Fit all polynomials using datashield: percentiles scores
################################################################################

perc_coh <- c("dnbc", "moba", "raine")

## ---- Externalising ----------------------------------------------------------
ext_pc_poly.fit <- dh.lmeMultPoly(
  df = "analysis_df", 
  formulae = ext_form_pc, 
  conns = conns[perc_coh])

## ---- Internalising ----------------------------------------------------------
int_pc_poly.fit <- dh.lmeMultPoly(
  df = "analysis_df", 
  formulae = int_form_pc,
  conns = conns[perc_coh])


################################################################################
# 3. Merge in ALSPAC data
################################################################################

## ---- CBCL -------------------------------------------------------------------

## This is simple as all done in datashield
ext_cbcl_poly.fit$fit %>% arrange(av_rank)
int_cbcl_poly.fit$fit %>% arrange(av_rank)

## ---- SDQ --------------------------------------------------------------------
alspac_ext.fit <- read.csv("ext_sdq_fit.csv") %>% 
  select(model, loglik_alspac, log_rank_alspac) 

alspac_int.fit <- read.csv("int_sdq_fit.csv") %>%
  select(model, loglik_alspac, log_rank_alspac) 


## ---- Join together ----------------------------------------------------------
ext_sdq_poly_fit.tab <- left_join(ext_sdq_poly.fit$fit, alspac_ext.fit, by = "model") %>%
  mutate(av_rank = rowMeans(select(., starts_with("log_rank")), na.rm = TRUE)) %>%
  arrange(av_rank)

int_sdq_poly_fit.tab <- left_join(int_sdq_poly.fit$fit, alspac_int.fit, by = "model") %>%
  mutate(av_rank = rowMeans(select(., starts_with("log_rank")), na.rm = TRUE)) %>%
  arrange(av_rank)


## ---- Tibble of best fitting models ------------------------------------------
bestfit <- tibble(
  polys = c("ext_age_m_0_5,ext_age_0_5", "int_age_m_1,int_age_m_0_5", 
            "ext_age_m_2,ext_age_m_1", "int_age_m_2,int_age_0_5"),
  model = c("ext_cbcl", "int_cbcl", "ext_sdq", "int_sdq"))
  

################################################################################
# 4. Run age only models for datashield cohorts
################################################################################

## ---- Externalisng CBCL ------------------------------------------------------
ext_cbcl_age.fit <- ds.lmerSLMA(
  dataName = "ext_cbcl_sub",
  formula = "ext_raw_ ~ 1 + ext_age_m_0_5 + ext_age_0_5 + (1|child_id_int)",
  datasources = conns[ext_cbcl_coh])


## ---- Externalising SDQ ------------------------------------------------------
ext_sdq_age.fit <- ds.lmerSLMA(
  dataName = "ext_sdq_sub",
  formula = "ext_raw_ ~ 1 + ext_age_m_2 + ext_age_m_1 + (1|child_id_int)",
  datasources = conns[ext_sdq_coh])


## ---- Internalising CBCL -----------------------------------------------------
int_cbcl_age.fit <- ds.lmerSLMA(
  dataName = "int_cbcl_sub",
  formula = "int_raw_ ~ 1 + int_age_m_1 + int_age_m_0_5 + (1|child_id_int)",
  datasources = conns[int_cbcl_coh])


## ---- Internalising SDQ ------------------------------------------------------
int_sdq_age.fit <- ds.lmerSLMA(
  dataName = "int_sdq_sub",
  formula = "int_raw_ ~ 1 + int_age_m_2 + int_age_m_0_5 + (1|child_id_int)",
  datasources = conns[int_sdq_coh])


################################################################################
# 5. Read in coefficients and standard errors for ALSPAC  
################################################################################

## ---- Read in coefficients ---------------------------------------------------
alsp_ext_coef <- read_csv("ext_coef.csv")
alsp_int_coef <- read_csv("int_coef.csv")

## ---- Read in vcov matrix ----------------------------------------------------
alsp_ext_vcov <- read_csv("ext_vcov.csv") %>% select(-X1) %>% as.matrix
alsp_int_vcov <- read_csv("int_vcov.csv") %>% select(-X1) %>% as.matrix

## ---- Calculate standard errors ----------------------------------------------
alsp_ext.tab <- tibble(
  term = alsp_ext_coef$X1,
  coef = alsp_ext_coef$x, 
  se = sqrt(diag(alsp_ext_vcov)))
  
alsp_int.tab <- tibble(
  term = alsp_int_coef$X1,
  coef = alsp_int_coef$x, 
  se = sqrt(diag(alsp_int_vcov)))


################################################################################
# 6. Meta-analyse study coefficients  
################################################################################
dh.metaManual <- function(terms, betas, ses){
  
  nvar <- seq(1, dim(betas)[1], 1)

    ma <- nvar %>%
    map(function(x){
      rma(
        yi = betas[x, ],
        sei = ses[x, ], 
        method = "ML")
      })
  
  out <- tibble(
    term = terms,
    coef = ma %>% map_chr(function(x){x[["beta"]]}),
    se = ma %>% map_chr(function(x){x[["se"]]})
    )
  
  return(out)
  
}


## ---- Externalising, CBCL ----------------------------------------------------
ext_cbcl_com <- dh.metaManual(
  terms = dimnames(ext_cbcl_age.fit$betamatrix.valid)[[1]], 
  betas = ext_cbcl_age.fit$betamatrix.valid, 
  ses = ext_cbcl_age.fit$sematrix.valid)

str(ext_cbcl_age.fit$betamatrix.valid)

## ---- Externalising, SDQ -----------------------------------------------------
ext_sdq_betas <- matrix(c(ext_sdq_age.fit$betamatrix.all[, 1], alsp_ext.tab$coef), nrow = 3)
ext_sdq_ses <- matrix(c(ext_sdq_age.fit$sematrix.all[, 1], alsp_ext.tab$se), nrow = 3)

ext_sdq_com <- dh.metaManual(
  terms = alsp_ext.tab$term,
  betas = ext_sdq_betas, 
  ses = ext_sdq_ses)


## ---- Internalising, CBCL ----------------------------------------------------
ext_cbcl_com <- dh.metaManual(
  terms = dimnames(int_cbcl_age.fit$betamatrix.valid)[[1]], 
  betas = int_cbcl_age.fit$betamatrix.valid, 
  ses = int_cbcl_age.fit$sematrix.valid)


## ---- Internalising, SDQ -----------------------------------------------------
int_sdq_betas <- matrix(c(int_sdq_age.fit$betamatrix.all[, 1], alsp_int.tab$coef), nrow = 3)
int_sdq_ses <- matrix(c(int_sdq_age.fit$sematrix.all[, 1], alsp_int.tab$se), nrow = 3)

int_sdq_com <- dh.metaManual(
  terms = alsp_int.tab$term,
  betas = int_sdq_betas, 
  ses = int_sdq_ses)
  

################################################################################
# PREDICTED VALUES: MAIN EFFECTS AGE
################################################################################
################################################################################
# FUNCTION TO GET REFERENCE AGES  
################################################################################
dh.predRef <- function(df, core_agevar, coeftab, conns){
  
ages_ref <- dh.getStats(
  df = df,
  vars = core_agevar,
  conns = conns
)

coefs <- as_tibble(cbind(cohort = names(coeftab), t(coeftab))) %>%
  filter(cohort != "coefficient")

ages <- ages_ref$continuous %>%
  select(cohort, perc_5, perc_95) %>%
  filter(cohort != "combined")

comb_ages <- tibble(
  cohort = "combined",
  perc_5 = min(ages$perc_5, na.rm = TRUE),
  perc_95 = max(ages$perc_95, na.rm = TRUE)
)

ages <- bind_rows(ages, comb_ages)

predref <- left_join(coefs, ages, by = "cohort")

colnames(predref) <- c("cohort", "intercept", coeftab$coefficient[2: 3], "low_age", "high_age")

predref %<>% mutate_at(vars(coeftab$coefficient[2: 3], "intercept"), as.numeric)


return(predref)
}


################################################################################
# EXTERNALISING: CBCL  
################################################################################
ext_cbcl.tab <- dh.lmerTab(ext_cbcl_age.fit)
colnames(ext_cbcl.tab) <- c("coefficient", ext_cbcl_coh, "combined")

ext_cbcl_ref <- dh.predRef(
  df = "ext_cbcl_sub",
  core_agevar = "ext_age_",
  coeftab = ext_cbcl.tab,
  conns = conns[ext_cbcl_coh])
  

ext_cbcl.pred <- ext_cbcl_ref %>%
  pmap(
    function(cohort, low_age, high_age, intercept, ext_age_m_0_5, ext_age_0_5) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_0_5 = age^-0.5,
        age_0_5 = age^0.5,
        predicted = intercept + (age_m_0_5 * ext_age_m_0_5) + (age_0_5 * ext_age_0_5)
      )
      
      return(pred)
    }
  )

ext_cbcl.pred <- bind_rows(ext_cbcl.pred) 

ggplot() + 
  geom_line(data = ext_cbcl.pred, aes(x = age, y = predicted)) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 14), breaks = seq(0, 14, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(0, 30), breaks = seq(0, 30, 5), expand = c(0, 0))


################################################################################
# EXTERNALISING: SDQ  
################################################################################
dh.getStats(
  df = "ext_sdq_sub", 
  vars = "ext_age_", 
  conns = conns["dnbc"])

ext_sdq.tab <- tibble(
  coefficient = names(ext_sdq_age.fit$betamatrix.valid), 
  dnbc = ext_sdq_age.fit$betamatrix.valid, 
  alspac = alsp_ext.tab %>% pull(coef), 
  combined = ext_sdq_com %>% pull(coef))

ext_sdq.ref <- as_tibble(cbind(cohort = names(ext_sdq.tab), t(ext_sdq.tab))) %>%
  filter(cohort != "coefficient") %>%
  mutate(
    low_age = c(7, 5, 5), 
    high_age = c(12, 16, 16)
  )

names(ext_sdq.ref) <- c(
  "cohort", "intercept", names(ext_sdq_age.fit$betamatrix.valid)[2:3], 
  "low_age", "high_age")

ext_sdq.ref %<>% mutate_at(vars(ext_sdq.tab$coefficient[2: 3], "intercept"), as.numeric)

ext_sdq.pred <- ext_sdq.ref %>%
  pmap(
    function(cohort, low_age, high_age, intercept, ext_age_m_2, ext_age_m_1) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_2 = age^-2,
        age_m_1 = age^-1,
        predicted = intercept + (age_m_2 * ext_age_m_2) + (age_m_1 * ext_age_m_1)
      )
      
      return(pred)
    }
  )

ext_sdq.pred <- bind_rows(ext_sdq.pred) 

ggplot() + 
  geom_line(data = ext_sdq.pred, aes(x = age, y = predicted)) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 14), breaks = seq(0, 14, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(0, 10), breaks = seq(0, 10, 2), expand = c(0, 0))


################################################################################
# INTERNALISING: CBCL  
################################################################################
int_cbcl.tab <- dh.lmerTab(int_cbcl_age.fit)
colnames(int_cbcl.tab) <- c("coefficient", int_cbcl_coh, "combined")

int_cbcl_ref <- dh.predRef(
  df = "int_cbcl_sub",
  core_agevar = "int_age_",
  coeftab = int_cbcl.tab,
  conns = conns[int_cbcl_coh])

int_cbcl.pred <- int_cbcl_ref %>%
  pmap(
    function(cohort, low_age, high_age, intercept, int_age_m_1, int_age_m_0_5) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_1 = age^-1,
        age_m_0_5 = age^-0.5,
        predicted = intercept + (age_m_1 * int_age_m_1) + (age_m_0_5 * int_age_m_0_5)
      )
      
      return(pred)
    }
  )

int_cbcl.pred <- bind_rows(int_cbcl.pred) 

ggplot() + 
  geom_line(data = int_cbcl.pred, aes(x = age, y = predicted)) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 14), breaks = seq(0, 14, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(0, 30), breaks = seq(0, 30, 5), expand = c(0, 0))


################################################################################
# INTERNALISING: SDQ  
################################################################################
int_sdq.tab <- tibble(
  coefficient = names(int_sdq_age.fit$betamatrix.valid), 
  dnbc = int_sdq_age.fit$betamatrix.valid, 
  alspac = alsp_int.tab %>% pull(coef), 
  combined = int_sdq_com %>% pull(coef))

int_sdq.ref <- as_tibble(cbind(cohort = names(int_sdq.tab), t(int_sdq.tab))) %>%
  filter(cohort != "coefficient") %>%
  mutate(
    low_age = c(7, 5, 5), 
    high_age = c(12, 16, 16)
  )

names(int_sdq.ref) <- c(
  "cohort", "intercept", names(int_sdq_age.fit$betamatrix.valid)[2:3], 
  "low_age", "high_age")

int_sdq.ref %<>% mutate_at(vars(int_sdq.tab$coefficient[2: 3], "intercept"), as.numeric)

int_sdq.pred <- int_sdq.ref %>%
  pmap(
    function(cohort, low_age, high_age, intercept, int_age_m_2, int_age_m_0_5) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_2 = age^-2,
        age_m_0_5 = age^-0.5,
        predicted = intercept + (age_m_2 * int_age_m_2) + (age_m_0_5 * int_age_m_0_5)
      )
      
      return(pred)
    }
  )

int_sdq.pred <- bind_rows(int_sdq.pred) 

ggplot() + 
  geom_line(data = int_sdq.pred, aes(x = age, y = predicted)) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 14), breaks = seq(0, 14, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(0, 10), breaks = seq(0, 10, 2), expand = c(0, 0))



################################################################################
# FULL MODELS: RAW SCORES
################################################################################

bestfit

## ---- Externalisng CBCL ------------------------------------------------------
ext_cbcl.fit <- ds.lmerSLMA(
  dataName = "ext_cbcl_sub",
  formula = "ext_raw_ ~ 1 + ext_age_m_0_5 + ext_age_0_5 + edu_rank_num + 
  edu_rank_num*ext_age_m_0_5 + edu_rank_num*ext_age_0_5 + (1|child_id_int)",
  datasources = conns[ext_cbcl_coh])

## ---- Internalising CBCL -----------------------------------------------------
int_cbcl.fit <- ds.lmerSLMA(
  dataName = "int_cbcl_sub",
  formula = "int_raw_ ~ 1 + int_age_m_1 + int_age_m_0_5 + edu_rank_num +
  edu_rank_num*int_age_m_1 + edu_rank_num*int_age_m_0_5 + (1|child_id_int)",
  datasources = conns[int_cbcl_coh])

## ---- Externalising SDQ ------------------------------------------------------
ext_sdq.fit <- ds.lmerSLMA(
  dataName = "ext_sdq_sub",
  formula = "ext_raw_ ~ 1 + ext_age_m_2 + ext_age_m_1 + edu_rank_num + 
  edu_rank_num*ext_age_m_2 + edu_rank_num*ext_age_m_1 + (1|child_id_int)",
  datasources = conns[ext_sdq_coh])

## ---- Internalising SDQ ------------------------------------------------------
int_sdq.fit <- ds.lmerSLMA(
  dataName = "int_sdq_sub",
  formula = "int_raw_ ~ 1 + int_age_m_2 + int_age_m_0_5 + edu_rank_num + 
  edu_rank_num*int_age_m_2 + edu_rank_num*int_age_m_0_5 + (1|child_id_int)",
  datasources = conns[int_sdq_coh])


################################################################################
# FULL MODELS: PERCENTILE SCORES  
################################################################################

## ---- Externalisng CBCL ------------------------------------------------------
ext_cbcl.fit <- ds.lmerSLMA(
  dataName = "ext_cbcl_sub",
  formula = "ext_raw_ ~ 1 + ext_age_m_0_5 + ext_age_0_5 + edu_rank_num + 
  edu_rank_num*ext_age_m_0_5 + edu_rank_num*ext_age_0_5 + (1|child_id_int)",
  datasources = conns[ext_cbcl_coh])

## ---- Internalising CBCL -----------------------------------------------------
int_cbcl.fit <- ds.lmerSLMA(
  dataName = "int_cbcl_sub",
  formula = "int_raw_ ~ 1 + int_age_m_1 + int_age_m_0_5 + edu_rank_num +
  edu_rank_num*int_age_m_1 + edu_rank_num*int_age_m_0_5 + (1|child_id_int)",
  datasources = conns[int_cbcl_coh])




################################################################################
# Read in coefficients and standard errors for ALSPAC  
################################################################################

## ---- Read in coefficients ---------------------------------------------------
alsp_ext_coef_full <- read_csv("ext_coef_full.csv")
alsp_int_coef_full <- read_csv("int_coef_full.csv")

## ---- Read in vcov matrix ----------------------------------------------------
alsp_ext_vcov <- read_csv("ext_vcov_full.csv") %>% select(-X1) %>% as.matrix
alsp_int_vcov <- read_csv("int_vcov_full.csv") %>% select(-X1) %>% as.matrix

## ---- Calculate standard errors ----------------------------------------------
alsp_ext_full.tab <- tibble(
  term = alsp_ext_coef$X1,
  coef = alsp_ext_coef$x, 
  se = sqrt(diag(alsp_ext_vcov)))

alsp_int_full.tab <- tibble(
  term = alsp_int_coef$X1,
  coef = alsp_int_coef$x, 
  se = sqrt(diag(alsp_int_vcov)))

################################################################################
# Meta analyse coefficients  
################################################################################

## ---- Externalising, CBCL ----------------------------------------------------
ext_cbcl_full_com <- dh.metaManual(
  terms = dimnames(ext_cbcl.fit$betamatrix.valid)[[1]], 
  betas = ext_cbcl.fit$betamatrix.valid, 
  ses = ext_cbcl.fit$sematrix.valid)


## ---- Externalising, SDQ -----------------------------------------------------
ext_sdq_full_betas <- matrix(c(ext_sdq.fit$betamatrix.all[, 1], alsp_ext_full.tab$coef), nrow = 6)
ext_sdq_full_ses <- matrix(c(ext_sdq.fit$sematrix.all[, 1], alsp_ext_full.tab$se), nrow = 6)

ext_sdq_full_com <- dh.metaManual(
  terms = alsp_ext_full.tab$term,
  betas = ext_sdq_full_betas, 
  ses = ext_sdq_full_ses)


## ---- Internalising, CBCL ----------------------------------------------------
ext_cbcl_full_com <- dh.metaManual(
  terms = dimnames(int_cbcl.fit$betamatrix.valid)[[1]], 
  betas = int_cbcl.fit$betamatrix.valid, 
  ses = int_cbcl.fit$sematrix.valid)


## ---- Internalising, SDQ -----------------------------------------------------
int_sdq_full_betas <- matrix(c(int_sdq.fit$betamatrix.all[, 1], alsp_int_full.tab$coef), nrow = 6)
int_sdq_full_ses <- matrix(c(int_sdq.fit$sematrix.all[, 1], alsp_int_full.tab$se), nrow = 6)

int_sdq_com <- dh.metaManual(
  terms = alsp_int_full.tab$term,
  betas = int_sdq_full_betas, 
  ses = int_sdq_full_ses)



################################################################################
# PREDICTED VALUES: FULL MODELS
################################################################################
dh.predRefFull <- function(df, core_agevar, coeftab, fixed, conns){
  
  
  ## ---- Make factor version of rank variable -----------------------------------
  fix_fac <- paste0(fixed, "_fac")
  
  ds.asFactor(paste0(df, "$", fixed), newobj = fix_fac, datasources = conns)
  ds.cbind(c(df, fix_fac), newobj = df, datasources = conns)
  
  ## ---- Get age info for each cohort -------------------------------------------
  ages_ref <- dh.getStats(
    df = df,
    vars = c(core_agevar, fix_fac),
    conns = conns
  )
  
    coefs <- as_tibble(cbind(cohort = names(coeftab), t(coeftab))) %>%
    filter(cohort != "coefficient")
  
  ages <- ages_ref$continuous %>%
    select(cohort, perc_5, perc_95) %>%
    filter(cohort != "combined")
  
  comb_ages <- tibble(
    cohort = "combined",
    perc_5 = min(ages$perc_5, na.rm = TRUE),
    perc_95 = max(ages$perc_95, na.rm = TRUE)
  )
  
    fixed <- ages_ref$categorical %>%
    filter(value != 0 & cohort != "combined") %>%
    group_by(cohort) %>%
    mutate(fact_levels = paste(category, collapse = ",")) %>%
    slice(1) %>%
    select(cohort, fact_levels)
  
  ages <- ages_ref$continuous %>%
    select(cohort, perc_5, perc_95) %>%
    filter(cohort != "combined")
  
  comb_ages <- tibble(
    cohort = "combined",
    perc_5 = min(ages$perc_5, na.rm = TRUE),
    perc_95 = max(ages$perc_95, na.rm = TRUE)
  )
  
  ages <- bind_rows(ages, comb_ages)
  
  predref <- left_join(coefs, fixed, by = "cohort") %>%
    left_join(., ages, by = "cohort")
  
  colnames(predref) <- c("cohort", "intercept", coeftab$coefficient[2:4], 
                         "poly_1_ed", "poly_2_ed", "fact_levels", "low_age", "high_age")
  
  predref %<>% mutate_at(vars(intercept, coeftab$coefficient[2:4], poly_1_ed, poly_2_ed), as.numeric)
  
  
  return(predref)
}

################################################################################
# EXTERNALISING: CBCL  
################################################################################
ext_cbcl_full.tab <- dh.lmerTab(ext_cbcl.fit)
colnames(ext_cbcl_full.tab) <- c("coefficient", ext_cbcl_coh, "combined")

ext_cbcl_full_ref <- dh.predRefFull(
  df = "ext_cbcl_sub",
  core_agevar = "ext_age_",
  coeftab = ext_cbcl_full.tab,
  fixed = "edu_rank_num",
  conns = conns[ext_cbcl_coh])


ext_cbcl_full.pred <- ext_cbcl_full_ref %>%
  pmap(
    function(cohort, low_age, high_age, ext_age_m_0_5, ext_age_0_5, 
             edu_rank_num, poly_1_ed, poly_2_ed, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_0_5 = age^-0.5,
        age_0_5 = age^0.5,
        sii = edu_rank_num + age_m_0_5*poly_1_ed, age_0_5*poly_2_ed
      )
      
      return(pred)
    }
  )

ext_cbcl_full.pred <- bind_rows(ext_cbcl_full.pred) 

ggplot() + 
  geom_line(data = ext_cbcl_full.pred, aes(x = age, y = sii, colour = cohort)) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-10, 10), breaks = seq(-10, 10, 2), expand = c(0, 0))






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




