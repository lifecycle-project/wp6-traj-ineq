################################################################################
## Project: wp6-traj-ineq
## Script purpose: Describe available data
## Date: 30th September 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################


################################################################################
# 1. Load packages  
################################################################################

library(DSI)
library(DSOpal)
library(dsBaseClient)
library(purrr)
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(ggplot2)

library(remotes)
install_github("lifecycle-project/ds-helper")
library(dsHelper)


################################################################################
# 1. Load workspace  
################################################################################
opals <- datashield.login(logindata, restore = "traj_assigned_23_10_20")

################################################################################
# 2. Visualise data using DS scatter plots  
################################################################################
ds.scatterPlot(
  x = "sdq_sub$ext_age_", 
  y = "sdq_sub$ext_raw_", 
  datasources = opals[c("chop", "dnbc")]
)

ds.scatterPlot(
  x = "cbcl_sub$ext_age_", 
  y = "cbcl_sub$ext_raw_", 
  datasources = opals[c("raine", "moba")]
)

ds.scatterPlot(
  x = "mhrep$ext_age_", 
  y = "mhrep$ext_pc_", 
  datasources = opals[c("chop", "dnbc", "moba", "raine", "inma")])


################################################################################
# 3. Get means per (rough) measurement occasion 
################################################################################

## ---- SDQ subsets ------------------------------------------------------------
dh.makeOutcome(
  df = "sdq_sub", 
  outcome = "e_r", 
  age_var = "ext_age_", 
  bands = c(4, 6, 10, 12), 
  df_name = "sdq_sub_all", 
  keep_original = TRUE, 
  cohorts = "chop"
)

datashield.workspace_save(opals, "mhineq_means_a")
opals <- datashield.login(logindata, restore = "mhineq_means_a")
## We need to keep saving the workspace as we go along because sometimes DS
## times out, and we need to redo everything again 

dh.makeOutcome(
  df = "sdq_sub", 
  outcome = "e_r", 
  age_var = "ext_age_", 
  bands = c(6, 10, 10, 15), 
  df_name = "sdq_sub_all", 
  keep_original = TRUE, 
  cohorts = "dnbc"
)

datashield.workspace_save(opals, "mhineq_means_b")
opals <- datashield.login(logindata, restore = "mhineq_means_b")

## ---- CBCL subsets -----------------------------------------------------------
dh.makeOutcome(
  df = "cbcl_sub", 
  outcome = "ext_raw_", 
  age_var = "ext_age_", 
  bands = c(0, 2, 2, 4, 4, 8), 
  df_name = "cbcl_sub_all", 
  keep_original = TRUE, 
  cohorts = "moba"
)

datashield.workspace_save(opals, "mhineq_means_c")
opals <- datashield.login(logindata, restore = "mhineq_means_c")

dh.makeOutcome(
  df = "cbcl_sub", 
  outcome = "ext_raw_", 
  age_var = "ext_age_", 
  bands = c(0, 4, 4, 7, 7, 10, 10, 12), 
  df_name = "cbcl_sub_all", 
  keep_original = TRUE, 
  cohorts = "raine"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(opals, "mhineq_means")
opals <- datashield.login(logindata, restore = "mhineq_means")

################################################################################
# 4. Stratify by sex  
################################################################################

## ---- SDQ --------------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "sdq_sub_all", 
  V1.name = "sdq_sub_all$sex", 
  V2.name = "1", 
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "sdq_sub_m",
  datasources = opals[c("chop", "dnbc")]
  )

ds.dataFrameSubset(
  df.name = "sdq_sub_all", 
  V1.name = "sdq_sub_all$sex", 
  V2.name = "2", 
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "sdq_sub_f",
  datasources = opals[c("chop", "dnbc")]
)

## ---- CBCL -------------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "cbcl_sub_all", 
  V1.name = "cbcl_sub_all$sex", 
  V2.name = "1", 
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "cbcl_sub_m",
  datasources = opals[c("moba", "raine")]
)

ds.dataFrameSubset(
  df.name = "cbcl_sub_all", 
  V1.name = "cbcl_sub_all$sex", 
  V2.name = "2", 
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "cbcl_sub_f",
  datasources = opals[c("moba", "raine")]
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(opals, "mhineq_a_4")
opals <- datashield.login(logindata, restore = "mhineq_a_4")

################################################################################
# 5. Get descriptives  
################################################################################

## ---- SDQ --------------------------------------------------------------------

## CHOP
sdq_means_chop_m <- dh.getStats(
  df = "sdq_sub_m", 
  vars = c("e_r.12", "age.12", "e_r.6", "age.6"),
  cohorts = "chop")

sdq_means_chop_f <- dh.getStats(
  df = "sdq_sub_f", 
  vars = c("e_r.12", "age.12", "e_r.6", "age.6"),
  cohorts = "chop")

## DNBC
sdq_means_dnbc_m <- dh.getStats(
  df = "sdq_sub_m", 
  vars = c("e_r.10", "e_r.15", "age.10", "age.15"),
  cohorts = "dnbc")

sdq_means_dnbc_f <- dh.getStats(
  df = "sdq_sub_f", 
  vars = c("e_r.10", "e_r.15", "age.10", "age.15"),
  cohorts = "dnbc")

## ---- CBCL -------------------------------------------------------------------

## MoBa
cbcl_means_moba_m <- dh.getStats(
  df = "cbcl_sub_m", 
  vars = c("ext_raw_.2", "ext_raw_.4", "ext_raw_.8", 
           "age.2", "age.4", "age.8"),
  cohorts = "moba"
)

cbcl_means_moba_f <- dh.getStats(
  df = "cbcl_sub_f", 
  vars = c("ext_raw_.2", "ext_raw_.4", "ext_raw_.8", 
           "age.2", "age.4", "age.8"),
  cohorts = "moba"
)

## Raine
cbcl_means_raine_m <- dh.getStats(
  df = "cbcl_sub_m", 
  vars = c("ext_raw_.4", "ext_raw_.7", "ext_raw_.10", "ext_raw_.12", 
           "age.4", "age.7", "age.10", "age.12"),
  cohorts = "raine"
)

cbcl_means_raine_f <- dh.getStats(
  df = "cbcl_sub_f", 
  vars = c("ext_raw_.4", "ext_raw_.7", "ext_raw_.10", "ext_raw_.12", 
           "age.4", "age.7", "age.10", "age.12"),
  cohorts = "raine"
)

################################################################################
# 6. Combine these in a table I can use for plotting  
################################################################################

## ---- SDQ --------------------------------------------------------------------
sdq_m_means <- list(sdq_means_chop_m[[2]], sdq_means_dnbc_m[[2]]) %>%
  map_df(function(x){mutate(x, sex = "male")})

sdq_f_means <- list(sdq_means_chop_f[[2]], sdq_means_dnbc_f[[2]]) %>%
  map_df(function(x){mutate(x, sex = "female")})

sdq_means <- bind_rows(sdq_m_means, sdq_f_means) %>%
  separate(variable, c("var", "age"), sep = "([.])") %>%
  mutate(var = ifelse(var == "e_r", "ext", var)) %>%
  rename(occ = age) %>%
  pivot_wider(
    id_cols = c(cohort, occ, sex),
    names_from = var,
    values_from = mean) %>%
  mutate(
    cohort = factor(
      cohort, 
      levels = c("chop", "dnbc", "Combined"),
      labels = c("CHOP", "DNBC", "Combined"),
      ordered = TRUE), 
    sex = factor(
      sex, 
      levels = c("male", "female"), 
      labels = c("Male", "Female")
    )
   )

## ---- CBCL -------------------------------------------------------------------
cbcl_m <- list(cbcl_means_moba_m[[2]], cbcl_means_raine_m[[2]]) %>%
  map_df(function(x){mutate(x, sex = "male")})

cbcl_f <- list(cbcl_means_moba_f[[2]], cbcl_means_raine_f[[2]]) %>%
  map_df(function(x){mutate(x, sex = "female")})

cbcl_means <- bind_rows(cbcl_m, cbcl_f) %>%
  separate(variable, c("var", "age"), sep = "([.])") %>%
  mutate(var = ifelse(var == "ext_raw_", "ext", var)) %>%
  filter(cohort != "Combined") %>%
  rename(occ = age) %>%
  pivot_wider(
    id_cols = c(cohort, occ, sex),
    names_from = var,
    values_from = mean) %>%
  mutate(
    sex = factor(
      sex, 
      levels = c("male", "female"), 
      labels = c("Male", "Female")
    )
  )
  
comb <- cbcl_means %>%
  mutate(cohort = "Combined")

cbcl_means <- bind_rows(cbcl_means, comb) %>%
  mutate(cohort = factor(
    cohort, 
    levels = c("moba", "raine", "Combined"),
    labels = c("MoBa", "Raine", "Combined"),
    ordered = TRUE))

################################################################################
# 7. Analysis 1  
################################################################################

## ---- Linear MLM using CHOP and DNBC -----------------------------------------
ds.asInteger("sdq_sub_m$child_id", "child_id_int", datasources = opals[c("chop", "dnbc")])

ds.dataFrame(
  x = c("sdq_sub_m", "child_id_int"), 
  newobj = "sdq_sub_m", 
  datasources = opals[c("chop", "dnbc")]
)

ds.asInteger("sdq_sub_f$child_id", "child_id_int", datasources = opals[c("chop", "dnbc")])

ds.dataFrame(
  x = c("sdq_sub_f", "child_id_int"), 
  newobj = "sdq_sub_f", 
  datasources = opals[c("chop", "dnbc")]
)

sdq_m <- ds.lmerSLMA(
  dataName = "sdq_sub_m",
  formula = "ext_raw_~ ext_age_ + (1|child_id_int)",
  datasources = opals[c("chop", "dnbc")]
)

sdq_f <- ds.lmerSLMA(
  dataName = "sdq_sub_f",
  formula = "ext_raw_~ ext_age_ + (1|child_id_int)",
  datasources = opals[c("chop", "dnbc")]
)


## ---- Get predicted values ---------------------------------------------------

## Maximum and minimum values
sdq_range <- dh.getStats(
  df = "sdq_sub", 
  vars = "ext_age_",
  cohorts = c("chop", "dnbc"))[[2]] %>%
  select(cohort, perc_5, perc_95)

## Male
pred_chop_sdq_m <- tibble(
  age = (seq(5.48, 11.3, 0.001)), 
  cohort = "chop", 
  predicted = 6.21 + age*-0.00538, 
  sex = "male")

pred_dnbc_sdq_m <- tibble(
  age = (seq(7, 12, 0.001)), 
  cohort = "dnbc",
  predicted = 0.10 + age*0.54143, 
  sex = "male"
)

pred_comb_sdq_m <- tibble(
  age = seq(5.48, 12, 0.001), 
  cohort = "Combined",
  predicted = 3.08 + age*0.27097, 
  sex = "male"
)

## Female
pred_chop_sdq_f <- tibble(
  age = (seq(5.48, 11.3, 0.001)), 
  cohort = "chop", 
  predicted = 4.53 + age *0.00428, 
  sex = "female")

pred_dnbc_sdq_f <- tibble(
  age = (seq(7, 12, 0.001)), 
  cohort = "dnbc", 
  predicted = -1.77 + age*0.68566, 
  sex = "female"
)

pred_comb_sdq_f <- tibble(
  age = (seq(5.48, 12, 0.001)), 
  cohort = "Combined",
  predicted = 1.36 + age*0.34661, 
  sex = "female"
)

pred_sdq <- bind_rows(pred_chop_sdq_m, pred_dnbc_sdq_m, pred_comb_sdq_m,
                      pred_chop_sdq_f, pred_dnbc_sdq_f, pred_comb_sdq_f) %>%
  mutate(
    cohort = factor(
      cohort, 
      levels = c("chop", "dnbc", "Combined"),
      labels = c("CHOP", "DNBC", "Combined"),
      ordered = TRUE), 
    sex = factor(sex, 
      levels = c("male", "female"), 
      labels = c("Male", "Female"), 
      ordered = TRUE))


## ---- Plot models over the top of means --------------------------------------
mlm_theme <-   theme(
  plot.background = element_rect(fill =scales::alpha("#CCCCCC", 0.3)),  #Background outside of plot
  panel.background = element_rect(fill="white"),  #Background for plot
  panel.grid.major=element_line(colour="grey"), #Major and minor gridlines
  panel.grid.minor=element_line(colour="white"), 
  panel.spacing = unit(1, "lines"),
  plot.title = element_text(hjust = 0, vjust=0, size=11, face="bold"), #Plot title, thought don't tend to use
  text=element_text(size=10), #General text 
  axis.title.y = element_text(family="ArialMT", size=11, margin = margin(t = 0, r = 10, b = 0, l = 0)), #Axis labels
  axis.title.x = element_text(family="ArialMT", size=11, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.text.x = element_text(family="ArialMT", size=8, margin = margin(t = 4, r=0, b=0, l=0), colour="black"), #Axis text
  axis.text.y = element_text(family="ArialMT", size=8, margin = margin(t = 0, r=4, b=0, l=0), colour="black"),
  axis.ticks.length=unit(0.3, "cm"),
  axis.ticks = element_line(colour = "grey"),
  strip.text.x = element_text(family="ArialMT", size=11),
  strip.text.y = element_text(family="ArialMT", size=11, face = "bold"),
  strip.background = element_blank(),
  plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm"),
  legend.position = "none") 

ggplot() + 
  geom_point(data = sdq_means, aes(x = age, y = ext), alpha = 0.6) +
  facet_grid(cohort ~ sex) +
  mlm_theme + 
  geom_line(data = pred_sdq, aes(x = age, y = predicted, color = cohort)) +
  scale_y_continuous(limit = c(2, 10), breaks = seq(2, 10, 1), expand = c(0, 0)) + 
  scale_x_continuous(limit = c(4, 14), breaks = seq(4, 16, 1), expand = c(0, 0)) + 
  labs(y = "SDQ Externalising score", x = "Age (years)") +
  scale_colour_manual(values = c(rep("#005690", 2), "#ff2600"))


################################################################################
# 8. Analysis 2: Prep 
################################################################################
opals <- datashield.login(logindata, restore = "mhineq_a_4")

## ---- Make table of polynomials ----------------------------------------------

## First we need to make lots of transformations of the age terms to fit
## different combinations of polynomials

age_comb_m <- "cbcl_sub_m$ext_age_"
age_comb_f <- "cbcl_sub_f$ext_age_"

polys_m = tibble(
  poly = c("age", "age_m_2", "age_m_1", "age_m_0_5", "age_log", "age_0_5", "age_2", "age_3"),
  form = c(
    age_comb_m, 
    paste0(age_comb_m, "^-2"), 
    paste0(age_comb_m, "^-1"), 
    paste0(age_comb_m, "^-0.5"), 
    paste0(age_comb_m, "^0"), 
    paste0(age_comb_m, "^0.5"), 
    paste0(age_comb_m, "^2"), 
    paste0(age_comb_m, "^3"))
) 

polys_f = tibble(
  poly = c("age", "age_m_2", "age_m_1", "age_m_0_5", "age_log", "age_0_5", "age_2", "age_3"),
  form = c(
    age_comb_f, 
    paste0(age_comb_f, "^-2"), 
    paste0(age_comb_f, "^-1"), 
    paste0(age_comb_f, "^-0.5"), 
    paste0(age_comb_f, "^0"), 
    paste0(age_comb_f, "^0.5"), 
    paste0(age_comb_f, "^2"), 
    paste0(age_comb_f, "^3"))
)

polys_m %>%
  pmap(function(poly, form){
    ds.assign(
      toAssign = form, 
      newobj = poly, 
      datasources = opals[c("moba", "raine")]
    )
  })

ds.cbind(
  x = c("cbcl_sub_m", polys_m$poly), 
  newobj = "cbcl_sub_m", 
  datasources = opals[c("moba", "raine")])

polys_f %>%
  pmap(function(poly, form){
    ds.assign(
      toAssign = form, 
      newobj = poly, 
      datasources = opals[c("moba", "raine")]
    )
  })

ds.cbind(
  x = c("cbcl_sub_f", polys_f$poly), 
  newobj = "cbcl_sub_f", 
  datasources = opals[c("moba", "raine")])


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(opals, "mhineq_a_8")
opals <- datashield.login(logindata, restore = "mhineq_a_8")

################################################################################
# 9. Analysis 2: analysis  
################################################################################
ds.asInteger("cbcl_sub_m$child_id", "child_id_int", 
             datasources = opals[c("moba", "raine")])

ds.dataFrame(
  x = c("cbcl_sub_m", "child_id_int"), 
  newobj = "cbcl_sub_m", 
  datasources = opals[c("moba", "raine")])

ds.asInteger("cbcl_sub_f$child_id", "child_id_int", 
             datasources = opals[c("moba", "raine")])

ds.dataFrame(
  x = c("cbcl_sub_f", "child_id_int"), 
  newobj = "cbcl_sub_f", 
  datasources = opals[c("moba", "raine")])


## ---- MoBa -------------------------------------------------------------------

## Here we use a function I wrote to fit all paired combinations of polynomials
cbcl_moba_m <- dh.lmeMultPoly(
  data = "cbcl_sub_m", 
  outcome = "ext_raw_", 
  cohorts = "moba")

cbcl_moba_f <- dh.lmeMultPoly(
  data = "cbcl_sub_f", 
  outcome = "ext_raw_", 
  cohorts = "moba")

## ---- Raine -------------------------------------------------------------------
cbcl_raine_m <- dh.lmeMultPoly(
  data = "cbcl_sub_m", 
  outcome = "ext_raw_", 
  cohorts = "raine")

cbcl_raine_f <- dh.lmeMultPoly(
  data = "cbcl_sub_f", 
  outcome = "ext_raw_", 
  cohorts = "raine")


################################################################################
# 10. Tables  
################################################################################

## ---- Table of best fitting models -------------------------------------------
moba_fit_m <- cbcl_moba_m[[2]] %>% 
  arrange(log_rank) %>% head(5) %>%
  mutate(
    cohort = "MoBa", 
    sex = "Male")

moba_fit_f <- cbcl_moba_f[[2]] %>% 
  arrange(log_rank) %>% head(5) %>%
  mutate(
    cohort = "MoBa", 
    sex = "Female")
  
raine_fit_m <- cbcl_raine_m[[2]] %>% 
  arrange(log_rank) %>% head(5) %>%
  mutate(
    cohort = "Raine", 
    sex = "Male")

raine_fit_f <- cbcl_raine_f[[2]] %>% 
  arrange(log_rank) %>% head(5) %>%
  mutate(
    cohort = "Raine", 
    sex = "Female")

## Three out of the four the best fitting model is the same, which is 
## encouraging. For raine female it's the second-best fitting model

## ---- Fit -------------------------------------------------------------------
cbcl_m.fit <- ds.lmerSLMA(
  dataName = "cbcl_sub_m",
  formula = "ext_raw_~ age_m_0_5 + age_0_5 + (1|child_id_int)", 
  datasources = opals[c("moba", "raine")]
)

cbcl_f.fit <- ds.lmerSLMA(
  dataName = "cbcl_sub_f",
  formula = "ext_raw_~ age_m_0_5 + age_0_5 + (1|child_id_int)", 
  datasources = opals[c("moba", "raine")]
)


## ---- Predicted values for best model ----------------------------------------

## Min and max values
cbcl_range <- dh.getStats(
  df = "cbcl_sub", 
  vars = "ext_age_",
  cohorts = c("moba", "raine"))[[2]] %>%
  select(cohort, perc_5, perc_95)

## Males
moba_m_pred <- tibble(
  age = seq(1.51, 5.33, 0.001), 
  predicted = 96.62 + (age^-0.5)*-67.85 + (age^0.5)*-22.62,
  cohort = "MoBa", 
  sex = "Male")

raine_m_pred <- tibble(
  age = seq(2.04, 10.7, 0.001), 
  predicted = 46.54 + (age^-0.5)*-36.60 + (age^0.5)*-7.98,
  cohort = "Raine", 
  sex = "Male")

combined_m_pred <- tibble(
  age = seq(1.53, 10.7, 0.001), 
  predicted = 71.77 + (age^-0.5)*-52.54 + (age^0.5)*-15.33,
  cohort = "Combined", 
  sex = "Male")


## Females
moba_f_pred <- tibble(
  age = seq(1.51, 5.33, 0.001),
  predicted = 99.52 + (age^-0.5)*-70.47 + (age^0.5)*-23.56,
  cohort = "MoBa", 
  sex = "Female")

raine_f_pred <- tibble(
  age = seq(2.04, 10.7, 0.001), 
  predicted = 30.47 + (age^-0.5)*-20.05 + (age^0.5)*-5.17,
  cohort = "Raine", 
  sex = "Female")

combined_f_pred <- tibble(
  age = seq(1.53, 10.7, 0.001), 
  predicted = 65.11 + (age^-0.5)*-45.41 + (age^0.5)*-14.39,
  cohort = "Combined", 
  sex = "Female")

cbcl_pred <- bind_rows(
  moba_m_pred,
  moba_f_pred,
  raine_m_pred,
  raine_f_pred, 
  combined_m_pred,
  combined_f_pred
) %>%
  mutate(
    sex = factor(sex, levels = c("Male", "Female"), ordered = TRUE),
    cohort = factor(cohort, levels = c("MoBa", "Raine", "Combined"), 
                    ordered = TRUE)
  )


## ---- Plot -------------------------------------------------------------------
ggplot() +
  geom_point(
    data = cbcl_means, aes(x = age, y = ext), size = 0.8, alpha = 0.6) +
  forest_theme + 
  facet_grid(cohort ~ sex) +
  geom_line(data = cbcl_pred, aes(x = age, y = predicted, color = cohort)) +
  scale_x_continuous(limit = c(1, 14), breaks = seq(1, 14, 1), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(0, 20), breaks = seq(0, 20, 2), expand = c(0, 0)) +
  labs(y = "CBCL Externalising score", x = "Age (years)") +
  scale_colour_manual(values = c(rep("#005690", 2), "#ff2600"))

