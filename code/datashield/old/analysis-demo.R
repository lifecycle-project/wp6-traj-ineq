################################################################################
## Project: wp6-traj-ineq
## Script purpose: Describe available data
## Date: 30th September 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(remotes)
install_github("sidohaakma/ds-helper", ref = "fix/do-not-use-opals-global-object")
library(dsHelper)

################################################################################
# 1. Load workspace  
################################################################################
opals <- datashield.login(logindata, restore = "traj_assigned_23_10_20")

################################################################################
# 2. Visualise data using DS scatter plots  
################################################################################
ds.scatterPlotJOHAN(
  x = "sdq_sub$ext_age_", 
  y = "sdq_sub$ext_raw_", 
  datasources = opals[c("chop", "dnbc")]
)

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

ds.scatterPlotTIM(
  x = "sdq_sub$ext_age_", 
  y = "sdq_sub$ext_raw_", 
  datasources = opals[c("chop", "dnbc")]
)

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

datashield.workspace_save(opals, "mhineq_means_d")

#dh.makeOutcome(
#  df = "cbcl_sub", 
#  outcome = "ext_raw_", 
#  age_var = "ext_age_", 
#  bands = c(7, 8, 8, 12), 
#  df_name = "cbcl_sub_all", 
#  keep_original = TRUE, 
#  cohorts = "inma"
#)
  
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
forest_theme <-   theme(
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

palette_khl <- c("#197d78", "#d79632", "#325573", "#d74632", "#d77896", 
                 "#827d78", "#4by6be", "#1e8c32")

ggplot() + 
  geom_point(data = sdq_means, aes(x = age, y = ext), alpha = 0.6) +
  facet_grid(cohort ~ sex) +
  forest_theme + 
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

ds.summary("cbcl_sub_m", datasources = opals[c("moba", "raine")])
ds.summary("cbcl_sub_f", datasources = opals[c("moba", "raine")])

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




#write.csv(loglik.tab, "./tables/moba_fit.csv")

#best <- cbcl_moba[[1]][order(cbcl_moba[[2]]$log_rank)]

#best[[1]]

## ---- Separate best models ---------------------------------------------------
#best_moba_m <- cbcl_moba_m[[1]][order(cbcl_moba_m[[2]]$log_rank)][[1]]
#best_moba_f <- cbcl_moba_f[[1]][order(cbcl_moba_f[[2]]$log_rank)][[1]]
#best_raine_m <- cbcl_raine_m[[1]][order(cbcl_raine_m[[2]]$log_rank)][[1]]
#best_raine_f <- cbcl_raine_f[[1]][order(cbcl_raine_f[[2]]$log_rank)][[1]]


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





################################################################################
# 11. Analysis 3: Percentiles by maternal education  
################################################################################
perc_m.fit <- ds.lmerSLMA(
  dataName = "mat_ed_sub_m",
  formula = "ext_pc_~ ext_age_ + mat_ed_rank + mat_ed_age + (1|id_int)", 
)

perc_f.fit <- ds.lmerSLMA(
  dataName = "mat_ed_sub_f",
  formula = "ext_pc_~ ext_age_ + mat_ed_rank + mat_ed_age + (1|id_int)", 
)

## ---- Extract coefficients ---------------------------------------------------
betas_m <- as_tibble(perc_m.fit$betamatrix.valid) %>%
  mutate(combined = perc_m.fit$SLMA.pooled.ests.matrix[, "pooled.ML"]) %>%
  mutate(variable = row.names(perc_m.fit$SLMA.pooled.ests.matrix))

colnames(betas_m) <- c(names(opals), "combined", "variable")

betas_m %<>% select(variable, everything())

betas_m %<>% pivot_longer(
  cols = -variable,  
  names_to = "cohort", 
  values_to = "beta"
  )

betas_f <- as_tibble(perc_f.fit$betamatrix.valid) %>%
  mutate(combined = perc_f.fit$SLMA.pooled.ests.matrix[, "pooled.ML"]) %>%
  mutate(variable = row.names(perc_f.fit$SLMA.pooled.ests.matrix))

colnames(betas_f) <- c(names(opals), "combined", "variable")

betas_f %<>% select(variable, everything())

betas_f %<>% pivot_longer(
  cols = -variable,  
  names_to = "cohort", 
  values_to = "beta"
)

  
  edu_pred <- function(x, data){
    
    pred <- tibble(
      age = seq(0, 16, 0.001),
      cohort = x,
      intercept = data %>% filter(cohort == x & variable == "(Intercept)") %>% pull(beta),
      mat_ed = data %>% filter(cohort == x & variable == "mat_ed_rank") %>% pull(beta),
      mat_ed_age = data %>% filter(cohort == x & variable == "mat_ed_age") %>% pull(beta)*age, 
      predicted = mat_ed + mat_ed_age)
      
    return(pred)
  }
  
edu_m.pred <- c(names(opals), "combined") %>% 
  map_df(edu_pred, data = betas_m) %>%
  mutate(sex = "male")

edu_f.pred <- c(names(opals), "combined") %>% 
  map_df(edu_pred, data = betas_f) %>%
  mutate(sex = "female")

edu.pred <- bind_rows(edu_m.pred, edu_f.pred) 


## ---- Plot -------------------------------------------------------------------
ggplot(data = edu.pred, aes(x = age, y = predicted)) +
  forest_theme + 
  facet_grid(cohort ~ sex) +
  geom_line() +
  scale_x_continuous(limit = c(0, 16), breaks = seq(1, 14, 1), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-0.3, 0.3), breaks = seq(-0.4, 0.4, 0.2), expand = c(0, 0)) +
  labs(y = "Externalising Percentile", x = "Age (years)") + 
  theme(legend.position = "top")
## Ok so my interaction term is meaningless I think, as it's based on the 
## factor levels, eg. 1, 2 & 3. Need to go the SII route I think.






table(predicted$edu_cat)

print(predicted, n = 100)


betas_m

perc_m_sep 





moba_f_pred <- tibble(
  age = seq(0, 8, 0.001), 
  predicted = 99.52 + (age^-0.5)*-70.47 + (age^0.5)*-23.56,
  cohort = "MoBa", 
  sex = "Female")


str(betas_m_sep)


################################################################################
# 2. First we visualise just with MoBa  
################################################################################
ds.scatterPlot(
  x = "cbcl_ext_rep$ext_age_", 
  y = "cbcl_ext_rep$ext_raw_")
  
ds.table("cbcl_ext_rep$n_repeated_obs")

## Also using my function
dh.binRM(
  df = "cbcl_ext", 
  outcome = "ext_raw_", 
  age_var = "ext_age_", 
  upper = 0,
  lower = 17,
  increment = 2
)


h1

## I think the counts



################################################################################
# 3. Now run a really simple model  
################################################################################
ds.asInteger("cbcl_ext_rep$child_id", "child_id_int")

ds.dataFrame(
  x = c("cbcl_ext_rep", "child_id_int"), 
  newobj = "cbcl_ext_rep"
)

ds.lmerSLMA(
  dataName = "cbcl_ext_rep",
  formula = "ext_raw_~ ext_age_ + (1|child_id_int)"
)


################################################################################
# 4. Get predicted values  
################################################################################


ds.summary("cbcl_sub$ext_age_")



################################################################################
# 2. Use ds scatter function  
################################################################################
sdq_coh <- c("ninfea", "chop", "inma", "dnbc")
cbcl_coh <- c("chop", "raine", "moba", "inma")

## ---- SDQ -------------------------------------------------------------------
ds.scatterPlot(
  x = "sdq_sub$int_age_", 
  y = "sdq_sub$int_raw_", 
  datasources = opals[sdq_coh])

ds.scatterPlot(
  x = "cbcl_sub$int_age_", 
  y = "cbcl_sub$int_raw_", 
  datasources = opals[cbcl_coh])

test <- ds.scatterPlot(
  x = "cbcl_sub$int_age_", 
  y = "cbcl_sub$int_raw_")

ds.summary("cbcl_sub$adhd_age")

str(test)

ds.scatterPlot(
  x = "mhrep$int_age_", 
  y = "mhrep$int_pc_", 
  method = "probabilistic")


################################################################################
# 3. Write function to show average value within age bins  
################################################################################

## ---- Function to get data ---------------------------------------------------
dh.binRM <- function(df, outcome_var, age_var, upper, lower, increment){
  
  dh.makeOutcome(
    df = df, 
    outcome = outcome_var, 
    age_var = age_var, 
    bands = c(lower, rep(seq(lower+increment, upper-increment, increment), each = 2), upper), 
    mult_action = "earliest")
  
  outvar <- paste0(outcome_var, "_derived")
  
  ds.dataFrameFill(outvar, "filled")
  
  stats <- dh.getStats(
    df = "filled", 
    vars = ds.colnames("filled")[[1]])
  
  to_plot <- separate(
    data = stats$continuous, 
    col = "variable",
    into = c("type", "age"), 
    sep = "\\.") %>%
    mutate(age = as.numeric(age))
  
  return(to_plot)
  
}
 

## ---- Function to plot data --------------------------------------------------
dh.plotBin <- function(obj, outcome, title, x_label, y_label, x_range, x_inc, y_range, y_inc){
  
require(ggplot2)

## Create theme 
theme_khl <- theme(
  plot.background = element_rect(fill =scales::alpha("#CCCCCC", 0.3)),  #Background outside of plot
  panel.background = element_rect(fill="white"),  #Background for plot
  panel.grid.major=element_line(colour="grey"), #Major and minor gridlines
  panel.grid.minor=element_line(colour="white"), 
  panel.spacing = unit(1, "lines"),
  plot.title = element_text(hjust = 0, vjust=0, size=11, face="bold"), #Plot title, thought don't tend to use
  text=element_text(size=10), #General text 
  axis.title.y = element_text(size=11, margin = margin(t = 0, r = 10, b = 0, l = 0)), #Axis labels
  axis.title.x = element_text(size=11, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.text.x = element_text(size=8, margin = margin(t = 4, r=0, b=0, l=0), colour="black"), #Axis text
  axis.text.y = element_text(size=8, margin = margin(t = 0, r=4, b=0, l=0), colour="black"),
  axis.ticks.length=unit(0.3, "cm"),
  axis.ticks = element_line(colour = "grey"),
  strip.text.x = element_text(size=11),
  strip.background = element_blank(),
  legend.background= element_rect(fill=scales::alpha("#CCCCCC", 0.03)), #Legend background colour
  legend.title=element_text(size=8, face="bold"), #Legend title
  legend.text=element_text(size=8), #Legend text
  legend.position="top", #Legend position
  legend.direction="vertical", #Legend stacking
  legend.justification = "left", #Left justify legend
  legend.key.width = unit(3, "line"), #Make amount of line displayed in legend longer
  legend.margin=margin(t=-1, r=0, b=0.2, l=0, unit="cm"), #Margin around legend
  plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) #Margin around (everything) in the plot
  

## Create palette 
palette_khl <- c("#197d78", "#d79632", "#325573", "#d74632", "#d77896", 
                   "#827d78", "#4by6be", "#1e8c32")
  
## Create plot 
  
plot <- ggplot(
  data = obj %>% filter(type == outcome & cohort != "Combined"), aes(x = age, y = mean)) +
  geom_point(aes(size = valid_n)) +
  facet_wrap(vars(cohort), ncol = 1) +
  theme_khl +
  labs(title = title, y = y_label, x = x_label) +
  scale_x_continuous(limit = x_range, breaks = seq(x_range[1], x_range[2], x_inc), expand = c(0, 0)) + 
  scale_y_continuous(limit = y_range, breaks = seq(y_range[1], y_range[2], y_inc), expand = c(0, 0)) +
  scale_size_continuous(range = c(0.5, 2)) 
  

} 
  
  prelim <- ggplot(
    data = to_plot %>% filter(type == outvar & cohort != "Combined"), aes(x = age, y = mean)) +
    geom_point(aes(size = valid_n)) +
    facet_wrap(vars(cohort), ncol = 1) +
    theme_khl +
    labs(title = "test", y = "Internalising", x = "Age") +
    scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 1)) + 
    scale_y_continuous(limit = c(0, 4), breaks = seq(0, 4, 1)) +
    scale_size_continuous(range = c(0.5, 2))  
  

}
  
  
  
  
  
  
  
  

dh.makeOutcome(
  df = "sdq_sub", 
  outcome = "int_raw_", 
  age_var = "int_age_", 
  bands = c(0, rep(seq(2, 17, 2), each = 2), 18), 
  mult_action = "earliest")

ds.dataFrameFill("int_raw__derived", "test")

practice <- dh.getStats(
  df = "test", 
  vars = ds.colnames("test")[[1]])

to_plot <- separate(
  data = practice$continuous, 
  col = "variable",
  into = c("type", "age"), 
  sep = "\\.") %>%
  mutate(age = as.numeric(age))

prelim <- ggplot(
  data = to_plot %>% filter(type == "int_raw_" & cohort != "Combined"), aes(x = age, y = mean)) +
  geom_point(aes(size = valid_n)) +
  facet_wrap(vars(cohort), ncol = 1) +
  theme_khl +
  labs(title = "test", y = "Internalising", x = "Age") +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 1)) + 
  scale_y_continuous(limit = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_size_continuous(range = c(0.5, 2))



################################################################################
# Show available data based on percentiles  
################################################################################
ds.summary("mhrep$int_pc")

ds.summary("mhrep")

ds.scatterPlot(
  x = "sdq_sub$int_age_", 
  y = "sdq_sub$int_pc_", 
  datasources = opals[c("ninfea", "chop", "raine", "moba", "inma", "dnbc")])

################################################################################
# 4. Save workspace  
################################################################################
datashield.workspace_save(opals, "traj_show_data_30_09_20")
