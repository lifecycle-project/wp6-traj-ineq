################################################################################
## Project: traj-ineq
## Script purpose: Produce tables for manuscipt
## Date: 17th August 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################    
library(dsHelper)
library(here)

withr::with_libpaths(new = "~/R/userlib",
                     devtools::install_github("timcadman/datashield-tim"))

withr::with_libpaths(new = "~/R/userlib",
                     devtools::install_github("lifecycle-project/ds-helper", 
                                              ref = "dev"))

conns <- datashield.login(logindata, restore = "mh_traj")
################################################################################
# METHODS  
################################################################################
################################################################################
# Get dimensions of data  
################################################################################

## ---- Functions --------------------------------------------------------------

## Wrapper for ds.dim
dimSimp <- function(x){ds.dim(x, type = "c")[[1]][[1]]}

## Get completed cases
getCompleteCases <- function(dims, label){
  
  list(
    x = dims, y = list(
      int.coh, ext.coh, adhd.coh)) %>%
    pmap(function(x, y){
      
      .dimNeat(x, label, y)
      
    }) %>%
    set_names(miss.ref$outcome) %>%
    bind_rows(.id = "stat") %>%
    pivot_wider(
      names_from = "cohort", 
      values_from = label) %>%
    dplyr::select(stat, combined) %>%
    mutate(stat = case_when(
      stat == "int_raw_" ~ "int_obs",
      stat == "ext_raw_" ~ "ext_obs",
      stat == "adhd_raw_" ~ "adhd_obs")) %>%
    dplyr::rename(!!sym(label) := combined)
  
}

## ---- Complete cases, long format --------------------------------------------
analysis_l_dims <- miss.ref %>%
  pmap(function(inc_long, cohort, ...){
    
    ds.dim(inc_long, datasources = conns[cohort])
    
  })

## ---- Complete cases, wide format --------------------------------------------
analysis_w_dims <- miss.ref %>%
  pmap(function(inc_wide, cohort, ...){
    
    ds.dim(inc_wide, datasources = conns[cohort])
    
  })

## ---- Dimensions, different exclusions ---------------------------------------
sample_dim <- ds.dim("nonrep")
baseline_n_w <- dimSimp("baseline_df_w")
any_exp_n_w <- dimSimp("any_exp_df_w")
any_out_n_w <- dimSimp("any_out_df_w")
analysis_n_w <- dimSimp("analysis_df_w")

## ---- Table for complete cases -----------------------------------------------
cc.tab <- left_join(
  getCompleteCases(analysis_w_dims, "n_sub"), 
  getCompleteCases(analysis_l_dims, "n_obs"), 
  by = "stat")

################################################################################
# Text
################################################################################

## ---- Included cohorts -------------------------------------------------------
included_coh <- sort(
  c(int.coh, ext.coh, adhd.coh)) %>%
  unique

length(included_coh)

## ---- Maximum sample size ----------------------------------------------------
max_n <- max(cc.tab$n_sub)

################################################################################
# Figure 1  
################################################################################
################################################################################
# Left column
################################################################################

## ---- Baseline sample --------------------------------------------------------
baseline_n_w 

## ---- Some exposures ---------------------------------------------------------
any_exp_n_w 

## ---- Some outcomes ----------------------------------------------------------
any_out_n_w 

## ---- All core covariates ----------------------------------------------------
analysis_n_w <

################################################################################
# Right column  
################################################################################

## ---- Excluded: no maternal education data -----------------------------------
baseline_n_w - any_exp_n_w

## ---- Excluded: no mental health data ----------------------------------------
any_exp_n_w - any_out_n_w

## ---- Complete cases ---------------------------------------------------------
cc.tab

################################################################################
# Table 1 same style as Robyn's  
################################################################################
.dimNeat <- function(dim, value_name, coh_names){
  
  dim %>%
  set_names(c(coh_names, "combined")) %>%
    map(., ~.x[1][1]) %>%
    map(., as_tibble) %>%
    bind_rows(.id = "cohort") %>%
    rename(!!value_name := value) 
  
}

## ---- Total sample N ---------------------------------------------------------
sample_n <- .dimNeat(sample_dim, "n_cohort", names(conns)) %>%
  pivot_wider(
    names_from = cohort, 
    values_from = n_cohort) %>%
  mutate(stat = "sample_n") %>%
  dplyr::select(-combined)
  
## ---- Analysis N participants ------------------------------------------------
analysis_ns <- list(
  x = analysis_w_dims, y = list(
    int.coh, ext.coh, adhd.coh)) %>%
  pmap(function(x, y){
    
    .dimNeat(x, "analysis_n_obs", y)
      
  }) %>%
  set_names(miss.ref$outcome) %>%
  bind_rows(.id = "stat") %>%
  pivot_wider(
    names_from = "cohort", 
    values_from = "analysis_n_obs") %>%
  dplyr::select(-combined)

## ---- Analysis N occasions ---------------------------------------------------
occ.ref <- meas.ref %>%
  group_by(cohort, outcome) %>%
  group_split %>%
  map(~tibble(
    cohort = .x$cohort[1], 
    outcome = .x$outcome[1], 
    occasions = length(.x$occasion))) %>%
  bind_rows
    
occasions.tab <- occ.ref %>%
  pivot_wider(
    names_from = "cohort", 
    values_from = "occasions") %>%
  dplyr::rename(stat = outcome) %>%
  mutate(stat = case_when(
    stat == "int" ~ "int_occ",
    stat == "ext" ~ "ext_occ",
    stat == "adhd" ~ "adhd_occ"))

## ---- Analysis N observations ------------------------------------------------
analysis_ns_l <- list(
  x = analysis_l_dims, y = list(
    int.coh, ext.coh, adhd.coh)) %>%
  pmap(function(x, y){
    
    .dimNeat(x, "analysis_n_obs_l", y)
    
  }) %>%
  set_names(miss.ref$outcome) %>%
  bind_rows(.id = "stat") %>%
  pivot_wider(
    names_from = "cohort", 
    values_from = "analysis_n_obs_l") %>%
  dplyr::select(-combined) %>%
  mutate(stat = case_when(
    stat == "int_raw_" ~ "int_obs",
    stat == "ext_raw_" ~ "ext_obs",
    stat == "adhd_raw_" ~ "adhd_obs"))

## ---- Stats for next section -------------------------------------------------
tab1.vars <- c("sex", "edu_m", "eusilc_income_quintiles", "eth_bin")

tab1.stats <- dh.getStats(
  df = "analysis_df_w", 
  vars = tab1.vars)

other.tab <- dh.createTableOne(
  stats = tab1.stats, 
  vars = c(tab1.vars, "eth_bin"),
  type = "both", 
  coh_direction = "cols", 
  cont_stats = "med_iqr", 
  inc_missing = FALSE, 
  perc_denom = "valid") %>%
  dplyr::select(-"combined") %>%
  dplyr::filter(
    variable == "sex" & category == "2" |
    variable == "edu_m" & category %in% c("2", "3") |
    variable == "eth_bin" & category == "2" |
    variable == "eusilc_income_quintiles") %>%
  mutate(stat = paste0(variable, "_", category)) %>%
  dplyr::select(-category, -variable)

## ---- Population, year or birth, age range -----------------------------------
cohort.tab <- dt.cohortTable(
  ref = "~/cohort_info.csv", 
  dataset = "analysis_df_w", 
  conns = conns)

cohort.tab <- cohort.tab %>% 
  dplyr::select(cohort, country, city_area, birth_years, ages) %>%
  pivot_longer(
    cols = c(country, city_area, birth_years, ages),
    names_to = "stat", 
    values_to = "value") %>%
  pivot_wider(
    names_from = "cohort",
    values_from = "value")

## ---- Put it together --------------------------------------------------------
tab1.tab <- bind_rows(sample_n, analysis_ns, analysis_ns_l, occasions.tab) %>%
  mutate(across(everything(), ~as.character(.))) %>%
  bind_rows(.,  other.tab) %>%
  bind_rows(., cohort.tab) %>%
  dplyr::select(stat, everything()) %>%
  mutate(stat = factor(stat, levels = c(
    "country", "city_area", "birth_years", "ages", "sample_n", "int_raw_", 
    "ext_raw_", "adhd_raw_", "int_occ", "ext_occ", "adhd_occ", "int_obs", 
    "ext_obs", "adhd_obs","sex_2", "eth_bin_2", "edu_m_2", "edu_m_3", 
    "eusilc_income_quintiles"), 
    ordered = TRUE)) %>%
  arrange(stat) 

tab1.tab %>% print(n = Inf)

write_csv(tab1.tab, file = here("tables", "table_1.csv"))

################################################################################
# Ethnicity information  
################################################################################
eth.tab <- eth.stats$categorical %>%
  dplyr::filter(cohort != "combined") %>%
  dplyr::select(cohort, category, value, perc_total) %>%
  print(n = Inf)

eth.tab %>% dplyr::filter(value > 20 & category == 2) 

eth.tab %>%
  dplyr::filter(cohort %in% c("alspac", "genr", "raine") & !is.na(category)) %>%
  summarise(eth_n = sum(value)) %>%
  mutate(
    total_n = max_n, 
    eth_perc = (eth_n / total_n)*100)

################################################################################
# Table S1: Missingness table based on Robyn's 
################################################################################
conns <- datashield.login(logindata, restore = "mh_traj")

## ---- Get included stats -----------------------------------------------------  
included.stats <- miss.ref %>%
  pmap(function(inc_wide, cohort, ...){
    
    dh.getStats(
      df = inc_wide, 
      vars = tab1.vars, 
      conns = conns[unlist(cohort)])
    
    })

## ---- Get excluded stats -----------------------------------------------------
excluded.stats <- miss.ref %>%
  pmap(function(exc_wide, cohort, ...){
    
    dh.getStats(
      df = exc_wide, 
      vars = tab1.vars, 
      conns = conns[unlist(cohort)])
    
    })

save.image()

ds.table("ext_sub_w_exc$edu_m")
ds.class("ext_sub_w_exc$edu_m")

## ---- Make the table ---------------------------------------------------------
cat_labs.ref <- tibble(
  variable = c(rep("edu_m", 3), rep("sex", 2), rep("ethn3_m", 2)),  
  category = c(c(1, 2, 3), c(1, 2), c(1, 2)),
  cat_label = c(c("Low", "Medium", "High"), c("Male", "Female"), 
              c("Western European", "Non-western European")))

included.tab <- included.stats %>%
  set_names(miss.ref$outcome) %>%
  map(
    ~dh.createTableOne(
      stats = .x,
      vars = tab1.vars,
      type = "both", 
      coh_direction = "row", 
      cont_stats = "med_iqr", 
      inc_missing = TRUE, 
      round_digits = 1, 
      perc_denom = "valid")) %>%
  map(., ~mutate(., across(everything(), ~as.character(.)))) %>%
  bind_rows(.id = "outcome") %>%
  dplyr::filter(cohort != "combined") %>%
  mutate(sample = "included")

excluded.tab <- excluded.stats %>%
  set_names(miss.ref$outcome) %>%
  map(
    ~dh.createTableOne(
      stats = .x,
      vars = tab1.vars,
      type = "both", 
      coh_direction = "row", 
      cont_stats = "med_iqr", 
      inc_missing = TRUE, 
      round_digits = 1, 
      perc_denom = "valid"))) %>%
  map(., ~mutate(., across(everything(), ~as.character(.)))) %>%
  bind_rows(.id = "outcome") %>%
  dplyr::filter(cohort != "combined") %>%
  mutate(sample = "not_included")

tabs1.tab <- bind_rows(included.tab, excluded.tab) %>%
  dplyr::select(-data_type) %>%
  dplyr::filter(cohort != "elfe") %>%
  pivot_wider(
    names_from = c("cohort", "sample"), 
    values_from = "value") %>%
  dplyr::select(outcome, variable, category, sort(tidyselect::peek_vars()))

write_csv(tabs1.tab, file = here("code/datashield/bmj/tables", "table_s1.csv"))

save.image()

################################################################################
# Table S2: Fit statistics for final models
################################################################################
fit.tab <- list(int.fit$fit, ext.fit$fit, adhd.fit$fit, asd.fit$fit, 
                lan.fit$fit, nvi.fit$fit) %>%
  map(~slice(., 1)) %>%
  set_names("Internalising", "Externalisin", "ADHD", "ASD", "Language", "NVI") %>%
  bind_rows(.id = "outcome") %>%
  dplyr::rename(combined = sum_log) %>%
  mutate(across(alspac:combined, ~round(., 1)))

write.csv(x = fit.tab, file = here("tables", "fit_stats.csv"))

################################################################################
# Table S3: Fixed effects coefficients for final models  
################################################################################
outcome_coh <- list(int_coh, ext_coh, adhd_coh[adhd_coh != "ninfea"], 
                    asd_coh[asd_coh != "inma"], lan_coh, nvi_coh)


coefs.tab <- list(
  fit = list(int.fit, ext.fit, adhd.fit, asd.fit, lan.fit, nvi.fit),
  coh = outcome_coh) %>%
  pmap(function(fit, coh){
    
    best.model <- fit$model[fit$fit$model[1]][[1]]
    
    coefs <- dh.lmTab(
      model = best.model, 
      type = "lmer_slma", 
      direction = "wide", 
      coh_names = coh, 
      ci_format = "paste", 
      digits = 1)
    
    fixed <- coefs$fixed %>%
      dplyr::select(cohort, variable, est) %>%
      pivot_wider(
        names_from = "cohort",
        values_from = "est")
    
    random <- coefs$random_sd %>% 
      dplyr::select(cohort, coefficient, stddev) %>%
      mutate(level = "random")
    
    resid <- coefs$resid_sd %>%
      dplyr::rename(stddev = res_std) %>%
      mutate(
        coefficient = "(Intercept)", 
        level = "residual")
    
    random <- bind_rows(random, resid)
    
    return(list(fixed = fixed, random = random))
    
  }) %>% 
  set_names(
    "Internalising", "Externalising", "ADHD", "ASD", "Language", "NVI")

## ---- Fixed effects ----------------------------------------------------------
fixed.tab <- coefs.tab %>% map(~.x$fixed) %>%
  bind_rows(.id = "outcome") 

write_csv(fixed.tab, file = here("tables", "best_mod_fixed.csv"))

################################################################################
# Table S4: random effects for best fitting models  
################################################################################
random.tab <- coefs.tab %>% map(~.x$random) %>%
  bind_rows(.id = "outcome") %>%
  pivot_wider(
    names_from = cohort, 
    values_from = stddev)

write_csv(random.tab, file = here("tables", "best_mod_random.csv"))

################################################################################
# Predicted values  
################################################################################
################################################################################
# Separate best models  
################################################################################
mod_names <- c("int", "ext", "adhd", "asd", "lan", "nvi")

best.fit <- list(int.fit, ext.fit, adhd.fit, asd.fit, lan.fit, nvi.fit) %>%
  map(~.x$model[.x$fit$model[1]][[1]]) %>%
  set_names(mod_names)

################################################################################
# Create new data  
################################################################################
pred_m.tab <- list(
  int = tibble(
    intercept = 0,
    age = seq(0, 18, by = 0.01),
    int_age__m_2 = 0,
    int_age__m_1 = 0,
    edu_rank_num = 1,
    "int_age__m_2:edu_rank_num" = (age^-2)*edu_rank_num,
    "int_age__m_1:edu_rank_num" = (age^-1)*edu_rank_num), 
  ext = tibble(
    intercept = 0,
    age = seq(0, 18, by = 0.01),
    ext_age__m_1 = 0,
    ext_age__m_0_5 = 0,
    edu_rank_num = 1,
    "ext_age__m_1:edu_rank_num" = (age^-1)*edu_rank_num,
    "ext_age__m_0_5:edu_rank_num" = (age^-0.5)*edu_rank_num), 
  adhd = tibble(
    intercept = 0,
    age = seq(0, 18, by = 0.01),
    adhd_age__m_0_5 = 0,
    adhd_age__0_5 = 0,
    edu_rank_num = 1,
    "adhd_age__m_0_5:edu_rank_num" = (age^-0.5)*edu_rank_num,
    "adhd_age__0_5:edu_rank_num" = (age^0.5)*edu_rank_num), 
  asd = tibble(
    intercept = 0,
    age = seq(0, 18, by = 0.01),
    asd_age__m_2 = 0,
    asd_age__m_1 = 0,
    edu_rank_num = 1,
    "asd_age__m_2:edu_rank_num" = (age^-2)*edu_rank_num,
    "asd_age__m_1:edu_rank_num" = (age^-1)*edu_rank_num), 
  lan = tibble(
    intercept = 0,
    age = seq(0, 18, by = 0.01),
    lan_age__m_2 = 0,
    lan_age__m_1 = 0,
    edu_rank_num = 1,
    "lan_age__m_2:edu_rank_num" = (age^-2)*edu_rank_num,
    "lan_age__m_1:edu_rank_num" = (age^-1)*edu_rank_num),
  nvi = tibble(
    intercept = 0,
    age = seq(0, 18, by = 0.01),
    nvi_age__m_2 = 0,
    nvi_age__m_1 = 0,
    edu_rank_num = 1,
    "nvi_age__m_2:edu_rank_num" = (age^-2)*edu_rank_num,
    "nvi_age__m_1:edu_rank_num" = (age^-1)*edu_rank_num))

################################################################################
# Get predicted values  
################################################################################
predicted <- list(model = best.fit, newdata = pred.tab, coh = outcome_coh) %>%
  pmap(function(model, newdata, coh){
    
    dh.predictLmer(
      model = model,
      new_data = newdata, 
      coh_names = coh)
    
  }) %>%
  set_names(mod_names)

################################################################################
# Trim data  
################################################################################
age_vars <- c("int_age_", "ext_age_", "adhd_age_", "asd_age_", "lan_age_", 
              "nvi_age_")
## ---- Get min and max ages for each outcome ----------------------------------
ages <- dh.getStats(
  df = "analysis_df_l", 
  vars = age_vars)

save.image()

ages <- ages$continuous %>%
  dplyr::select(variable, cohort, perc_5, perc_95) %>%
  mutate(variable = factor(variable, levels = age_vars, ordered = TRUE)) %>%
  arrange(variable) %>%
  group_by(variable) %>%
  group_split %>%
  set_names(mod_names)
  
pred_trimmed <- list(predicted = predicted, ages = ages) %>%
  pmap(function(predicted, coh, ages){
    
    dh.trimPredData(
      pred = predicted,
      coh_names = ages$cohort,
      min = ages$perc_5, 
      max = ages$perc_95) %>%
      dplyr::filter(cohort != "combined") %>%
      left_join(., names_neat)
    
  })

pred_trimmed %>%
  bind_rows(.id = "outcome")

################################################################################
# Table S5: Predicted values at different ages  
################################################################################

## ---- Function ---------------------------------------------------------------
predByAge <- function(predicted){
  
  predicted %>% 
    mutate(
      group = case_when(
        age == 2  ~ "age_2", 
        age == 4  ~ "age_4", 
        age == 6  ~ "age_6", 
        age == 8  ~ "age_8",
        age == 10  ~ "age_10",
        age == 12  ~ "age_12",
        age == 14  ~ "age_14",
        age == 16  ~ "age_16")) %>%
    mutate(across(c(predicted, low_ci, upper_ci), ~round(., 1))) %>%
    dplyr::filter(age %in% c(2, 4, 6, 8, 10, 12, 14, 16)) %>%
    mutate(est = paste0(predicted, " (", low_ci, ", ", upper_ci, ")")) %>%
    left_join(., names_neat) %>%
    dplyr::select(group, cohort_neat, est) 
  
}

## ---- To the business --------------------------------------------------------
pred_ages <- pred_trimmed %>% 
  map(~predByAge(.x)) %>%
  bind_rows(.id = "outcome") %>%
  pivot_wider(
    names_from = "group", 
    values_from = "est") %>%
  mutate(across(everything(), ~replace_na(., "_"))) %>%
  dplyr::select(outcome, cohort_neat, "age_2", "age_4", "age_6", "age_8", "age_10", 
                "age_12", "age_14", "age_16")

write_csv(pred_ages, file = here("tables", "pred_ages.csv"))

################################################################################
# PLOTS  
################################################################################
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/lc-names-neat.R")
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/themes.R")

## ---- Function ---------------------------------------------------------------
siiPlot <- function(predicted, y_low = -20, y_high = 40){
  
  ggplot() + 
    geom_line(
      data = predicted, 
      aes(x = age, y = predicted, colour = cohort_neat), 
      size = 0.4) +
    geom_ribbon(
      data = predicted, 
      aes(x = age, ymin = low_ci, ymax = upper_ci), 
      alpha = 0.1) +
    facet_wrap(~cohort_neat, ncol = 3) +
    scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 4), expand = c(0, 0)) + 
    scale_y_continuous(limit = c(y_low, y_high), breaks = seq(y_low, y_high, 20), expand = c(0, 0)) +
    theme_traj +
    theme(
      legend.position = "none", 
      axis.title.y = element_text(size = 8),
      axis.title.x = element_text(size = 8),
      axis.text.x = element_text(size = 6), 
      axis.text.y = element_text(size = 6), 
      strip.text.x = element_text(size = 6)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
    geom_hline(yintercept = 20, alpha = 0.05) +
    xlab("Child age") +
    ylab("Slope Index of Inequality") +
    scale_colour_manual(values = palette_ext)
  
}

## ---- Plots ------------------------------------------------------------------
int.plot <- siiPlot(pred_trimmed$int)
ext.plot <- siiPlot(pred_trimmed$ext)
adhd.plot <- siiPlot(pred_trimmed$adhd)
asd.plot <- siiPlot(pred_trimmed$asd)

## Intelligence
intelligence.pdata <- bind_rows(
  pred_trimmed$lan %>% mutate(outcome = "Verbal"),
  pred_trimmed$nvi %>% mutate(outcome = "Non-verbal"))
  
intelligence.plot <- ggplot() + 
  geom_line(
    data = intelligence.pdata, 
    aes(x = age, y = predicted, colour = cohort_neat), 
    size = 0.4) +
  geom_ribbon(
    data = intelligence.pdata, 
    aes(x = age, ymin = low_ci, ymax = upper_ci), 
    alpha = 0.1) +
  facet_grid(outcome~cohort_neat) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 4), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-60, 40), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(
    legend.position = "none", 
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 6), 
    axis.text.y = element_text(size = 6), 
    strip.text.x = element_text(size = 6), 
    strip.text.y = element_text(size = 8)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality") +
  scale_colour_manual(values = palette_ext)

## ---- Save -------------------------------------------------------------------
ggsave(
  filename = here("figures", "int_sii.jpg"), 
  plot = int.plot,
  h = 8, w = word_full, units = "cm", dpi = 300,
  device="png")

ggsave(
  filename = here("figures", "ext_sii.jpg"), 
  plot = ext.plot,
  h = 8, w = word_full, units = "cm", dpi = 300,
  device="png")

ggsave(
  filename = here("figures", "adhd_sii.jpg"), 
  plot = adhd.plot,
  h = 8, w = word_full, units = "cm", dpi = 300,
  device="png")

ggsave(
  filename = here("figures", "asd_sii.jpg"), 
  plot = asd.plot,
  h = 4, w = word_full, units = "cm", dpi = 300,
  device="png")

ggsave(
  filename = here("figures", "intelligence_sii.jpg"), 
  plot = intelligence.plot,
  h = 8, w = word_full, units = "cm", dpi = 300,
  device="png")


################################################################################
# Supplementary figures: Males & Females separately
################################################################################
################################################################################
# Prepare data  
################################################################################

## ---- Males ------------------------------------------------------------------
pred_males <- list(model = males.fit, newdata = pred.tab, coh = outcome_coh) %>%
  pmap(function(model, newdata, coh){
    
    dh.predictLmer(
      model = model,
      new_data = newdata, 
      coh_names = coh)
    
  }) %>%
  set_names(mod_names)

pred_males_trim <- list(predicted = pred_males, ages = ages) %>%
  pmap(function(predicted, coh, ages){
    
    dh.trimPredData(
      pred = predicted,
      coh_names = ages$cohort,
      min = ages$perc_5, 
      max = ages$perc_95) %>%
      dplyr::filter(cohort != "combined") %>%
      left_join(., names_neat) %>%
      mutate(sex = "male")
    
  })

## ---- Females ----------------------------------------------------------------
pred_fem <- list(model = females.fit, newdata = pred.tab, coh = outcome_coh) %>%
  pmap(function(model, newdata, coh){
    
    dh.predictLmer(
      model = model,
      new_data = newdata, 
      coh_names = coh)
    
  }) %>%
  set_names(mod_names)

pred_fem_trim <- list(predicted = pred_fem, ages = ages) %>%
  pmap(function(predicted, coh, ages){
    
    dh.trimPredData(
      pred = predicted,
      coh_names = ages$cohort,
      min = ages$perc_5, 
      max = ages$perc_95) %>%
      dplyr::filter(cohort != "combined") %>%
      left_join(., names_neat) %>%
      mutate(sex = "female")
    
  })

## ---- Combined ---------------------------------------------------------------
sex_pred <- list(males = pred_males_trim, females = pred_fem_trim) %>%
  pmap(function(males, females){
    
    bind_rows(males, females)
    
  })
  
################################################################################
# New function for these plots  
################################################################################
siiSexPlot <- function(predicted, y_low = -20, y_high = 40){
  
  ggplot() + 
    geom_line(
      data = predicted, 
      aes(x = age, y = predicted, colour = sex), 
      size = 0.4) +
    geom_ribbon(
      data = predicted, 
      aes(x = age, ymin = low_ci, ymax = upper_ci), 
      alpha = 0.1) +
    facet_wrap(~cohort_neat, ncol = 3) +
    scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 4), expand = c(0, 0)) + 
    scale_y_continuous(limit = c(y_low, y_high), breaks = seq(y_low, y_high, 20), expand = c(0, 0)) +
    theme_traj +
    theme(
      legend.position = "none", 
      axis.title.y = element_text(size = 8),
      axis.title.x = element_text(size = 8),
      axis.text.x = element_text(size = 6), 
      axis.text.y = element_text(size = 6), 
      strip.text.x = element_text(size = 6)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
    geom_hline(yintercept = 20, alpha = 0.05) +
    xlab("Child age") +
    ylab("Slope Index of Inequality") +
    scale_colour_manual(values = palette_ext)
  
}

################################################################################
# Make plots  
################################################################################
int_sex.plot <- siiSexPlot(sex_pred$int)
ext_sex.plot <- siiSexPlot(sex_pred$ext)
adhd_sex.plot <- siiSexPlot(sex_pred$adhd)
asd_sex.plot <- siiSexPlot(sex_pred$asd)

## Intelligence
intelligence_sex.pdata <- bind_rows(
  sex_pred$lan %>% mutate(outcome = "Verbal"),
  sex_pred$nvi %>% mutate(outcome = "Non-verbal"))

intelligence_sex.plot <- ggplot() + 
  geom_line(
    data = intelligence_sex.pdata, 
    aes(x = age, y = predicted, colour = sex), 
    size = 0.4) +
  geom_ribbon(
    data = intelligence_sex.pdata, 
    aes(x = age, ymin = low_ci, ymax = upper_ci), 
    alpha = 0.1) +
  facet_grid(outcome~cohort_neat) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 4), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-60, 40), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(
    legend.position = "none", 
    axis.title.y = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    axis.text.x = element_text(size = 6), 
    axis.text.y = element_text(size = 6), 
    strip.text.x = element_text(size = 6), 
    strip.text.y = element_text(size = 8)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality") +
  scale_colour_manual(values = palette_ext)

## ---- Save -------------------------------------------------------------------
ggsave(
  filename = here("figures", "int_sex.jpg"), 
  plot = int_sex.plot,
  h = 8, w = word_full, units = "cm", dpi = 300,
  device="png")

ggsave(
  filename = here("figures", "ext_sex.jpg"), 
  plot = ext_sex.plot,
  h = 8, w = word_full, units = "cm", dpi = 300,
  device="png")

ggsave(
  filename = here("figures", "adhd_sex.jpg"), 
  plot = adhd_sex.plot,
  h = 8, w = word_full, units = "cm", dpi = 300,
  device="png")

ggsave(
  filename = here("figures", "asd_sex.jpg"), 
  plot = asd_sex.plot,
  h = 4, w = word_full, units = "cm", dpi = 300,
  device="png")

ggsave(
  filename = here("figures", "intelligence_sex.jpg"), 
  plot = intelligence_sex.plot,
  h = 8, w = word_full, units = "cm", dpi = 300,
  device="png")




################################################################################
# MODEL FIT  
################################################################################
################################################################################
# Observed values by age  
################################################################################
observed_by_age <- dh.meanByAge(
  df = "analysis_df_l", 
  outcome = "int_", 
  age_var = "age", 
  intervals = c(0, 1, 1, 2, 3, 5, 6, 10, 11, 15, 16, 18)
)






 


plot_trim <- dh.trimPredData(
  pred = plotdata,
  coh_names = c("alspac", "bcg", "bib", "probit", "combined"),
  min = rep(0, 5), 
  max = c(20, 5, 5, 20, 20)
)



## ---- Internalising ----------------------------------------------------------
int.pred <- 

## ---- Externalising ----------------------------------------------------------


best.fit$nvi

  list(
  int = int.fit$model[int.fit$fit$model[1]][[1]]
)

int_best.fit <- 











test_1 <- dh.predictLmer(
  model = int_best.fit,
  new_data = int_pred.tab, 
  coh_names = ext_coh)


ages <- dh.getStats(
  df = "analysis_df_l", 
  vars = "int_age_")


plot_trim <- dh.trimPredData(
  pred = test_1,
  coh_names = c(ext_coh, "combined"),
  min = rep(4, 10), 
  max = rep(18, 10))


test_2 <- dh.predictLmer(
  model = int_best.fit,
  new_data = int_pred.tab, 
  coh_names = ext_coh)



%>%
  mutate(sii = edu)
  


  sii = edu_rank_num + age_m_1*ext_age_m_1_ed + age_m_0_5*ext_age_m_0_5_ed
)



ext_nl.pred <- ext_nl.tab %>%
  pmap(
    function(cohort, int_age__m2, int_age__m1, edu_rank_num, ...) {
      
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

ext_pred.tab <- tibble(
  age = seq(1, 18, by = 0.01),
  cohort = cohort,
  age_m_1 = age^-1,
  age_m_0_5 = age^-0.5,
  sii = edu_rank_num + age_m_1*ext_age_m_1_ed + age_m_0_5*ext_age_m_0_5_ed
)


<- dh.lmerTab(
  fit = ext_pc.fit, 
  coh = nl_coh,
  coh_format = "rows") %>%
  dplyr::rename(
    "ext_age_m_1_ed" = 'ext_age_m_1:edu_rank_num', 
    "ext_age_m_0_5_ed" = 'ext_age_m_0_5:edu_rank_num')




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


fit <- asd.fit
coh <- "moba"

str(best.model)

coh <- int_coh
fit <- int.fit

names(int.fit$model)

dh.lmTab(
  model = adhd.fit$model[[1]], 
  type = "lmer_slma", 
  direction = "wide", 
  coh_names = adhd_coh[adhd_coh != "ninfea"], 
  ci_format = "paste")$fixed 


%>%
  dplyr::select(cohort, variable, est) %>%
  pivot_wider(
    names_from = "cohort",
    values_from = "est")

, 
adhd.fit$model

str(int.fit$model[[1]])

int.fit$fit %>% print(n = Inf)


  , , , ),
  , , , nvi_coh)) %>%
  
dh.lmTab(
  model = int.fit$model[[1]], 
  type = "lmer_slma", 
  direction = "wide", 
  coh_names = int_coh, 
  ci_format = "paste")$fixed %>%
  dplyr::select(cohort, variable, est) %>%
  pivot_wider(
    names_from = "cohort",
    values_from = "est")

str(int.fit$model[[1]]$output.summary[[1]]$coefficients)

list(int.fit$model, ext.fit$model, adhd.fit$model, lan.fit$model, 
     nvi.fit$model) %>%
  map(~.x[[1]]) %>%
  map(
    ~dh.lmTab(
      model - .x, 
      type = "lmer_slma", 
      coh_names = ""
    )

int.fit$model[[1]]


nvi.fit$model %>% slice(1)

str(nvi.fit$model[1])

rm(fit.tab)


str(fit.tab)








%>%
  pivot_wider(
    names_from = "cohort", 
    values_from = "value") %>%
  dplyr::select(-elfe)




    )

pmap()

sample.stats <- dh.getStats(
  df = "analysis_df_l", 
  vars = c("edu_rank_num", "sex", "eusilc_income_quintiles", "ethn3_m", 
           "int_raw_", "ext_raw_", "adhd_raw_", "asd_raw_", "nvi_raw_", 
           "lan_raw_"))

excluded.stats <- dh.getStats(
  df = "analysis_df_l", 
  vars = c("edu_rank_num", "sex", "eusilc_income_quintiles", "ethn3_m", 
           "int_raw_", "ext_raw_", "adhd_raw_", "asd_raw_", "nvi_raw_", 
           "lan_raw_"))


miss_attr.ref <- tibble(
  var = c(
    "int_age_", "ext_age_", "adhd_age_", "asd_age_", "nvi_age_", "lan_age_"),
  outcome = c(
    "int_raw_", "ext_raw_", "adhd_raw_", "asd_raw_", "nvi_raw_", "lan_raw_"))

miss_attr.stats_1 <- miss_attr.ref[1:2, ] %>%
  pmap(function(var, outcome){
    
    dh.meanByGroup(
      df = "analysis_df_l", 
      outcome = "int_raw_", 
      group_var = "int_age_", 
      intervals = c(0, 3, 4, 7, 8, 13, 14, 17))
    
  })

save.image()

miss_attr.stats_2 <- miss_attr.ref[3:4, ] %>%
  pmap(function(var, outcome){
    
    dh.meanByGroup(
      df = "analysis_df_l", 
      outcome = "int_raw_", 
      group_var = "int_age_", 
      intervals = c(0, 3, 4, 7, 8, 13, 14, 17))
    
  })

save.image()

miss_attr.stats_3 <- miss_attr.ref[5:6, ] %>%
  pmap(function(var, outcome){
    
    dh.meanByGroup(
      df = "analysis_df_l", 
      outcome = "int_raw_", 
      group_var = "int_age_", 
      intervals = c(0, 3, 4, 7, 8, 13, 14, 17))
    
  })

save.image()

miss_attr.stats <- c(
  miss_attr.stats_1 %>% set_names(c("int", "ext")), 
  miss_attr.stats_2 %>% set_names(c("adhd", "asd")),
  miss_attr.stats_3 %>% set_names(c("nvi", "lan"))) %>%
  bind_rows(.id = "outcome")

miss_attr.stats %>% print(n = Inf)

max_cohort_n <- original_l_n %>%
  map(~.x[1] %>% as_tibble) %>%
  keep(!names(.) %in% c("dimensions of mh_df in combined studies")) %>%
  set_names(names(conns)) %>%
  bind_rows(.id = "cohort") %>%
  dplyr::rename(original_n = value)

miss_attr.stats %>%
  left_join(., max_cohort_n, by = "cohort") %>%
  group_by(outcome, group) %>%
  group_split %>%
  map(~mutate(., total_valid = sum(nvalid))) %>%
  map(~mutate(., total_cohort = sum(original_n))) %>%
  map(~dplyr::select(., outcome, group, total_valid, total_cohort)) %>%
  map(~slice(., 1)) %>%
  bind_rows %>%
  mutate(available_paste = round(((total_valid / total_cohort)*100), 2)) %>%
  mutate(available = paste0(total_valid, " (", available_paste, ")"))

## denominator is going to wrong. Need denominator to be those for which there
## is a chance of there being some data. I think without knowing which 
## measurement occassion there were from it's going to be tricky. Maybe just
## follow robyn's approach and ditch this table.


ds.colnames("analysis_df_l")



#missing_ext <- dh.getStats(
#  df = "ext_miss",
#  vars = c("ext_pc_.4", "ext_pc_.8", "ext_pc_.12", "ext_pc_.17", "ext_age_.4",
"ext_age_.8", "ext_age_.12", "ext_age_.17"))

#missing_ext$continuous %>%
#  mutate(missing_pc = paste0(missing_n, " (", missing_perc, ")")) %>%
#  dplyr::select(variable, cohort, missing_pc) %>%
#  separate(col = variable, sep = "\\.", into = c("type", "age")) %>%
#  dplyr::filter(type != "ext_pc_") %>%
#  mutate(age = as.numeric(age)) %>%
#  dplyr::select(-type) %>%
#  mutate(missing_pc = ifelse(missing_pc == "NA (NA)", "0 (100)", missing_pc)) %>%
#  pivot_wider(
#    names_from = "cohort", 
#    values_from = "missing_pc") %>%
#  arrange(age) %>%
#  mutate(age = case_when(
#    age == 4 ~ "0 - 3",
#    age == 8 ~ "4 - 7",
#    age == 12 ~ "8 - 11", 
#    age == 17 ~ "12 - 17")) %>%
#  dplyr::select(age, combined, everything())

## Actually I think this can be more efficiently done with 6.2



################################################################################
# 3. Recruitment flow chart
################################################################################

## ---- Get dimensions ---------------------------------------------------------
original_w_n <- ds.dim("mh_df_w")
original_l_n <- ds.dim("mh_df")

exp_out_w_n <- ds.dim("exp_out_df_w")
exp_out_l_n <- ds.dim("exp_out_df_l")

analysis_w_n <- ds.dim("analysis_df_w")
analysis_l_n <- ds.dim("analysis_df_l")

## ---- Left column ------------------------------------------------------------

# Original
o_w_n <- original_w_n$`dimensions of mh_df_w in combined studies`[1]
o_l_n <- original_l_n$`dimensions of mh_df in combined studies`[1]

# Some exposures and outcomes
e_o_w_n <- exp_out_w_n$`dimensions of exp_out_df_w in combined studies`[1]
e_o_l_n <- exp_out_l_n$`dimensions of exp_out_df_l in combined studies`[1]

# Final dataset
a_w_n <- analysis_w_n$`dimensions of analysis_df_w in combined studies`[1]
a_l_n <- analysis_l_n$`dimensions of analysis_df_l in combined studies`[1]


## ---- Right column -----------------------------------------------------------
o_w_n - e_o_w_n
o_l_n - e_o_l_n

e_o_w_n - a_w_n
e_o_l_n - a_l_n

################################################################################
# 4. Table S1: Available outcomes n cohort  
################################################################################

## ---- Main table -------------------------------------------------------------
instr.vars <- c("ext_instr_", "int_instr_", "adhd_instr_", "asd_instr_", 
                "nvi_instr_", "lan_instr_")

avail_exp <- dh.getStats(
  df = "analysis_df_l", 
  vars = instr.vars)

instr_neat <- tibble(
  variable  = instr.vars, 
  neat_name = c("Externalising", "Internalising", "ADHD", "ASD", 
                "Non-verbal intelligence", "language"))

instruments.tab <- avail_exp$categorical %>%
  dplyr::filter(cohort != "combined" & value != 0 & !is.na(category)) %>%
  arrange(cohort, variable, category) %>%
  left_join(., instr.vals) %>%
  left_join(., instr_neat) %>%
  dplyr::select(cohort, neat_name, instrument) %>%
  group_by(cohort, neat_name) %>%
  group_split %>%
  map(~mutate(., instr_neat = paste0(unlist(instrument), collapse = ","))) %>%
  map(~slice(., 1)) %>%
  bind_rows %>%
  dplyr::select(cohort, neat_name, instrument = instr_neat) %>%
  pivot_wider(
    names_from = "neat_name",
    values_from = "instrument")

write_csv(
  instruments.tab, 
  file = here("code/datashield/bmj/tables", "instruments.csv"))

## ---- Legend -----------------------------------------------------------------
included_instr <- avail_exp$categorical %>%
  dplyr::filter(value != 0 & !is.na(category)) %>%
  pull(category) %>%
  unique

instr_legend.tab <- instr.vals %>%
  dplyr::filter(category %in% included_instr) %>%
  arrange(instrument) %>%
  dplyr::select(instrument, full_name) %>%
  mutate(combined = paste0(instrument, " = ", full_name))

paste0(instr_legend.tab$combined, collapse = ", ")

################################################################################
# 5. Available ethnicity data  
################################################################################
ethnicity <- dh.getStats(
  df = "analysis_df_w", 
  vars = "ethn3_m")

## ---- Cohorts with data ------------------------------------------------------
ethnicity$categorical %>%
  dplyr::filter(cohort != "combined" & perc_missing != 100) %>%
  pull(cohort) %>%
  unique

## ---- Maximum N & % ----------------------------------------------------------
ethnicity$categorical %>%
  dplyr::filter(cohort == "combined" & category == 1) %>%
  dplyr::select(valid_n, perc_total)



################################################################################
# Table comparing included and excluded participants  
################################################################################
descriptives_w <- dh.getStats(
  df = "analysis_df_w", 
  vars = wide.vars)

descriptives_w_exc <- dh.getStats(
  df = "excluded_w",
  vars = wide.vars)

save.image()


dh.createTableOne(
  stats = descriptives_w, 
  vars = wide.vars,
  type = "both", 
  coh_direction = "rows", 
  cont_stats = "med_iqr", 
  inc_missing = TRUE)

%>%
  print(n = Inf)

# Cohort labels
# sorting by cohort



wide_ref <- c()


stats <- descriptives_l
vars <- all.vars
var_labs <- all.ref
cat_labs <- cat.ref

save.image()

all.ref

dh.createTableOne(
  stats = descriptives_l, 
  vars = all.vars,
  var_labs = all.ref,
  cat_labs = cat.ref,
  type = "both", 
  value_format = "separate") 



