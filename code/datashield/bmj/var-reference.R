################################################################################
## Project: Urban environment and postnatal depression
## Script purpose: Reference tables for variables
## Date: 3rd August 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/lc-names-neat.R")
################################################################################
# 1. Create vectors of variables
################################################################################

## ---- Exposures --------------------------------------------------------------
exp.vars <- c("edu_rank_num")

exp.names <- c()

## ---- Outcomes ---------------------------------------------------------------
out.vars <- c("int_pc_", "ext_pc_", "adhd_pc_", "asd_pc_", "lan_pc_", "nvi_pc_")

## ---- Instruments ------------------------------------------------------------
inst.vars <- c("int_instr_", "ext_instr_", "adhd_instr_", "asd_instr_", 
               "nvi_instr_", "lan_instr_")

instr.vals <- tribble(
  ~category, ~instrument, 
  "47", "SDQ",
  "20", "CPRS-R",
  "25", "DISC-IV/DSM",
  "13", "CBCL",
  "1", "ADBB",
  "37", "M-CHAT",
  "50", "SRS",
  "12", "CAST",
  "46", "SCQ",
  "61", "YSR",
  "24", "DDST",
  "9", "BVPS",
  "41", "NEPSY-II",
  "36", "MCDI",
  "39", "MSCA",
  "52", "SVF",
  "10", "BSID",
  "59", "WISC",
  "60", "WPSSI",
  "17", "CFIT",
  "49", "SON-R",
  "45", "RPM",
  "4", "ASQ")


## ---- Covariates -------------------------------------------------------------
cov.vars <- c(
  "edu_m_0", "eusilc_income_quintiles", "areases_tert", "ethn3_m", 
  "parity_bin", "sex", "prepreg_psych", "preg_dia", "preg_ht", 
  "birth_month_f", "birth_year_f", "mat_age_f", "preterm", "cohab")

cov.names <- c(
  "Maternal education", "Income", "Area deprivation", "Maternal ethnicity",
  "Parity", "Child sex", "Maternal pre-pregnancy psychiatric conditions",
  "Gestational diabetes", "Pregnancy hypertension", "Birth season", 
  "Birth year", "Maternal age at birth", "Pre-term birth", 
  "Maternal cohabitation at birth")

cov_analysis.vars <- c("birth_month", "birth_year")


## ---- Outcome ----------------------------------------------------------------
out.vars <- "ppd"

out.names <- "Postpartum depression"


## ---- Meta variables ---------------------------------------------------------
meta.vars <- c(ref_codes$dummy, "child_no", "outcome", "child_id")

meta.names <- c(
  "alspac_dummy", "bib_dummy", "dnbc_dummy", "genr_dummy", "moba_dummy", 
  "ninfea_dummy", "rhea_dummy", "eden_nancy_dummy", "eden_poitiers dummy", 
  "inma_gip_dummy", "inma_sabadell dummy", "Child number", "Birth outcome", 
  "Child identifier")

################################################################################
# 2. Create reference tables
################################################################################
bin_lab <- c("No", "Yes")

## ---- Exposures --------------------------------------------------------------
exp_preg.ref <- tibble(
  variable = exp_preg.vars,
  full_name = exp.names, 
  type = c(rep("cont", 3), "cat", "cont", rep("cat", 2), rep("cont", 3)), 
  var_type = "exposure"
)

exp_year_1.ref <- tibble(
  variable = exp_year_1.vars,
  full_name = exp.names, 
  type = c(rep("cont", 3), "cat", "cont", rep("cat", 2), rep("cont", 3)), 
  var_type = "exposure"
)

exp_preg_analysis.ref <- tibble(
  variable = exp_preg_analysis.vars, 
  full_name = exp.names, 
  type = c(rep("cont", 3), "cat", "cont", rep("cat", 2), rep("cont", 3)), 
  var_type = "exposure_analysis"
)

exp_preg_coef.ref <- tibble(
  variable = exp_preg_coef.vars, 
  full_name = exp_preg_coef.names, 
  type = c(rep("cont", 3), rep("cat", 4), "cont", rep("cat", 2), rep("cont", 3)), 
  var_type = "coef_names"
)

exp_preg_coef_sep.ref <- tibble(
  variable = exp_preg_coef_sep.vars, 
  full_name = exp_preg_coef.names, 
  type = c(rep("cont", 3), rep("cat", 4), "cont", rep("cat", 2), rep("cont", 3)), 
  var_type = "coef_names"
)

exp_quart.ref <- tibble(
  variable = quart.vars, 
  full_name = rep(quart.names, each = 4),
  type = "cont", 
  var_type = "quantiles", 
  quartile = rep(seq(1, 4, 1), 7)
)

exp_values.ref <- tibble(
  variable = c(
    rep("lden_preg_f", 5),
    rep("greenyn300_preg", 2),
    rep("blueyn300_preg", 2)),
  category = c(
    seq(1, 5, 1), 
    c(0, 1), 
    c(0, 1)), 
  cat_label = c(
    c("<55 dB", "55-55.9 dB", "60-64.9 dB", "65-69.9 dB", ">70 dB"),
    bin_lab,
    bin_lab)) %>%
  mutate(category = as.character(category))

## ---- Covariates -------------------------------------------------------------
cov.ref <- tibble(
  variable = c(cov.vars, cov_analysis.vars),
  full_name = c(cov.names, "Birth Month (cont)", "Birth Year (cont)"),
  type = c(rep("cat", 14), rep("cont", 2)),
  var_type = "covariate") 

cov_values.ref <- tibble(
  variable = c(
    rep("edu_m_0", 3), 
    rep("eusilc_income_quintiles", 5), 
    rep("areases_tert", 3), 
    rep("ethn3_m", 3), 
    rep("parity_bin", 2), 
    rep("sex", 2), 
    rep("prepreg_psych", 2), 
    rep("preg_dia", 2),
    rep("preg_ht", 2), 
    rep("birth_month_f", 4), 
    rep("birth_year_f", 4), 
    rep("mat_age_f", 6), 
    rep("preterm", 2), 
    rep("cohab", 2)),
  category = c(
    c(3, 2, 1), 
    seq(1, 5, 1), 
    c(1, 2, 3), 
    c(1, 2, 3), 
    c(0, 1), 
    c(1, 2), 
    c(0, 1), 
    c(0, 1), 
    c(0, 1),
    c("spring", "summer", "autumn", "winter"),
    c("90_95", "96_00", "01_05", "05_11"),
    c("15_20", "21_25", "26_30", "31_35", "36_40", "41_50"),
    c(0, 1), 
    c(1, 2)), 
  cat_label = c(
    c("Low", "Medium", "High"), 
    c("First quintile", "Second quintile", "Third quintile", "Fourth quintile", 
      "Fifth quintile"),
    c("Low deprivation", "Medium deprivation", "High deprivation"),
    c("Western European", "Non-western European", "Mixed"),
    c("Nulliparous", "Not nulliparous"),
    c("Male", "Female"),
    bin_lab,
    bin_lab,
    bin_lab,
    c("Spring", "Summer", "Autumn", "Winter"),
    c("1990 - 1995", "1996 - 2000", "2001 - 2005", "2006 - 2011"), 
    c("15 - 20", "21 - 25", "26 - 30", "31 - 35", "36 - 40", "41 - 45"),
    bin_lab,
    bin_lab)
)

## ---- Outcome ----------------------------------------------------------------
out.ref <- tibble(
  variable = out.vars,
  full_name = out.names, 
  type = "cat",
  var_type = "outcome"
)

out_values.ref <- tibble(
  variable = rep("ppd", 2),
  category = c("0", "1"),
  cat_label = bin_lab
)

## ---- Meta variables ---------------------------------------------------------
meta.ref <- tibble(
  variable = meta.vars, 
  full_name = meta.names,
  type = c(rep("cat", 13), "cont"), 
  var_type = "meta"
)

## ---- Combined reference table -----------------------------------------------
full.ref <- bind_rows(
  exp_preg.ref, exp_year_1.ref, exp_preg_analysis.ref, cov.ref, out.ref, 
  meta.ref, exp_preg_coef.ref) 

full_values.ref <- bind_rows(
  out_values.ref, exp_values.ref, cov_values.ref)


