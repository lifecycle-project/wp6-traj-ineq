################################################################################
## Project: Urban environment and postnatal depression
## Script purpose: Reference tables for variables
## Date: 3rd August 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/lc-names-neat.R")

names_neat <- names_neat %>%
  dplyr::rename(coh_label = cohort_neat)

################################################################################
# 1. Create vectors of variables
################################################################################

## ---- Exposures --------------------------------------------------------------
exp.vars <- c("edu_rank_num", "eusilc_income_quintiles")
exp.names <- c("Maternal Education", "Disposable Income")

## ---- Outcomes ---------------------------------------------------------------
out.vars <- c("int_pc_", "ext_pc_", "adhd_pc_", "asd_pc_", "lan_pc_", "nvi_pc_")
out.names <- c("Internalising (percentiles)", "Externalising (percentiles)", 
               "ADHD (percentiles)", "ASD (percentiles)", 
               "Language (percentiles)", 
               "Non-verbal Intelligence (percentiles)")


## ---- Instruments ------------------------------------------------------------
inst.vars <- c("int_instr_", "ext_instr_", "adhd_instr_", "asd_instr_", 
               "nvi_instr_", "lan_instr_")
inst.names <- c("Internalising instrument", "Externalising instrument", 
                "ADHD instrument", "ASD instrument", 
                "Non-Verbal Intelligence instrument", "Language instrument")

instr.vals <- tribble(
  ~category, ~instrument, ~full_name,
  "47", "SDQ", "Strengths and Difficulties Questionnaire",
  "20", "CPRS-R", "Conners' Parenting Rating Scale - Revised",
  "25", "DISC-IV/DSM", "Diagnostic Interview Schedule for Children",
  "13", "CBCL", "Child Behaviour Checklist",
  "1", "ADBB", "Alarm Distress Baby Scale",
  "37", "M-CHAT", "Modified Checklist for Autism in Toddlers - Revised",
  "50", "SRS", "Social Responsiveness Scale",
  "12", "CAST", "Childhood Autism Spectrum Test",
  "46", "SCQ", "Social Communication Questionnaire",
  "61", "YSR", "Youth Self-Report",
  "24", "DDST", "Denver Developmental Screening Test",
  "9", "BVPS", "British Picture Vocabulary Scale",
  "41", "NEPSY-II", "A Developmental NEuroPSYchological Assessment - II",
  "36", "MCDI", "MacArthur-Bates Communicative Development Inventories",
  "39", "MSCA", "McCarthy Scales of Children's Abilities",
  "52", "SVF", "Stress Coping Style Questionnaire",
  "10", "BSID", "Bayley Scales Of Infant and Toddler Development",
  "59", "WISC", "Wechsler Intelligence Scale for Children",
  "60", "WPSSI", "Wechsler Preschool & Primary Scale of Intelligence",
  "17", "CFIT", "Cattell Culture Fair Intelligence Test",
  "49", "SON-R", "Snijders-Oomen Nonverbal Intelligence Tests",
  "45", "RPM", "Raven's Progressive Matrices",
  "4", "ASQ", "Ages and Stages Questionnaire")

## ---- Ages -------------------------------------------------------------------
age.vars <- c("int_age_", "ext_age_", "adhd_age_", "asd_age_", 
               "nvi_age_", "lan_age_")
age.names <- c("Internalising age at measurement", 
                "Externalising age at measurement", 
                "ADHD age at measurement", "ASD age at measurement", 
                "Non-Verbal Intelligence age at measurement", 
                "Language age at measurement")


## ---- Covariates -------------------------------------------------------------
cov.vars <- c("ethn3_m", "agebirth_m_y", "sex")
cov.names <- c("Maternal ethnicity", "Maternal age at birth", "Child sex")

wide.vars <- c(exp.vars, cov.vars)

wide.ref <- tibble(
  variable = wide.vars, 
  var_label = c(exp.names, cov.names))

long.vars <- c(out.vars, inst.vars, age.vars)

long.ref <- tibble(
  variable = long.vars,
  var_label = c(out.names, inst.names, age.names))

all.vars <- c(wide.vars, long.vars)
all.ref <- bind_rows(wide.ref, long.ref)

cat.ref <- tibble(
  variable = c(
    rep("edu_rank_num", 3), 
    rep("eusilc_income_quintiles", 5),
    rep(inst.vars, each = 23), 
    rep("ethn3_m", 3), 
    rep("sex", 2)), 
  category = c(
    seq(1, 3, 1),
    seq(1, 5, 1),
    rep(instr.vals$category, 6),
    seq(1, 3, 1),
    c(1, 2)),
  cat_label = c(
    c("Low", "Medium", "High"), 
    c("1st quintile", "2nd quintile", "3rd quintile", "4th quintile", 
      "5th quintile"), 
    rep(instr.vals$full_name, 6),
    c("Western", "Non-western", "Mixed"),
    c("Male", "Female"))
  )
    
    c(exp.vars)
  
  
)

nrow(instr.vals)

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


