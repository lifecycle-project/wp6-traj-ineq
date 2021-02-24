################################################################################
## Project: wp6-traj-ineq
## Script purpose: Describe available data
## Date: 30th September 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(magrittr)
library(tidyr)
library(dsHelper)
library(dplyr)

################################################################################
# 1. Load workspace  
################################################################################
conns <- datashield.login(logindata, restore = "mhtraj_10")


tmp_dnbc <- dh.meanByOccasion(
  df = "analysis_df", 
  agevar = "ext_age_",
  outcome = "ext_pc_",
  conns = conns["dnbc"],
  bands = list(
    dnbc = c(5, 8, 8, 14)),
  grouping = "edu_m"
)

tmp_dnbc %>% filter(cohort != "combined")

tmp_dnbc_raw <- dh.meanByOccasion(
  df = "analysis_df", 
  agevar = "ext_age_",
  outcome = "ext_raw_",
  conns = conns["dnbc"],
  bands = list(
    dnbc = c(5, 8, 8, 14)),
  grouping = "edu_m"
)

tmp_dnbc_raw %>% filter(cohort != "combined")

################################################################################
# 2. QC  
################################################################################

# I think this will have to wait until we have better functionality in DS


################################################################################
# 3. Describe instruments used  
################################################################################

## To begin with I want to restrict to one instrument per construct. I'm a bit
## worried about what artefacts could be brought in by combing instruments.

## ---- Overall summary --------------------------------------------------------
instruments <- dh.getStats(
  df = "mhrep", 
  vars = c("ext_instr_", "int_instr_"),
  conns = conns)


## ---- Summarise this ---------------------------------------------------------
mh_available <- instruments[[1]] %>%  mutate(
  instrument = case_when(
    category == 13 ~ "CBCL",
    category == 47 ~ "SDQ")

ext_cbcl_avail <- mh_available %>% 
  filter(variable == "ext_instr_" & category == 13 & value >0 & cohort != "combined")

ext_sdq_avail <- mh_available %>% 
  filter(variable == "ext_instr_" & category == 47 & value >0 & cohort != "combined")

int_available <- mh_available %>% 
  filter(variable == "int_instr_" & category == 47 & value >0 & cohort != "combined")

ext_cbcl_coh <- ext_cbcl_avail %>% pull(cohort) %>% as.character %>% unique
ext_sdq_coh <- ext_sdq_avail %>% pull(cohort) %>% as.character %>% unique

int_coh <- int_available %>% pull(cohort) %>% as.character %>% unique

int_coh <- int_coh[which(int_coh != "dnbc")]

################################################################################
# 4. Scatterplots  
################################################################################

## ---- Externalising ----------------------------------------------------------
ds.scatterPlot(
  x = "analysis_df$ext_age_", 
  y = "analysis_df$ext_pc_", 
  datasources = conns[ext_coh])

## ---- Internalising ----------------------------------------------------------
ds.scatterPlot(
  x = "analysis_df$int_age_", 
  y = "analysis_df$int_pc_", 
  datasources = conns[int_coh])
  
## ---- Vector of cohorts with >=2 observations --------------------------------
ext_coh_nl <- ext_coh[which(ext_coh != "elfe")]
int_coh_nl <- int_coh[which(int_coh != "elfe")]

################################################################################
# 5. Occasion averages
################################################################################

## ---- Scatter plots ----------------------------------------------------------
ds.scatterPlot(x = "analysis_df$ext_age_", y = "analysis_df$ext_pc_", 
               datasources = conns["dnbc"])


## ---- Externalising education factor -----------------------------------------
tmp_chop <- dh.meanByOccasion(
  df = "analysis_df", 
  agevar = "ext_age",
  outcome = "ext_pc_",
  conns = conns["chop"],
  bands = list(
    chop = c(4, 6, 7, 9, 10, 12)),
  grouping = "edu_m"
)

tmp_dnbc <- dh.meanByOccasion(
  df = "analysis_df", 
  agevar = "ext_age",
  outcome = "ext_pc_",
  conns = conns["dnbc"],
  bands = list(
    dnbc = c(6, 8, 8, 10, 10, 15)),
  grouping = "edu_m"
)

tmp_moba <- dh.meanByOccasion(
  df = "analysis_df", 
  agevar = "ext_age",
  outcome = "ext_pc_",
  conns = conns["moba"],
  bands = list(
    moba = c(0, 2, 2, 4, 4, 7)),
  grouping = "edu_m"
)

tmp_inma <- dh.meanByOccasion(
  df = "analysis_df", 
  agevar = "ext_age",
  outcome = "ext_pc_",
  conns = conns["inma"],
  bands = list(
    inma = c(4, 6, 7, 9, 10, 12)),
  grouping = "edu_m"
)


## ---- Externalising rank score -----------------------------------------------
tmp_chop_r <- dh.meanByOccasion(
  df = "analysis_df", 
  agevar = "ext_age",
  outcome = "ext_pc_",
  conns = conns["chop"],
  bands = list(
    chop = c(4, 6, 7, 9, 10, 12)),
  grouping = "edu_rank_num"
)

tmp_dnbc_r <- dh.meanByOccasion(
  df = "analysis_df", 
  agevar = "e_age",
  outcome = "e_pc",
  conns = conns["dnbc"],
  bands = list(
    dnbc = c(6, 7, 7, 10, 10, 12)),
  grouping = "edu_rank_num"
)

tmp_moba_r <- dh.meanByOccasion(
  df = "analysis_df", 
  agevar = "ext_age",
  outcome = "ext_pc_",
  conns = conns["moba"],
  bands = list(
    moba = c(0, 2, 2, 4, 4, 7)),
  grouping = "edu_rank_num"
)

tmp_inma_r <- dh.meanByOccasion(
  df = "analysis_df", 
  agevar = "ext_age",
  outcome = "ext_pc_",
  conns = conns["inma"],
  bands = list(
    inma = c(4, 6, 7, 9, 10, 12)),
  grouping = "edu_rank_num"
)


rank_avs <- bind_rows(tmp_chop_r, tmp_dnbc_r, tmp_moba_r, tmp_inma_r)

ggplot(data = rank_avs, aes(x = age, y = ext_pc_, colour = factorlevel)) + 
  geom_point() +
  facet_wrap(~cohort)


################################################################################
# 6. Minimum and maximum(ish) ages  
################################################################################
ages_min_max <- dh.getStats(
  df = "analysis_df",
  vars = c("int_age_", "ext_age_"), 
  conns = conns
)

