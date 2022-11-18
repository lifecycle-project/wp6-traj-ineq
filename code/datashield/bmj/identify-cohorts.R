################################################################################
## Project: wp6-traj-ineq
## Script purpose: Define available cohorts
## Date: 15th March 2022
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

conns <- datashield.login(logindata, restore = "mhtraj_15")

################################################################################
# 1. Fix columns  
################################################################################
dh.columnCast(
  df = "analysis_df_l", 
  target_vars =  instr.vars, 
  target_class = "factor")

################################################################################
# 2. Helper function
################################################################################
cohAvail <- function(stats, var){
  stats %>%
    dplyr::filter(variable == var & !is.na(mean) & cohort != "combined") %>%
    pull(cohort) %>%
    as.character()
}

################################################################################
# Identify available cohorts
################################################################################
################################################################################
# 3. Internalising
################################################################################
avail_int <- dh.getStats(
  df = "analysis_df_l",
  vars = c("int_pc_", "int_raw_", "int_instr_"))

int_coh_any <- avail_int$continuous %>% cohAvail("int_pc_")

int_scatter_1 <- ds.scatterPlot(
  x = "analysis_df_l$int_age_", 
  y = "analysis_df_l$int_pc_", 
  datasources = conns[int_coh_any[1:(length(int_coh_any)/2)]])

int_scatter_2 <- ds.scatterPlot(
  x = "analysis_df_l$int_age_", 
  y = "analysis_df_l$int_pc_", 
  datasources = conns[int_coh_any[((length(int_coh_any)/2)+1):length(int_coh_any)]])

int_coh <- int_coh_any[!int_coh_any %in% c("ninfea")]

occ_int <- tibble(
  cohort = c("alspac", "chop", "dnbc", "eden", "genr", "inma", "moba", 
             "raine", "rhea"), 
  outcome = "int",
  occasions = c(7, 3, 2, 3, 4, 2, 3, 6, 3))

################################################################################
# 4. Externalising  
################################################################################
avail_ext <- dh.getStats(
  df = "analysis_df_l",
  vars = c("ext_pc_", "ext_raw_", "ext_instr_"))

ext_coh_any <- avail_ext$continuous %>% cohAvail("ext_pc_")

ext_scatter_1 <- ds.scatterPlot(
  x = "analysis_df_l$ext_age_", 
  y = "analysis_df_l$ext_pc_", 
  datasources = conns[ext_coh_any[1:(length(ext_coh_any)/2)]])

ext_scatter_2 <- ds.scatterPlot(
  x = "analysis_df_l$ext_age_", 
  y = "analysis_df_l$ext_pc_", 
  datasources = conns[ext_coh_any[((length(ext_coh_any)/2)+1):length(ext_coh_any)]])

ext_coh <- ext_coh_any[!ext_coh_any %in% c("ninfea", "sws", "elfe")]

occ_ext <- tibble(
  cohort = c("alspac", "chop", "dnbc", "eden", "genr", "inma", "moba", 
             "raine", "rhea"),
  outcome = "ext",
  occasions = c(7, 3, 2, 3, 4, 2, 3, 6, 3))

################################################################################
# 5. ADHD  
################################################################################
avail_adhd <- dh.getStats(
  df = "analysis_df_l",
  vars = c("adhd_pc_", "adhd_raw_", "adhd_instr_"))

adhd_coh_any <- avail_adhd$continuous %>% cohAvail("adhd_pc_")

adhd_scatter_1 <- ds.scatterPlot(
  x = "analysis_df_l$adhd_age_", 
  y = "analysis_df_l$adhd_pc_", 
  datasources = conns[adhd_coh_any[1:5]])

adhd_scatter_2 <- ds.scatterPlot(
  x = "analysis_df_l$adhd_age_", 
  y = "analysis_df_l$adhd_pc_", 
  datasources = conns[adhd_coh_any[6:length(adhd_coh_any)]])

adhd_coh <- adhd_coh_any[!adhd_coh_any %in% c("genr")]

occ_adhd <- tibble(
  cohort = c("alspac", "chop", "dnbc", "eden", "inma", "moba", "ninfea", 
             "raine", "rhea"),
  outcome = "adhd",
  occasions = c(7, 2, 2, 3, 2, 4, 2, 5, 3))

################################################################################
# 6. ASC  
################################################################################
avail_asd <- dh.getStats(
  df = "analysis_df_l",
  vars = c("asd_pc_", "asd_raw_"))

asd_coh_any <- avail_asd$continuous %>% cohAvail("asd_pc_")

asd_scatter <- ds.scatterPlot(
  x = "analysis_df_l$asd_age_", 
  y = "analysis_df_l$asd_pc_", 
  datasources = conns[asd_coh_any])

asd_coh <- c("inma", "moba")

occ_asd <- tibble(
  cohort = c("inma", "moba"), 
  outcome = "asd",
  occasions = c(2, 3))

#"elfe"
################################################################################
# 7. Language  
################################################################################
avail_lan <- dh.getStats(
  df = "analysis_df_l",
  vars = c("lan_pc_", "lan_raw_", "lan_instr_"))

lan_coh_any <- avail_lan$continuous %>% cohAvail("lan_pc_")

lan_scatter <- ds.scatterPlot(
  x = "analysis_df_l$lan_age_", 
  y = "analysis_df_l$lan_pc_", 
  datasources = conns[lan_coh_any])

lan_coh <- c("alspac", "inma", "rhea")

occ_lan <- tibble(
  cohort = c("alspac", "inma", "rhea"), 
  outcome = "lan",
  occasions = c(4, 3, 3))

################################################################################
# 8. NVI  
################################################################################
avail_nvi <- dh.getStats(
  df = "analysis_df_l",
  vars = c("nvi_pc_", "nvi_raw_", "nvi_instr_"))

nvi_coh_any <- avail_nvi$continuous %>% cohAvail("nvi_pc_")

nvi_scatter <- ds.scatterPlot(
  x = "analysis_df_l$nvi_age_", 
  y = "analysis_df_l$nvi_pc_", 
  datasources = conns[nvi_coh_any])

nvi_coh <- c("alspac", "inma", "rhea")

occ_nvi <- tibble(
  cohort = c("alspac", "inma", "rhea"), 
  outcome = "nvi",
  occasions = c(2, 4, 4))

save.image()
