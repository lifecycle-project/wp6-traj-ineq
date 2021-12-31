################################################################################
## Project: wp6-traj-ineq
## Script purpose: Visualise available data   
## Date: 19th June 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################
library(remotes)
install_github("lifecycle-project/dsHelper")
library(dsHelper)

remove.packages("dsHelper")

conns <- datashield.login(logindata, restore = "mhtraj_9")



## Main analysis: keep everyone with >= 2 measurements
## Sensitivity: compare (i) restricting to >= 3, (ii) restrict to same measure
## at all time points

cohAvail <- function(stats, var){
  stats %>%
    dplyr::filter(variable == var & !is.na(mean) & cohort != "combined") %>%
    pull(cohort) %>%
    as.character()
}
################################################################################
# 1. Internalising  
################################################################################
avail_int <- dh.getStats(
  df = "analysis_df",
  vars = c("int_pc_", "int_raw_", "int_instr_")
)

int_coh_any <- avail_int$continuous %>% cohAvail("int_pc_")
  
int_scatter <- ds.scatterPlot(
  x = "analysis_df$int_age_", 
  y = "analysis_df$int_pc_", 
  datasources = conns[int_coh_any])

int_coh <- int_coh_any[!int_coh_any %in% c("ninfea", "sws")]

################################################################################
# 2. Externalising  
################################################################################
avail_ext <- dh.getStats(
  df = "analysis_df",
  vars = c("ext_pc_", "ext_raw_", "ext_instr_")
)

ext_coh_any <- avail_ext$continuous %>% cohAvail("ext_pc_")

ext_scatter <- ds.scatterPlot(
  x = "analysis_df$ext_age_", 
  y = "analysis_df$ext_pc_", 
  datasources = conns[ext_coh_any])

ext_coh <- ext_coh_any[!ext_coh_any %in% c("ninfea", "sws")]
################################################################################
# 3. ADHD  
################################################################################
avail_adhd <- dh.getStats(
  df = "analysis_df",
  vars = c("adhd_pc_", "adhd_raw_", "adhd_instr_")
)

adhd_coh_any <- avail_adhd$continuous %>% cohAvail("adhd_pc_")

adhd_scatter <- ds.scatterPlot(
  x = "analysis_df$adhd_age_", 
  y = "analysis_df$adhd_pc_", 
  datasources = conns[adhd_coh_any])

adhd_scatter <- ds.scatterPlot(
  x = "analysis_df$adhd_age_", 
  y = "analysis_df$adhd_pc_", 
  datasources = conns[adhd_coh_any])


adhd_coh <- adhd_coh_any[!adhd_coh_any %in% c("genr")]

################################################################################
# 4. ASC  
################################################################################
avail_asd <- dh.getStats(
  df = "analysis_df",
  vars = c("asd_pc_", "asd_raw_", "asd_instr_"),
  conns = conns[names(conns) != "dnbc"]
)

asd_coh_any <- avail_asd$continuous %>% cohAvail("asd_pc_")

asd_scatter <- ds.scatterPlot(
  x = "analysis_df$asd_age_", 
  y = "analysis_df$asd_pc_", 
  datasources = conns[asd_coh_any])

asd_coh <- c("elfe", "inma", "moba")

################################################################################
# 5. Language  
################################################################################
avail_lan <- dh.getStats(
  df = "analysis_df",
  vars = c("lan_pc_", "lan_raw_", "lan_instr_"), 
  conns = conns[names(conns) != "dnbc"]
)

lan_coh_any <- avail_lan$continuous %>% cohAvail("lan_pc_")

lan_scatter <- ds.scatterPlot(
  x = "analysis_df$lan_age_", 
  y = "analysis_df$lan_pc_", 
  datasources = conns[lan_coh_any])

lan_coh <- c("alspac", "inma", "rhea")


################################################################################
# 6. NVI  
################################################################################
avail_nvi <- dh.getStats(
  df = "analysis_df",
  vars = c("nvi_pc_", "nvi_raw_", "nvi_instr_"), 
  conns = conns[names(conns) != "dnbc"]
)

nvi_coh_any <- avail_nvi$continuous %>% cohAvail("nvi_pc_")

nvi_scatter <- ds.scatterPlot(
  x = "analysis_df$nvi_age_", 
  y = "analysis_df$nvi_pc_", 
  datasources = conns[nvi_coh_any])

nvi_coh <- c("alspac", "inma", "rhea")



