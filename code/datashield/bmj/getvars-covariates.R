################################################################################
## Project: traj-ineq
## Script purpose: Derive covariates
## Date: 04.12.22
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

################################################################################
# Create different versions of id variable  
################################################################################
ds.asInteger("nonrep$child_id", "child_id_int")

ds.dataFrame(
  x = c("nonrep", "child_id_int"), 
  newobj = "nonrep")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Create maternal ethnicity variable  
################################################################################

## ---- Identify cohorts -------------------------------------------------------
eth_stats <- dh.getStats(
  df = "nonrep", 
  vars = "ethn3_m")

eth_coh <- eth_stats$categorical %>%
  dplyr::filter(perc_missing < 100 & cohort != "combined") %>%
  distinct(cohort) %>%
  pull(cohort)

save.image()

## ---- Collapse categories ----------------------------------------------------
#ds.recodeValues(
#  var.name = "nonrep$ethn3_m",
#  values2replace.vector = seq(1, 3, 1),
#  new.values.vector = c(1, 2, 2),
#  newobj = "eth_bin", 
#  datasources = conns[eth_coh])

ds.recodeLevels(
  x = "nonrep$ethn3_m",
  newCategories = c(1, 2, 2),
  newobj = "eth_bin", 
  datasources = conns[eth_coh])

#ds.table("eth_bin", useNA = "always", datasources = conns[eth_coh])
## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")
