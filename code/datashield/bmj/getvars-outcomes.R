################################################################################
## Project: traj-ineq
## Script purpose: Derive outcomes
## Date: 14.12.22
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

## Do differently I think. Make the subsets. Rename the age terms in each subset
## to have the same name. Merge these (should get a more mangeagable size df).
## Then make the polynomial transformations.


################################################################################
# 1. Make polynomial transformations of age terms
################################################################################
age_vars <- c("int_age_", "ext_age_", "adhd_age_")

## ---- Add a small amount to the age term -------------------------------------
age_vars %>%
  map(function(x){
    
    ds.assign(
      toAssign = paste0("mhrep$", x, "+0.01"), 
      newobj = paste0(x, "c"))
  })

ds.dataFrame(
  x = c("mhrep", paste0(age_vars, "c")), 
  newobj = "age_vars_df")

dh.dropCols(
  df = "age_vars_df", 
  vars = c("child_id", age_vars, paste0(age_vars, "c")), 
  type = "keep")

## ---- Normal age terms -------------------------------------------------------
paste0(age_vars, "c") %>%
  map(
    ~dh.makeAgePolys(
      df = "age_vars_df", 
      age_var = .))

## ---- Split them for merging purposes -----------------------------------------


## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Fill  
################################################################################
ds.dataFrameFill("mhrep", "mhrep")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Fix factor variables  
################################################################################
instr.vars <- c("adhd_instr_", "ext_instr_", "int_instr_")

dh.columnCast(
  df = "mhrep",
  target_vars = instr.vars, 
  target_class = "factor")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Choose one measure per cohort per outcome  
################################################################################
################################################################################
# Define subsets  
################################################################################

## ---- Master table -----------------------------------------------------------
master_out.ref <- tibble(
  outcome = c("int", "ext", "adhd"),
  out_var = paste0(outcome, "_raw_"),
  age_var = paste0(outcome, "_age_"),
  instr_var = paste0(outcome, "_instr_"))

## ---- ALSPAC -----------------------------------------------------------------
alspac_out.ref <- master_out.ref %>%
  mutate(
    cohort = "alspac", 
    instr_name = "sdq", 
    instr_num = 47)

## ---- CHOP -------------------------------------------------------------------
chop_out.ref <- master_out.ref %>%
  mutate(
    cohort = "chop", 
    instr_name = "sdq", 
    instr_num = 47)

## ---- DNBC -------------------------------------------------------------------
dnbc_out.ref <- master_out.ref %>%
  mutate(
    cohort = "dnbc", 
    instr_name = "sdq", 
    instr_num = 47)

## ---- EDEN -------------------------------------------------------------------
eden_out.ref <- master_out.ref %>%
  mutate(
    cohort = "eden", 
    instr_name = "sdq", 
    instr_num = 47)

## ---- GEN-R ------------------------------------------------------------------
genr_out.ref <- tibble(
  outcome = c("int", "ext"),
  out_var = c("int_raw_", "ext_raw_"),
  instr_var = c("int_instr_", "ext_instr_"),
  age_var = c("int_age_", "ext_age_"),
  cohort = "genr",
  instr_name = "cbcl", 
  instr_num = 13)
  
## ---- INMA -------------------------------------------------------------------
#inma_out.ref <- tibble(
#  outcome = "adhd",
#  out_var = "adhd_raw_",
#  instr_var = "adhd_instr_",
#  age_var = "adhd_age_",
#  cohort = "inma",
#  instr_name = "cprs-d", 
#  instr_num = 20)

## ---- MoBa -------------------------------------------------------------------
moba_out.ref <- master_out.ref %>%
  mutate(
    cohort = "moba", 
    instr_name = "cbcl", 
    instr_num = c(13, 13, 13))

## ---- Raine ------------------------------------------------------------------
raine_out.ref <- master_out.ref %>%
  mutate(
    cohort = "raine", 
    instr_name = "cbcl", 
    instr_num = 13)

## ---- Rhea -------------------------------------------------------------------
rhea_out.ref <- master_out.ref %>%
  mutate(
    cohort = "rhea", 
    instr_name = c("cbcl", "cbcl", "cprs-d"), 
    instr_num = c(13, 13, 20))

## ---- Combine ----------------------------------------------------------------
out.ref <- bind_rows(alspac_out.ref, chop_out.ref, dnbc_out.ref, eden_out.ref, 
                     genr_out.ref, moba_out.ref, raine_out.ref, 
                     rhea_out.ref)

################################################################################
# Make subsets  
################################################################################
out.ref %>%
  pmap(function(outcome, instr_var, cohort, instr_name, instr_num, out_var,
                age_var, ...){
    
    ds.dataFrameSubset(
      df.name = "mhrep", 
      V1.name = paste0("mhrep$", instr_var), 
      V2.name = as.character(instr_num), 
      Boolean.operator = "==", 
      keep.NAs = FALSE, 
      newobj = "tmp_1", 
      datasources = conns[cohort])
    
    dh.dropCols(
      df = "tmp_1", 
      vars = c("child_id", out_var, age_var, instr_var),
      type = "keep",
      checks = F,
      new_obj = "tmp_2", 
      conns = conns[cohort])
    
    ds.completeCases(
      x1 = "tmp_2", 
      newobj = paste0(outcome, "_sub"), 
      datasources = conns[cohort])
    
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Rename age variables  
################################################################################
dh.renameVars(
  df = "int_sub", 
  current_names = "int_age_",
  new_names = "age")

dh.renameVars(
  df = "ext_sub", 
  current_names = "ext_age_",
  new_names = "age")

dh.renameVars(
  df = "adhd_sub", 
  current_names = "adhd_age_",
  new_names = "age", 
  conns = conns[names(conns) != "genr"])

getCoh <- function(var){
  
  out.ref %>% 
    dplyr::filter(outcome == var) %>%
    pull(cohort)
  
}

int.coh <- getCoh("int")
ext.coh <- getCoh("ext")
adhd.coh <- getCoh("adhd")

save.image()
## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Create square root transformations  
################################################################################
transformMH <- function(prefix, cohorts){
  
  ds.assign(
    paste0(prefix, "_sub$", prefix, "_raw_^0.5"), 
    paste0(prefix, "_raw_sqrt"), 
    datasources = conns[cohorts])
  
  ds.dataFrame(
    x = c(paste0(prefix, "_sub"), paste0(prefix, "_raw_sqrt")), 
    newobj = paste0(prefix, "_sub_t"), 
    datasources = conns[cohorts])
  
}

transformMH("int", int.coh)  
transformMH("ext", ext.coh)  
transformMH("adhd", adhd.coh)  
  
## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# Create z scores  
################################################################################
################################################################################
# Define measurement occasions  
################################################################################
int_meas <- tribble(
  ~cohort, ~outcome, ~occasion, ~age_low, ~age_high,
  "alspac", "int", 1, 3, 6,
  "alspac", "int", 2, 6, 8, 
  "alspac", "int", 3, 8, 9,
  "alspac", "int", 4, 9, 11,
  "alspac", "int", 5, 11, 13,
  "alspac", "int", 6, 13, 16,
  "alspac", "int", 7, 16, 18,
  "chop", "int", 1, 3, 6,
  "chop", "int", 2, 6, 13,
  "dnbc", "int", 1, 6, 9, 
  "dnbc", "int", 2, 9, 15,
  "eden", "int", 1, 2, 4,
  "eden", "int", 2, 4, 7,
  "eden", "int", 3, 7, 10,
  "genr", "int", 1, 0, 3, 
  "genr", "int", 2, 3, 5, 
  "genr", "int", 3, 5, 9, 
  "genr", "int", 4, 9, 13,
  "moba", "int", 1, 0, 2, 
  "moba", "int", 2, 2, 4, 
  "moba", "int", 3, 4, 7,
  "raine", "int", 1, 1, 4,
  "raine", "int", 2, 4, 7,
  "raine", "int", 3, 7, 9, 
  "raine", "int", 4, 9, 12, 
  "rhea", "int", 1, 5, 8, 
  "rhea", "int", 2, 8, 13)

ext_meas <- int_meas %>% mutate(outcome = "ext")

#ds.scatterPlot(
#  x = "adhd_sub$adhd_age_", 
#  y = "adhd_sub$adhd_raw_", 
#  datasources = conns[adhd.coh])

adhd_meas_dif <- tribble(
  ~cohort, ~outcome, ~occasion, ~age_low, ~age_high,
  "raine", "adhd", 1, 4, 7,
  "raine", "adhd", 2, 7, 10,
  "raine", "adhd", 3, 10, 12, 
  "raine", "adhd", 4, 12, 16, 
  "raine", "adhd", 5, 16, 18)

adhd_meas <- bind_rows(
  int_meas %>% dplyr::filter(!cohort %in% c("genr", "raine")), 
  adhd_meas_dif) %>%
  mutate(outcome = "adhd")

meas.ref <- bind_rows(int_meas, ext_meas, adhd_meas)

################################################################################
# Make z-scores
################################################################################
conns <- datashield.login(logindata, restore = "mh_traj")

## ---- Make z-scores ----------------------------------------------------------
meas.ref %>% 
  pmap(function(cohort, outcome, occasion, age_low, age_high){
    
    dh.zByGroup(
      df = paste0(outcome, "_sub_t"), 
      out_var = paste0(outcome, "_raw_"), 
      age_var = "age",
      low_band = age_low,
      upp_band = age_high, 
      new_obj = paste0(outcome, "_t_z.", occasion), 
      conns = conns[cohort])
    
  })

datashield.workspace_save(conns, "mh_traj")

## ---- Reference table for joining back into data frame -----------------------
z_join <- meas.ref %>%
  mutate(
    tmp_df = paste0(outcome, "_sub_t"),
    tmp_out = paste0(outcome, "_t_z.", occasion)) %>%
  group_by(cohort, outcome) %>%
  group_split %>%
  map(function(x){
    
    tibble(
      outcome = x$outcome[1],
      cohort = x$cohort[1],
      vars = list(c(x$tmp_df[1],  paste0(x$tmp_out))), 
      out_name = paste0(outcome, "_sub_t_z"))
    
  }) %>%
  bind_rows

################################################################################
# Join back into one data frame  
################################################################################
z_join %>% 
  pmap(function(cohort, vars, out_name, ...){
    
    ds.dataFrame(
      x = unlist(vars), 
      newobj = out_name, 
      datasources = conns[cohort])
    
  })

datashield.workspace_save(conns, "mh_traj")

################################################################################
# Reshape into long format  
################################################################################
z_join %>%
  pmap(function(outcome, cohort, vars, out_name){
    
    ds.reShape(
      data.name = out_name, 
      varying = vars[2:length(vars)], 
      idvar.name = c("child_id", "age"),
      newobj = paste0(out_name, "_l"), 
      direction = "long",
      datasources = conns[cohort])
    
  })

datashield.workspace_save(conns, "mh_traj")

