################################################################################
## Project: wp6-traj-ineq
## Script purpose: Prepare data for analysis    
## Date: 19th June 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

################################################################################
# 1. Define variables and tables
################################################################################

## ---- Non-repeated -----------------------------------------------------------
nonrep.vars <- c("child_id", "sex", "coh_country", "cohort_id", "eusilc_income",
                 "eusilc_income_quintiles", "agebirth_m_y", "ethn3_m")

## ---- Yearly repeated --------------------------------------------------------
yearrep.vars <- c("child_id", "edu_m_", "age_years")

## ---- Monthly repeated -------------------------------------------------------
mhrep.vars <- c("child_id", "age_years", "ext_raw_", "ext_age_", "ext_instr_", "ext_eval_",
                "ext_pro_", "ext_avg_", "ext_pc_", "int_raw_", "int_age_",
                "int_instr_", "int_eval_", "int_pro_", "int_avg_", "int_pc_",
                "asd_raw_", "asd_age_", "asd_instr_", "asd_eval_", "asd_pro_",
                "asd_avg_", "asd_pc_", "adhd_raw_", "adhd_age_", "adhd_instr_",
                "adhd_eval_", "adhd_pro_", "adhd_avg_", "adhd_pc_", "nvi_raw_",
                "nvi_age_", "nvi_instr_", "nvi_eval_", "nvi_pro_", "nvi_avg_",
                "nvi_pc_", "lan_raw_", "lan_age_", "lan_instr_", "lan_eval_",
                "lan_pro_", "lan_avg_", "lan_pc_", "adhdR_", "adhdR_age_")

## ---- Table names for cohorts ------------------------------------------------
cohorts_tables <- bind_rows(
  tibble(
    conn_name = "alspac",
    table = c(
      "alspac/2_1_core_1_4/non_rep",
      "alspac/2_1_core_1_4/yearly_rep",
      "alspac/1_1_outcome_1_4/yearly_rep")),
  tibble(
    conn_name = "bib",
    table = c(
      "sp443/2_2_core_1_3/non_rep",
      "sp443/2_2_core_1_3/yearly_rep",
      "sp443/1_1_outcome_1_2/yearly_rep")),
  tibble(
    conn_name = "chop",
    table = c(
      "mhtraj/2_2_core_1_0/non_rep",
      "mhtraj/2_2_core_1_0/yearly_rep",
      "mhtraj/1_5_outcome_1_0/yearly_rep")),
  tibble(
    conn_name = "dnbc",
    table = c(
      "lc_dnbc_core_2_2.2_2_core_non_rep_tcadman_2020-lc13",
      "lc_dnbc_core_2_2.2_2_core_yearly_rep_tcadman_2020-lc19",
      "lc_dnbc_outcome_1_2.1_2_outcome_yearly_rep_tcadman_2020-lc13")),
  tibble(
    conn_name = "eden",
    table = c(
      "project1-eden/2_1_core_1_0/non_rep",
      "project1-eden/2_1_core_1_0/yearly_rep",
      "project1-eden/1_1_outcome_1_0/yearly_rep")),
  tibble(
    conn_name = "elfe",
    table = c(
      "project1-elfe/2_1_core_1_0/non_rep",
      "project1-elfe/2_1_core_1_0/yearly_rep",
      "project1-elfe/1_1_outcome_1_0/yearly_rep")),
  tibble(
    conn_name = "genr",
    table = c(
      "lc_genr_core_2_2.2_2_core_non_rep_TC _ECCNLC202035",
      "lc_genr_core_2_2.2_2_core_yearly_rep_TC _ECCNLC202035",
      "lc_genr_outcome_1_2.1_2_outcome_yearly_rep_TC _ECCNLC202035")),
  tibble(
    conn_name = "inma",
    table = c(
      "lc_isglobal_core_2_1.2_1_core_1_0_non_rep_200513_1_Inequalities",
      "lc_isglobal_core_2_1.2_1_core_1_0_yearly_rep_200513_1_Inequalities",
      "lc_isglobal_outcome_1_1.1_1_outcome_1_0_yearly_rep_200513_1_Inequalities")), 
  tibble(
    conn_name = "moba",
    table = c(
      "lc_moba_core_2_1.2_1_core_2022_3_non_rep_early_determinants_adiposity_new",
      "lc_moba_core_2_1.2_1_core_2022_3_yearly_rep_inequalities_MH_trajectories_new",
      "lc_moba_outcome_1_1.1_1_outcome_2022_3_yearly_rep_inequalities_MH_trajectories_new")),
  tibble(
    conn_name = "nfbc86",
    table = c(
      "p0650/2_2_core_1_0/non_rep",
      "p0650/2_2_core_1_0/yearly_rep",
      "p0650/1_1_outcome_1_0/yearly_rep")),
  tibble(
    conn_name = "ninfea",
    table = c(
      "lc_ninfea_core_2_1.2_1_core_2_0_non_rep",
      "lc_ninfea_core_2_1.2_1_core_2_0_yearly_rep",
      "lc_ninfea_outcome_1_1.1_1_outcome_2_0_yearly_rep")),
  tibble(
    conn_name = "raine",
    table = c(
      "lc_raine_core_2_1.2_1_core_1_0_non_rep",
      "lc_raine_core_2_1.2_1_core_1_0_yearly_rep",
      "lc_raine_outcome_1_1.1_1_outcome_1_0_yearly_rep")),
  tibble(
    conn_name = "rhea",
    table = c(
      "lc_rhea_core_2_1.tcadman_nr",
      "lc_rhea_core_2_1.tcadman_y",
      "lc_rhea_outcome_1_1.tcadman_y")),
  tibble(
    conn_name = "sws",
    table = c(
      "lc_sws_core_2_1.2_1_core_1_1_non_rep",
      "lc_sws_core_2_1.2_1_core_1_1_yearly_rep",
      "lc_sws_outcome_1_1.1_1_outcome_1_1_yearly_rep"))) %>%
  mutate(type = rep(c("nonrep", "yearrep", "mhrep"), 14))


################################################################################
# 2. Assign variables
################################################################################
cohorts_tables %>%
  dplyr::filter(conn_name %in% "moba") %>%
  pwalk(function(conn_name, table, type){
    
    datashield.assign(
      conns = conns[conn_name], 
      symbol = type, 
      value = table, 
      variables = eval(parse(text = paste0(type, ".vars"))))
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_1")
conns <- datashield.login(logindata, restore = "mhtraj_1")

ds.colnames("nonrep")
################################################################################
# 3. See what's there   
################################################################################
discrepancy <- list(
  nonrep = 
    dh.classDiscrepancy(
      df = "nonrep",
      vars = nonrep.vars), 
  mhrep = dh.classDiscrepancy(
    df = "mhrep",
    vars = mhrep.vars))

save.image()

################################################################################
# 4. Sort out blank variables  
################################################################################
ds.dataFrameFill("nonrep", "nonrep")
ds.dataFrameFill("mhrep", "mhrep")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_2")
conns <- datashield.login(logindata, restore = "mhtraj_2")

################################################################################
# 5. Check filled variables  
################################################################################
filled <- list(
  nonrep = 
    dh.classDiscrepancy(
      df = "nonrep",
      vars = nonrep.vars), 
  mhrep = dh.classDiscrepancy(
    df = "mhrep",
    vars = mhrep.vars))

save.image()

################################################################################
# 4. Derive maternal education variable
################################################################################

## ---- Subset to keep only measurements from first year -----------------------
ds.dataFrameSubset(
  df.name = "yearrep", 
  V1.name = "yearrep$age_years",
  V2.name = "0",
  Boolean.operator = "==",
  newobj = "baseline_vars")

## ---- Reshape ----------------------------------------------------------------
ds.reShape(
  data.name = "baseline_vars",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = "edu_m_", 
  direction = "wide", 
  newobj = "baseline_wide")

## ---- Rename education variable ----------------------------------------------
dh.renameVars(
  df = "baseline_wide", 
  current_names = "edu_m_.0", 
  new_names = "edu_m")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_3")
conns <- datashield.login(logindata, restore = "mhtraj_3")

################################################################################
# 5. Create rank maternal education variable  
################################################################################

## ---- Get proportions -------------------------------------------------------
mat_prop <- dh.getStats(
  df = "baseline_wide",
  vars = "edu_m",
  conns = conns
)

## ---- Make tibble with values to assign --------------------------------------
rank <- mat_prop$categorical %>%
  mutate(
    rank_val = case_when(
      category == 1 ~ perc_valid/2,
      category == 2 ~ (lag(perc_valid, 1) + (lag(perc_valid, 1) + perc_valid)) / 2,
      category == 3 ~ 100 - (perc_valid/2)),
    rank_val = rank_val / 100) %>%
  dplyr::filter(cohort != "combined" & category != "missing") %>%
  group_by(cohort) %>%
  select(cohort, rank_val) %>%
  group_split %>%
  map(function(x){
    
    mutate(x, rank_vec = paste(rank_val, collapse = ",")) %>%
      head(1) %>%
      select(-rank_val)
  }) %>%
  bind_rows %>%
  mutate_at(vars(cohort), as.character)
    
## ---- Now recode education variable ------------------------------------------
rank %>%
  pmap(function(cohort, rank_vec){
    ds.recodeLevels(
    x = "baseline_wide$edu_m", 
    newCategories = unlist(str_split(rank_vec, ",")),
    datasources = conns[cohort], 
    newobj = "edu_rank")
  })

ds.asNumeric("edu_rank", newobj = "edu_rank_num")

ds.cbind(
  c("baseline_wide", "edu_rank_num"),
   newobj = "baseline_wide")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_4")
conns <- datashield.login(logindata, restore = "mhtraj_4")

################################################################################
# 6. Create rank income variable  
################################################################################

## ---- Get proporstions -------------------------------------------------------
#income_prop <- dh.getStats(
#  df = "nonrep",
#  vars = "eusilc_income_quintiles",
#  conns = conns
#)

## ---- Make tibble with values to assign --------------------------------------
## not correct

#income_rank <- income_prop[[1]] %>%
#  mutate(
#    rank_val = case_when(
#      category == 1 ~ perc_valid/2,
#      category == 2 ~ (perc_valid + lag(perc_valid, 1)) / 2,
#      category == 3 ~ (perc_valid + lag(perc_valid, 1)) / 2,
#      category == 4 ~ (perc_valid + lag(perc_valid, 1)) / 2,
#      category == 5 ~ 100 - (perc_valid/2)),
#    rank_val = rank_val / 100) %>%
#  filter(cohort != "combined" & category != "missing") %>%
#  group_by(cohort) %>%
#  select(cohort, rank_val) %>%
#  group_split %>%
#  map(function(x){
    
#    mutate(x, rank_vec = paste(rank_val, collapse = ",")) %>%
#      head(1) %>%
#      select(-rank_val)
#  }) %>%
#  bind_rows %>%
#  mutate_at(vars(cohort), as.character) 

## ---- Now recode education variable ------------------------------------------
#income_rank %>%
#  pmap(function(cohort, rank_vec){
#    ds.recodeLevels(
#      x = "nonrep$eusilc_income_quintiles", 
#      newCategories = unlist(str_split(rank_vec, ",")),
#      datasources = conns[cohort], 
#      newobj = "income_rank")
#  })

#ds.asNumeric("income_rank", newobj = "income_rank")

#ds.cbind(c("nonrep", "income_rank"), newobj = "nonrep")

## ---- Save progress ----------------------------------------------------------
#datashield.workspace_save(conns, "mhtraj_5")
#conns <- datashield.login(logindata, restore = "mhtraj_5")

################################################################################
# 7. Create maternal ethnicity variable  
################################################################################

## ---- Identify cohorts -------------------------------------------------------
eth_stats <- dh.getStats(
  df = "nonrep", 
  vars = "ethn3_m")

eth_coh <- eth_stats$categorical %>% print(n = Inf)

eth_coh <- eth_stats$categorical %>%
  dplyr::filter(perc_missing < 100 & cohort != "combined") %>%
  distinct(cohort) %>%
  pull(cohort)
  

## ---- Collapse categories ----------------------------------------------------
ds.recodeValues(
  var.name = "nonrep$ethn3_m",
  values2replace.vector = seq(1, 3, 1),
  new.values.vector = c(1, 2, 2),
  newobj = "eth_bin", 
  datasources = conns[eth_coh])

ds.table("eth_bin", useNA = "always", datasources = conns[eth_coh])

ds.dataFrame(
  c("nonrep", "eth_bin"), 
  newobj = "nonrep", 
  datasources = conns[eth_coh])

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_5")
conns <- datashield.login(logindata, restore = "mhtraj_5")

################################################################################
# 6. Merge datasets  
################################################################################
ds.merge(
  x.name = "nonrep",
  y.name = "mhrep",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  all.y = TRUE,
  newobj = "mh_df")

ds.merge(
  x.name = "mh_df", 
  y.name = "baseline_wide", 
  by.x.names = "child_id", 
  by.y.names = "child_id", 
  all.x = TRUE,
  all.y = TRUE,
  newobj = "mh_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_6")
conns <- datashield.login(logindata, restore = "mhtraj_6")

################################################################################
# 7. Create different versions of id variable  
################################################################################

## ---- Integer ----------------------------------------------------------------
ds.asInteger("mh_df$child_id", "child_id_int")

## ---- Factor -----------------------------------------------------------------
#ds.asFactor("child_id_int", "child_id_fac")

## ---- Merge back in ----------------------------------------------------------
ds.dataFrame(
  x = c("mh_df", "child_id_int"), 
  newobj = "mh_df")

#"child_id_f"
## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_7")
conns <- datashield.login(logindata, restore = "mhtraj_7")

################################################################################
# 8. Make polynomial transformations of age term
################################################################################
agevars <- c("ext_age_", "int_age_", "adhd_age_", "asd_age_", "lan_age_", 
             "nvi_age_")

## ---- Add a small amount to the age term -------------------------------------
agevars %>%
  map(function(x){
    
    ds.assign(
      toAssign = paste0("mh_df$", x, "+0.01"), 
      newobj = x)
  })
    
dh.dropCols(
  df = "mh_df", 
  vars = agevars, 
  type = "remove", 
  new_obj = "mh_df")

ds.dataFrame(
  x = c("mh_df", agevars), 
  newobj = "mh_df")
  
## ---- Normal age terms -------------------------------------------------------
agevars %>%
  map(
    ~dh.makeAgePolys(
      df = "mh_df", 
      age_var = .,
      conns = conns))

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_8")
conns <- datashield.login(logindata, restore = "mhtraj_8")

################################################################################
# Fix factor variables  
################################################################################
instr.vars <- c("adhd_instr_", "ext_instr_", "int_instr_", "lan_instr_", 
                "nvi_instr_", "asd_instr_")

dh.columnCast(
  df = "mh_df",
  target_vars = instr.vars, 
  target_class = "factor")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_9")
conns <- datashield.login(logindata, restore = "mhtraj_9")

################################################################################
# Create wide format of original dataset  
################################################################################
ds.reShape(
  data.name = "mh_df",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = "ext_raw_",
  direction = "wide", 
  newobj = "mh_df_w")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_10")
conns <- datashield.login(logindata, restore = "mhtraj_10")

################################################################################
# Define cases with exposure and one outcome
################################################################################

## ---- Exposures --------------------------------------------------------------
dh.defineCases(
  df = "mh_df", 
  vars = "edu_rank_num", 
  type = "any", 
  new_obj = "any_exp")

## ---- Outcome ----------------------------------------------------------------
dh.defineCases(
  df = "mh_df", 
  vars = c("adhd_raw_", "asd_raw_", "ext_raw_", "int_raw_", "lan_raw_", 
           "nvi_raw_"), 
  type = "any", 
  new_obj = "any_out")

## ---- At least one of each ---------------------------------------------------
ds.make(
  toAssign = "any_exp+any_out", 
  newobj = "n_complete")

ds.Boole(
  V1 = "n_complete", 
  V2 = "2", 
  Boolean.operator = "==", 
  na.assign = 0, 
  newobj = "exp_out")

## ---- Create long subset -----------------------------------------------------
ds.dataFrameSubset(
  df.name = "mh_df", 
  V1.name = "exp_out", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "exp_out_df_l")

## ---- Create wide subset -----------------------------------------------------
ds.reShape(
  data.name = "exp_out_df_l",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = "ext_raw_",
  direction = "wide", 
  newobj = "exp_out_df_w")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_11")
conns <- datashield.login(logindata, restore = "mhtraj_11")

################################################################################
# Define cases with non-missing covariates  
################################################################################
dh.defineCases(
  df = "exp_out_df_l", 
  vars = "sex", 
  type = "any", 
  new_obj = "any_cov")

## ---- Create long subset -----------------------------------------------------
ds.dataFrameSubset(
  df.name = "exp_out_df_l", 
  V1.name = "any_cov", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "analysis_df_l")

## ---- Create wide subset -----------------------------------------------------
ds.reShape(
  data.name = "analysis_df_l",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = "ext_raw_",
  direction = "wide", 
  newobj = "analysis_df_w")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_12")
conns <- datashield.login(logindata, restore = "mhtraj_12")

################################################################################
# Define excluded DFs  
################################################################################
dt.makeExcludedDf(
  original_df = "mh_df_w", 
  final_df = "analysis_df_w",
  new_obj = "excluded_w")

#dt.makeExcludedDf(
#  original_df = "mh_df", 
#  final_df = "analysis_df_l",
#  new_obj = "excluded_l")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_13")
conns <- datashield.login(logindata, restore = "mhtraj_13")

################################################################################
# Make complete case subsets
################################################################################
out_stem <- c("int_", "ext_", "adhd_", "asd_", "nvi_", "lan_")
exp_cov.vars <- c("child_id", "edu_rank_num", "sex")

## ---- Define subsets ---------------------------------------------------------
miss.ref <- tibble(
  out_stem = out_stem,
  outcome = paste0(out_stem, "raw_"), 
  age_var = paste0(out_stem, "age_"), 
  new_vec = paste0(out_stem, "valid"),
  inc_long = paste0(out_stem, "sub_l_inc"),
  inc_wide = paste0(out_stem, "sub_w_inc"),
  exc_long = paste0(out_stem, "sub_l_exc"),
  exc_wide = paste0(out_stem, "sub_w_exc"),
  cohort = c(
    list(int_coh), 
    list(ext_coh), 
    list(adhd_coh), 
    list(asd_coh), 
    list(nvi_coh),
    list(lan_coh)),
  vars = c(
    list(c("int_raw_", "int_age_", exp_cov.vars)), 
     list(c("ext_raw_", "ext_age_", exp_cov.vars)), 
     list(c("adhd_raw_", "adhd_age_", exp_cov.vars)), 
     list(c("asd_raw_", "asd_age_", exp_cov.vars)), 
     list(c("nvi_raw_", "nvi_age_", exp_cov.vars)), 
     list(c("lan_raw_", "lan_age_", exp_cov.vars))))

save.image()
  
## ---- Drop variables ---------------------------------------------------------
miss.ref %>%
  pmap(function(new_vec, vars, cohort, ...){
    
    dh.defineCases(
      df = "mh_df", 
      vars = unlist(vars),
      type = "all", 
      new_obj = new_vec,
      checks = FALSE, 
      conns = conns[cohort])
    
  })

datashield.workspace_save(conns, "mhtraj_14a")
conns <- datashield.login(logindata, restore = "mhtraj_14a")

## ---- Create included subset, long -------------------------------------------  
miss.ref %>%
  pmap(function(new_vec, inc_long, cohort, ...){
    
    ds.dataFrameSubset(
      df.name = "mh_df", 
      V1.name = new_vec, 
      V2.name = "1",
      Boolean.operator = "==",
      keep.NAs = FALSE, 
      newobj = inc_long, 
      datasources = conns[unlist(cohort)])
    
  }) 

datashield.workspace_save(conns, "mhtraj_14b")
conns <- datashield.login(logindata, restore = "mhtraj_14b")

## ---- Create excluded subset, long -------------------------------------------
miss.ref %>%
  pmap(function(new_vec, exc_long, cohort, ...){
    
    ds.dataFrameSubset(
      df.name = "mh_df", 
      V1.name = new_vec, 
      V2.name = "0",
      Boolean.operator = "==",
      keep.NAs = FALSE, 
      newobj = exc_long, 
      datasources = conns[cohort])
    
    }) 

datashield.workspace_save(conns, "mhtraj_14c")
conns <- datashield.login(logindata, restore = "mhtraj_14c")

## ---- Create included subset, wide -------------------------------------------
miss.ref %>%
  pmap(function(inc_long, age_var, outcome, inc_wide, cohort, ...){
    
    ds.reShape(
      data.name = inc_long,
      timevar.name = age_var,
      idvar.name = "child_id",
      v.names = outcome,
      direction = "wide", 
      newobj = inc_wide, 
      datasources = conns[cohort])
    
  }) 

datashield.workspace_save(conns, "mhtraj_14d")
conns <- datashield.login(logindata, restore = "mhtraj_14d")

## ---- Create excluded subset, wide -------------------------------------------
miss.ref %>%
  pmap(function(exc_long, age_var, outcome, exc_wide, cohort, ...){
    
    ds.reShape(
      data.name = exc_long,
      timevar.name = age_var,
      idvar.name = "child_id",
      v.names = outcome,
      direction = "wide", 
      newobj = exc_wide, 
      datasources = conns[cohort])
    
  }) 

datashield.workspace_save(conns, "mhtraj_14e")
conns <- datashield.login(logindata, restore = "mhtraj_14e")

################################################################################
# Tidy up  
################################################################################
dh.tidyEnv(
  obj = c("nonrep", "yearrep", "mhrep", miss.ref$inc_long, miss.ref$inc_wide,
          miss.ref$exc_long, miss.ref$exc_wide, "mh_df", "mh_df_w", "any_exp",
          "any_out", "n_complete", "exp_out", "exp_out_df_l", "exp_out_df_w",
          "any_cov", "analysis_df_l", "analysis_df_w", "excluded_w"), 
  type = "keep")

datashield.workspace_save(conns, "mhtraj_14")
conns <- datashield.login(logindata, restore = "mhtraj_14")

ds.ls()

################################################################################
# Create Male and Female subsets  
################################################################################

## ---- Male -------------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df_l", 
  V1.name = "analysis_df_l$sex", 
  V2.name = "1",
  Boolean.operator = "==", 
  newobj = "analysis_df_l_m")

## ---- Female -----------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df_l", 
  V1.name = "analysis_df_l$sex", 
  V2.name = "1",
  Boolean.operator = "==", 
  newobj = "analysis_df_l_f")

datashield.workspace_save(conns, "mhtraj_15")
conns <- datashield.login(logindata, restore = "mhtraj_15")






################################################################################
# Create outcomes at different timepoints to describe missingness  
################################################################################
out_prefix <- c("adhd_", "ext_", "int_", "lan_", "nvi_", "asd_") 

out_prefix %>%
  map(
    ~dh.makeStrata(
      df = "analysis_df_l", 
      id_var = "child_id", 
      age_var = paste0(.x, "age_"), 
      var_to_subset = paste0(.x, "pc_"), 
      bands = c(0, 4, 4, 8, 8, 12, 12, 18),
      band_action = "ge_l",
      mult_action = "earliest", 
      new_obj = paste0(.x, "m_s"), 
      checks = FALSE, 
      conns = conns["raine"]))

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_13")
conns <- datashield.login(logindata, restore = "mhtraj_13")

ds.ls()










################################################################################
# 9. Describe instruments used   
################################################################################

## As I'm going to work with the raw scores, I need to identify which cohorts
## have what measure at how many time points

## ---- Get descriptives -------------------------------------------------------
instruments <- dh.getStats(
  df = "mhrep", 
  vars = c("ext_instr_", "int_instr_"),
  conns = conns)

mh_available <- instruments[[1]] %>%  mutate(
  instrument = case_when(
    category == 13 ~ "CBCL",
    category == 47 ~ "SDQ"))
  
## ---- Create vectors of cohort names with these measures ---------------------  
ext_pc_avail = mh_available %>% 
  filter(variable == "ext_instr_" & value >0 & cohort != "combined")

int_pc_avail = mh_available %>% 
  filter(variable == "int_instr_" & value >0 & cohort != "combined")

ext_cbcl_avail <- mh_available %>% 
  filter(variable == "ext_instr_" & category == 13 & value >0 & cohort != "combined")
  
ext_sdq_avail <- mh_available %>% 
  filter(variable == "ext_instr_" & category == 47 & value >0 & cohort != "combined")
  
int_cbcl_avail <- mh_available %>% 
  filter(variable == "int_instr_" & category == 13 & value >0 & cohort != "combined")

int_sdq_avail <- mh_available %>% 
  filter(variable == "int_instr_" & category == 47 & value >0 & cohort != "combined")
  
ext_cbcl_tmp <- ext_cbcl_avail %>% pull(cohort) %>% as.character %>% unique
ext_sdq_tmp <- ext_sdq_avail %>% pull(cohort) %>% as.character %>% unique
int_cbcl_tmp <- int_cbcl_avail %>% pull(cohort) %>% as.character %>% unique  
int_sdq_tmp <- int_sdq_avail %>% pull(cohort) %>% as.character %>% unique  

ext_pc_coh_lin <- c("chop", "dnbc", "inma", "moba", "raine")
int_pc_coh_lin <- c("chop", "inma", "moba", "raine")

################################################################################
# 10. Create subsets
################################################################################

## Now we create subsets based on available instruments

## ---- Externalising CBCL -----------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$ext_instr_", 
  V2.name = "13",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "ext_cbcl_sub", 
  datasources = conns[ext_cbcl_tmp]
)

## ---- Externalising SDQ ------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$ext_instr_", 
  V2.name = "47",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "ext_sdq_sub", 
  datasources = conns[ext_sdq_tmp]
)

## ---- Internalising CBCL -----------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$int_instr_", 
  V2.name = "13",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "int_cbcl_sub", 
  datasources = conns[int_cbcl_tmp]
)

## ---- Internalising SDQ ------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$int_instr_", 
  V2.name = "47",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "int_sdq_sub", 
  datasources = conns[int_sdq_tmp]
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_9")
conns <- datashield.login(logindata, restore = "mhtraj_9")



