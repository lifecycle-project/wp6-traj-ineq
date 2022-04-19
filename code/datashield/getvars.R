################################################################################
## Project: wp6-traj-ineq
## Script purpose: Prepare data for analysis    
## Date: 19th June 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

de <- function(){datashield.errors()}
################################################################################
# 1. Define variables and tables
################################################################################

## ---- Non-repeated -----------------------------------------------------------
nonrep.vars <- c("child_id", "sex", "coh_country", "cohort_id", "eusilc_income",
                 "eusilc_income_quintiles", "agebirth_m_y")

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
      "lc_chop_core_2_1.2_1_core_non_rep_bmi_earlylife_poc",
      "lc_chop_core_2_1.2_1_core_yearly_rep_mh_traj",
      "lc_chop_outcome_1_1.1_1_outcome_yearly_rep_mh_traj")),
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
      "lc_moba_core_2_1.2_1_core_2021_7_non_rep_inequalities_MH_trajectories",
      "lc_moba_outcome_1_1.1_1_outcome_2021_2_yearly_rep_inequalities_MH_trajectories",
      "lc_moba_outcome_1_1.1_1_outcome_2021_2_yearly_rep_inequalities_MH_trajectories")),
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
  dplyr::filter(conn_name %in% c("eden", "elfe", "alspac", "inma", "rhea")) %>%
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

ds.asNumeric("nonrep$eusilc_income", "eusilc_income")

ds.colnames("nonrep")

dh.dropCols(
  df = "nonrep", 
  vars = "eusilc_income")

ds.dataFrame(
  x = c("nonrep", "eusilc_income"), 
  newobj = "nonrep"
)


discrepancy$nonrep
discrepancy$mhrep

ds.levels("nonrep$sex")
ds.levels("nonrep$coh_country")
ds.levels("nonrep$cohort_id")


ds.levels("nonrep$eusilc_income")

ds.length("nonrep$eusilc_income")

ds.levels("nonrep$agebirth_m_y")



################################################################################
# 4. Sort out blank variables  
################################################################################
ds.dataFrameFill("nonrep", "nonrep")
ds.dataFrameFill("mhrep", "mhrep")




## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_2")
conns <- datashield.login(logindata, restore = "mhtraj_2")

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

## ---- Get proporstions -------------------------------------------------------
mat_prop <- dh.getStats(
  df = "baseline_wide",
  vars = "edu_m",
  conns = conns
)

mat_prop$categorical %>% print(n = Inf)

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
# 6. Merge datasets  
################################################################################
ds.merge(
  x.name = "nonrep",
  y.name = "mhrep",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  all.y = TRUE,
  newobj = "analysis_df"
)

ds.merge(
  x.name = "analysis_df", 
  y.name = "baseline_wide", 
  by.x.names = "child_id", 
  by.y.names = "child_id", 
  all.x = TRUE,
  all.y = TRUE,
  newobj = "analysis_df"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_6")
conns <- datashield.login(logindata, restore = "mhtraj_6")

################################################################################
# 7. Create different versions of id variable  
################################################################################

## ---- Integer ----------------------------------------------------------------
ds.asInteger("analysis_df$child_id", "child_id_int")

## ---- Factor -----------------------------------------------------------------
#ds.asFactor("child_id_int", "child_id_fac")

## ---- Merge back in ----------------------------------------------------------
ds.dataFrame(
  x = c("analysis_df", "child_id_int"), 
  newobj = "mh_df"
)

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
  new_df_name = "mh_df")

ds.dataFrame(
  x = c("mh_df", agevars), 
  newobj = "mh_df")
  
## ---- Normal age terms -------------------------------------------------------
agevars%>%
  map(
    dh.makeAgePolys(
      df = "mh_df", 
      agevars = .,
      conns = conns)
  )

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_8")
conns <- datashield.login(logindata, restore = "mhtraj_8")


################################################################################
# 9. Fix factor variables  
################################################################################

## ---- Convert to factor ------------------------------------------------------
inst.vars %>%
  map(
    ~ds.asFactor(
      input.var.name = paste0("mh_df$", .), 
      newobj = .
    )
  )

## ---- Remove originals from dataframe ----------------------------------------
dh.dropCols(
  df = "mh_df", 
  vars = fact.var, 
  type = "remove")

## ---- Join fixed versions back in --------------------------------------------
ds.dataFrame(
  x = c("mh_df", fact.var), 
  newobj = "mh_df"
)

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_9")
conns <- datashield.login(logindata, restore = "mhtraj_9")

################################################################################
# Create analysis dataframe
################################################################################

## ---- Exposures --------------------------------------------------------------
dh.defineCases(
  df = "mh_df", 
  vars = exp.vars, 
  type = "any", 
  new_obj = "any_exp"
)

## ---- Outcome ----------------------------------------------------------------
dh.defineCases(
  df = "mh_df", 
  vars = out.vars, 
  type = "any", 
  new_obj = "any_out"
)

## ---- At least one of each ---------------------------------------------------
ds.make(
  toAssign = "any_exp+any_out", 
  newobj = "n_complete")

ds.Boole(
  V1 = "n_complete", 
  V2 = "2", 
  Boolean.operator = "==", 
  na.assign = 0, 
  newobj = "some_vars")

## ---- Create subset ----------------------------------------------------------
ds.dataFrameSubset(
  df.name = "mh_df", 
  V1.name = "some_vars", 
  V2.name = "1", 
  Boolean.operator = "==", 
  keep.NAs = FALSE, 
  newobj = "analysis_df")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mhtraj_10")
conns <- datashield.login(logindata, restore = "mhtraj_10")







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



