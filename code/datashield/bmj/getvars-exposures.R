################################################################################
## Project: traj-ineq   
## Script purpose: Derive exposures
## Date: 14.12.22
## Author: Tim Cadman
## Email: tica@sund.ku.dk
################################################################################

################################################################################
# 1. Derive maternal education variable
################################################################################

## ---- Subset to keep only measurements from first year -----------------------
ds.dataFrameSubset(
  df.name = "yearrep", 
  V1.name = "yearrep$age_years",
  V2.name = "0",
  Boolean.operator = "==",
  newobj = "year_zero_df")

## ---- Reshape ----------------------------------------------------------------
ds.reShape(
  data.name = "year_zero_df",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = "edu_m_", 
  direction = "wide", 
  newobj = "mat_ed_df")

## ---- Rename education variable ----------------------------------------------
dh.renameVars(
  df = "mat_ed_df", 
  current_names = "edu_m_.0", 
  new_names = "edu_m")

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# 2. Create rank maternal education variable  
################################################################################

## ---- Get proportions -------------------------------------------------------
mat_prop <- dh.getStats(
  df = "mat_ed_df",
  vars = "edu_m",
  conns = conns)

test <- mat_prop$categorical

test$perc_valid
lag(test$perc_valid, 1)

## ---- Make tibble with values to assign --------------------------------------
rankAv <- function(x){
  
  a <- c(0, x)
  b <- cumsum(a)
  
  c <- 2:length(b) %>% 
    map_dbl(~((b[.x] + lag(b, 1)[.x]) / 2))
  
  as.character(c / 100)
  
}

mat_ed_rank.ref <- mat_prop$categorical %>%
  dplyr::filter(cohort != "combined" & !is.na(category)) %>%
  group_by(cohort) %>%
  group_split %>%
  map(~mutate(., rank_val = rankAv(.x$perc_valid))) %>%
  bind_rows %>%
  dplyr::select(cohort, category, rank_val) %>%
  mutate(category = as.numeric(category))

## ---- Now recode education variable ------------------------------------------
ds.asNumeric("mat_ed_df$edu_m", "edu_rank")

#mat_ed_rank.ref %>%
#  pmap(function(cohort, category, rank_val){
#    ds.recodeValues(
#      var.name = "edu_rank", 
#      values2replace.vec = category,
#      new.values.vector = rank_val,
#      datasources = conns[cohort], 
#      force.output.format = "numeric",
#      newobj = "edu_rank")
#  })

mat_ed_rank.ref %>%
  group_by(cohort) %>%
  group_split %>%
  map(function(x){
    
    tibble(
      cohort = x$cohort[1], 
      new_cats = list(x$rank_val))
    
  }) %>%
  bind_rows %>%
  pmap(function(cohort, new_cats, ...){
    ds.recodeLevels(
      x = "mat_ed_df$edu_m", 
      newCategories = new_cats,
      datasources = conns[cohort], 
      newobj = "edu_rank")
  })

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")

################################################################################
# 3. Create rank income variable  
################################################################################

## ---- Get proportions -------------------------------------------------------
income_prop <- dh.getStats(
  df = "nonrep",
  vars = "eusilc_income_quintiles",
  conns = conns)

## ---- Make tibble with values to assign --------------------------------------
income_rank.ref <- income_prop$categorical %>%
  dplyr::filter(cohort != "combined" & !is.na(category) & cohort != "raine") %>%
  group_by(cohort) %>%
  group_split %>%
  map(~mutate(., rank_val = rankAv(.x$perc_valid))) %>%
  bind_rows %>%
  dplyr::select(cohort, category, rank_val)
  
## ---- Now recode income variable ---------------------------------------------
#ds.asNumeric("nonrep$eusilc_income_quintiles", "income_rank")

income_rank.ref %>%
  pmap(function(cohort, category, rank_val){
    ds.recodeValues(
      var.name = "income_rank", 
      values2replace.vec = category,
      new.values.vector = rank_val,
      datasources = conns[cohort], 
      force.output.format = "numeric",
      newobj = "income_rank")
  })

income_rank.ref %>%
  group_by(cohort) %>%
  group_split %>%
  map(function(x){
    
    tibble(
      cohort = x$cohort[1], 
      new_cats = list(x$rank_val))
    
  }) %>%
  bind_rows %>%
  pmap(function(cohort, new_cats, ...){
    ds.recodeLevels(
      x = "nonrep$eusilc_income_quintiles", 
      newCategories = new_cats,
      datasources = conns[cohort], 
      newobj = "income_rank")
  })
  
ds.table("income_rank", datasources = conns[names(conns) != "raine"])

ds.assign("nonrep$eusilc_income_quintiles", "income_rank", 
          datasources = conns[names(conns) == "raine"])

## ---- Save progress ----------------------------------------------------------
datashield.workspace_save(conns, "mh_traj")
conns <- datashield.login(logindata, restore = "mh_traj")
