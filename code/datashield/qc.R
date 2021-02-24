################################################################################
## Project: wp6-traj-ineq
## Script purpose: Write functions to do QC 
## Date: 30th September 2020 
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

################################################################################
# 1. Load data  
################################################################################

## Here we will use three small cohorts to get the function working
opals <- datashield.login(logindata, restore = "traj_assigned_30_09_20") 

opals <- opals["moba"]

ds.dim("cbcl_sub")

################################################################################
# 2. Subset dataframe  
################################################################################

## ---- Keep only externalising variables --------------------------------------
ds.dataFrameSubset(
  df = "cbcl_sub",
  V1.name = "cbcl_sub$ext_raw_",
  V2.name = "0",
  Boolean.operator = ">=",
  keep.NAs = FALSE,
  newobj = "cbcl_comp"
)

ds.dim("cbcl_comp")

ds.asNumeric("cbcl_comp$child_id", "id_num")

ds.dataFrame(
  x = c("cbcl_comp", "id_num"), 
  newobj = "cbcl_comp", 
  stringsAsFactors = FALSE
)

ds.table(
  rvar = "cbcl_comp$id_num",
  table.assign = TRUE,
  newobj = "id_table"
)

ds.scatterPlot(
  x = "cbcl_comp$ext_age_", 
  y = "cbcl_comp$ext_raw_")

ds.summary("cbcl_comp$id_int")

dh.countRM(
  df = "cbcl_comp",
  idvar = "child_id"
)




dh.dropCols(
  df = "cbcl_sub",
  vars = c("ext_age_", "ext_pc_", "ext_raw_", "child_id"),
  type = "keep",
  new_df_name = "cbcl_ext", 
  remove_temp = TRUE, 
  comp_var = "child_id"
)

ds.completeCases("cbcl_ext", newobj = "test")

ds.summary("test")

dh.countRM(
  df = "test",
  idvar = "child_id"
)

ds.summary("test$in_complete_obs")


## ---- Drop people who -------------------------------------------------------------------

## ---- Summarise number of observations ---------------------------------------
dh.countRM(
  df = "cbcl_ext",
  idvar = "child_id"
)

ds.summary("cbcl_ext$n_repeated_obs")

## ---- Exclude people with only one observation -------------------------------
ds.dataFrameSubset(
  df.name = "cbcl_ext",
  V1.name = "cbcl_ext$n_repeated_obs", 
  V2.name = "1", 
  Boolean.operator = ">", 
  newobj = "cbcl_ext_rep", 
  keep.NAs = FALSE
)

################################################################################
# 3. Save dataset  
################################################################################
datashield.workspace_save(opals, "cbcl_ext_qc")




