################################################################################
## Project: mh-ineq
## Script purpose: Create tables for manuscript
## Date: 4th November 2019
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################
library(purrr)

path <- "z:/projects/ieu2/p6/069/working/data/"
################################################################################
# Table 1  
################################################################################

load(paste0(path, "table1.RData"))

## ---- Convert list to dataframe ----------------------------------------------
table1.df <- map_df(table1.data, ~as.data.frame(.x), .id="id")

table1.df %<>%
mutate(occ_years = round(occasion_num, 0))

str(ans)