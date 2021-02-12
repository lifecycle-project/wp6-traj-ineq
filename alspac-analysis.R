################################################################################
## Project: lc-traj-ineq
## Script purpose: Conduct the MLM analysis for ALSPAC
## Date: 12th February 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(stringr)
library(tableone)
library(Hmisc)
library(magrittr)
library(dplyr)

setwd("C:/Users/timca/OneDrive - University of Bristol/repos")

source("./mh-ineq/code/mh-ineq-draft-functions.R")
################################################################################
# 1. Get data  
################################################################################
load("//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p6/048/working/data/wp1/lc_data_v1_1.RData")

mh.data <- lc.data

################################################################################
# 2. Make variable vectors  
################################################################################
ext_sdq.vars <- colnames(mh.data)[str_detect(colnames(mh.data), "ext_raw_")]
ext_age.vars <- colnames(mh.data)[str_detect(colnames(mh.data), "ext_age_")]
int_sdq.vars <- colnames(mh.data)[str_detect(colnames(mh.data), "int_raw_")]
int_age.vars <- colnames(mh.data)[str_detect(colnames(mh.data), "int_age_")]


################################################################################
# 3. Calculate rank education variable  
################################################################################
Hmisc::describe(mh.data$edu_m_0)
Hmisc::describe(mh.data$edu_rank)

mh.data %<>%
mutate(mat_ed_rank = case_when(
edu_m_0 == 1 ~ 0.129 / 2,
edu_m_0 == 2 ~ (0.129 + (0.129 + 0.669)) / 2,
edu_m_0 == 3 ~ (1 - 0.202) / 2)
)


################################################################################
# 4. Convert to long format  
################################################################################

## ---- SDQ externalising ------------------------------------------------------
ext_sdq.data <- mhIneqLong(data = mh.data, 
	                   outcome = "ext_sdq", 
                       outcome_vars = ext_sdq.vars, 
                       age_vars = ext_age.vars, 
                       zscore = FALSE)

## ---- SDQ emotional ----------------------------------------------------------
int_sdq.data <- mhIneqLong(data = mh.data, 
                           outcome = "int_sdq",
                           outcome_vars = int_sdq.vars, 
                           age_vars = int_age.vars, 
                           zscore = FALSE)


################################################################################
# 5. Run all combinations of polynomials  
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_sdq_poly.fit <- mlmPolyComb(
	data = ext_sdq.data, 
	outcome = "ext_sdq", 
	type = "Normal")

## ---- Internalising ----------------------------------------------------------
int_sdq_poly.fit <- mlmPolyComb(
	data = int_sdq.data, 
	outcome = "int_sdq", 
	type = "Normal")


################################################################################
# 6. Summarise best fitting models  
################################################################################
write.csv(ext_sdq_poly.fit[[2]], "./wp6-traj-ineq/ext_sdq_fit.csv")
write.csv(int_sdq_poly.fit[[2]], "./wp6-traj-ineq/int_sdq_fit.csv")

int_sdq_poly.fit$fit.tab