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
setwd("/Users/timcadman/OneDrive - University of Bristol/repos")

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


## Wrapper function to select vars and reshape 
mhIneqLong <- function(data, outcome, outcome_vars, 
	other_vars = c("aln", "qlet", "sex", "mat_ed_rank"),
	age_vars, zscore = FALSE){

long <- data %>%
dplyr::select(other_vars, outcome_vars, age_vars)

long <- reshape(data = long,
                varying = list(outcome_vars, age_vars),                                    
                direction = "long",
                v.names = c(outcome, "age"),
                idvar = "id", 
                ids = 1:nrow(long),
                timevar = "occasion", 
                times = age_vars)

long %<>%
mutate(!!paste0(outcome, age_m_2) := age ^ -2,
	   !!paste0(outcome, age_m_1) := age ^ -1,
	   !!paste0(outcome, age_m_0_5) := age ^ -0.5,
	   !!paste0(outcome, age_log) := log(age),
	   age_0_5 = age ^ 0.5,
	   age_2 = age ^ 2,
	   age_3 = age ^ 3)

## Sort by ID as this is needed for the binomial models and shouldn't harm
## the other models

long %<>% arrange(id)

return(long)

}

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

mlmPolyComb <- function(data, outcome, type = "Normal"){

## First arrange the data to keep it happy when fitting binomial model
#data <- arrange(data, id)

# Next get all combinations of polynomials. Here we have to make it work
# so model is fit with single polynomials and combinations

## Vector of polynomials
polys <- c("age", "age_m_2", "age_m_1", "age_m_0_5", "age_log", "age_0_5", 
	       "age_2", "age_3")

## Create all combinations of these
comb <- t(combn(combn(polys, 1, paste, collapse=""), 2))

## Create terms of these for model formulas
comb_form <- paste(comb[, 1], "+", comb[, 2])

## Combine these with each of the single polynomials
comb_form <- c(comb_form, polys)

# Finally we need to add the single polynomials to the df with the 
# combinations of polynomials. This is used later on for the table
# of fit statistics
comb <- rbind(comb, cbind(polys, NA))

## Run the models
poly.fit <- list()
converged <- vector()

for(i in 1:length(comb_form)){

if(type == "Normal"){
form <- paste0(outcome, " ~ ", "1 + ", comb_form[i], "+ ", 
	           "(1 + ", comb_form[i], "| id)")

est <- 0

} else if(type == "Binomial"){
form <- paste0("logit(", outcome, ")", " ~ ", "1 + ", comb_form[i], "+ ", 
	           "(1 + ", comb_form[i], "| id)")

est <- 1
}

poly.fit[[i]] <- runMLwiN(Formula = as.formula(form), 
                          estoptions = list(resi.store = TRUE, maxiter = 100,
                          	                EstM = est),
                          data = data, 
                          D = type)

if(poly.fit[[i]]@Converged == FALSE){

poly.fit[[i]] <- NULL
print("Model did not converge, coefficients not stored")
converged[i] <- FALSE

} else if(poly.fit[[i]]@Converged == TRUE){

poly.fit[[i]] <- poly.fit[[i]]
converged[i] <- TRUE

} 

}

## Display how many models removed
removed <- comb_form[which(converged == FALSE)]

if(length(removed) > 0){
cat("The following model(s) were removed due to non-convergence: ", 
	removed, sep = "\n")
} else if(length(removed) == 0){
print("All models converged succesfully")
}

## Remove non-converged models
poly.fit <- poly.fit[which(converged == TRUE)]


## Create table with fit statistics 
fit.tab <- data.frame(matrix(NA, nrow = length(poly.fit), ncol = 4))

colnames(fit.tab) <- c("Polynomial 1", "Polynomial 2", "Deviance", 
	                   "Log likelihood")

fit.tab[, 1:2] <- comb[which(converged == TRUE), ]
fit.tab[, 3] <- sapply(poly.fit, deviance)
fit.tab[, 4] <- sapply(poly.fit, logLik)

## Create a variable ranking the fit 
fit.tab %<>%
mutate(dev_rank = dense_rank(Deviance))

out <- list(poly.fit, fit.tab, data, outcome)

return(out)
}

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