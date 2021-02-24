################################################################################
## Project: mh-ineq
## Script purpose: Run singular models for the best fitting identified
## Date: 5th November 2019
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(R2MLwiN)
library(lspline)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gtools)
library(stats)
library(purrr)

data_path <- "z:/projects/ieu2/p6/069/working/data/"

################################################################################
# 1. Get data  
################################################################################

## ---- Functions --------------------------------------------------------------
source("./mh-ineq/code/mh-ineq-draft-functions.R")
source("./useful-code-r/code/functions/listN.R")

## ---- Data -------------------------------------------------------------------
load(file = "z:/projects/ieu2/p6/069/working/data/spline.RData")
load(file = "z:/projects/ieu2/p6/069/working/data/bin.RData")


################################################################################
# 2. Create list of datasets  
################################################################################
spline_names <- names(spline_data$data)
bin_names <- names(bin_data$data)


################################################################################
# 3. Get centring values  
################################################################################

centerVals <- function(x, list_name){

list_name$data[[x]]$age[1] - list_name$data[[x]]$age_cen[1]	
}

## ---- Continuous -------------------------------------------------------------
spline_data$centre_val <- lapply(
	names(spline_data$data), centerVals, spline_data) %>%
setNames(., spline_names)


## ---- Categorical ------------------------------------------------------------
bin_data$centre_val <- lapply(
	names(bin_data$data), centerVals, bin_data) %>%
setNames(., bin_names)


################################################################################
# 3. Make splines for spline models  
################################################################################

## ---- Define knots -----------------------------------------------------------
spline_data$knots$mfq <- c(153.6, 224.4, 262.8)
spline_data$knots$sdq_emo <- 86 
spline_data$knots$sdq_peer <- 63 
spline_data$knots$sdq_con <- 101 
spline_data$knots$sdq_hyp <- 87 
spline_data$knots$epds_mat <- c(7, 15, 42, 111)
spline_data$knots$epds_pat <- c(8, 16, 44, 112)

names(spline_data$knots) <- spline_names


## ---- Centre knots -----------------------------------------------------------
spline_data$knots_cen <- lapply(names(spline_data$knots), function(x) {
	spline_data$knots[[x]] - spline_data$centre_val[[x]]}) %>%
setNames(., spline_names)

## ---- Get splines ------------------------------------------------------------
spline_data$data <- lapply(names(spline_data$data), function(x){

splines <- data.frame(lspline(x = spline_data$data[[x]]$age_cen, 
	                          knots = spline_data$knots_cen[[x]]))

colnames(splines) <- paste0("s", 
	seq(1, length(spline_data$knots_cen[[x]]) + 1, 1))

out <- cbind(spline_data$data[[x]], splines)
return(out) }) 

names(spline_data$data) <- spline_names


################################################################################
# 4. Run spline models based on raw scores
################################################################################

## ---- MFQ --------------------------------------------------------------------
spline_data$fit$mfq <- runMLwiN(
	Formula = mfq ~ 1 + mat_ed_rank + s1 + s2 + s3 + s4 +
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + 
              (mat_ed_rank * s3) + (mat_ed_rank * s4) + 
	          (1 + s1 + s2 + s3 + s4 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$mfq)

## ---- SDQ emotional ----------------------------------------------------------
spline_data$fit$sdq_emo <- runMLwiN(
	Formula = sdq_emo ~ 1 + mat_ed_rank + s1 + s2 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + 
	          (1 + s1 + s2 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$sdq_emo)

## ---- SDQ peer ---------------------------------------------------------------
spline_data$fit$sdq_peer <- runMLwiN(
	Formula = sdq_peer ~ 1 + mat_ed_rank + s1 + s2 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + 
	          (1 + s1 + s2 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$sdq_peer)

## ---- SDQ conduct ------------------------------------------------------------
spline_data$fit$sdq_con <- runMLwiN(
	Formula = sdq_con ~ 1 + mat_ed_rank + s1 + s2 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + 
	          (1 + s1 + s2 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$sdq_con)


## ---- SDQ hyperactivity ------------------------------------------------------
spline_data$fit$sdq_hyp <- runMLwiN(
	Formula = sdq_hyp ~ 1 + mat_ed_rank + s1 + s2 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + 
	          (1 + s1 + s2 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$sdq_hyp)


## ---- Maternal EPDS ----------------------------------------------------------
spline_data$fit$epds_mat <- runMLwiN(
	Formula = epds_mat ~ 1 + mat_ed_rank + s1 + s2 + s3 + s4 + s5 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + (mat_ed_rank * s3) + 
	          (mat_ed_rank * s4) + (mat_ed_rank * s5) + 
	          (1 + s1 + s2 + s3 + s4 + s5 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$epds_mat)


## ---- Paternal EPDS ----------------------------------------------------------
spline_data$fit$epds_pat <- runMLwiN(
	Formula = epds_pat ~ 1 + mat_ed_rank + s1 + s2 + s3 + s4 + s5 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + (mat_ed_rank * s3) + 
	          (mat_ed_rank * s4) + (mat_ed_rank * s5) + 
	          (1 + s1 + s2 + s3 + s4 + s5 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$epds_pat)


################################################################################
# 5. Run spline models based on z-scores
################################################################################
spline_data_z <- spline_data

## ---- MFQ --------------------------------------------------------------------
spline_data_z$fit$mfq <- runMLwiN(
	Formula = mfq_z ~ 1 + mat_ed_rank + s1 + s2 + s3 + s4 +
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + 
              (mat_ed_rank * s3) + (mat_ed_rank * s4) + 
	          (1 + s1 + s2 + s3 + s4 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$mfq)

## ---- SDQ emotional ----------------------------------------------------------
spline_data_z$fit$sdq_emo <- runMLwiN(
	Formula = sdq_emo_z ~ 1 + mat_ed_rank + s1 + s2 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + 
	          (1 + s1 + s2 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$sdq_emo)

## ---- SDQ peer ---------------------------------------------------------------
spline_data_z$fit$sdq_peer <- runMLwiN(
	Formula = sdq_peer_z ~ 1 + mat_ed_rank + s1 + s2 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + 
	          (1 + s1 + s2 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$sdq_peer)

## ---- SDQ conduct ------------------------------------------------------------
spline_data_z$fit$sdq_con <- runMLwiN(
	Formula = sdq_con_z ~ 1 + mat_ed_rank + s1 + s2 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + 
	          (1 + s1 + s2 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$sdq_con)

## ---- SDQ hyperactivity ------------------------------------------------------
spline_data_z$fit$sdq_hyp <- runMLwiN(
	Formula = sdq_hyp_z ~ 1 + mat_ed_rank + s1 + s2 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + 
	          (1 + s1 + s2 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$sdq_hyp)

## ---- Maternal EPDS ----------------------------------------------------------
spline_data_z$fit$epds_mat <- runMLwiN(
	Formula = epds_mat_z ~ 1 + mat_ed_rank + s1 + s2 + s3 + s4 + s5 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + (mat_ed_rank * s3) + 
	          (mat_ed_rank * s4) + (mat_ed_rank * s5) + 
	          (1 + s1 + s2 + s3 + s4 + s5 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$epds_mat)


## ---- Paternal EPDS ----------------------------------------------------------
spline_data_z$fit$epds_pat <- runMLwiN(
	Formula = epds_pat_z ~ 1 + mat_ed_rank + s1 + s2 + s3 + s4 + s5 + 
	          (mat_ed_rank * s1) + (mat_ed_rank * s2) + (mat_ed_rank * s3) + 
	          (mat_ed_rank * s4) + (mat_ed_rank * s5) + 
	          (1 + s1 + s2 + s3 + s4 + s5 | id), 
    estoptions = list(resi.store = TRUE),
    data = spline_data$data$epds_pat)


################################################################################
# 6. Run binary models  
################################################################################

## ---- DAWBA ADHD -------------------------------------------------------------
bin_data$fit$dawba_adhd <- runMLwiN(
	Formula = logit(dawba_adhd) ~ 1 + mat_ed_rank + age_cen + 
	               (mat_ed_rank * age_cen) + (1 | id), 
    estoptions = list(resi.store = TRUE, EstM = 1),
    data = bin_data$data$dawba_adhd, 
    D = "Binomial")


## ---- DAWBA conduct ----------------------------------------------------------
bin_data$fit$dawba_con <- runMLwiN(
	Formula = logit(dawba_con) ~ 1 + mat_ed_rank + age_cen + 
	               (mat_ed_rank * age_cen) + (1 | id), 
    estoptions = list(resi.store = TRUE, EstM = 1),
    data = bin_data$data$dawba_con, 
    D = "Binomial")


## ---- DAWBA ODD --------------------------------------------------------------
bin_data$fit$dawba_odd <- runMLwiN(
	Formula = logit(dawba_odd) ~ 1 + mat_ed_rank + age_cen + 
	               (mat_ed_rank * age_cen) + (1 | id), 
    estoptions = list(resi.store = TRUE, EstM = 1),
    data = bin_data$data$dawba_odd, 
    D = "Binomial")


## ---- DAWBA depression -------------------------------------------------------
bin_data$fit$dawba_dep <- runMLwiN(
	Formula = logit(dawba_dep) ~ 1 + mat_ed_rank + age_cen + 
	          (mat_ed_rank * age_cen) + (1 | id), 
    estoptions = list(resi.store = TRUE, EstM = 1),
    data = bin_data$data$dawba_dep, 
    D = "Binomial")


## ---- DAWBA anxiety ----------------------------------------------------------
bin_data$fit$dawba_anx <- runMLwiN(
	Formula = logit(dawba_anx) ~ 1 + mat_ed_rank + age_cen + 
	          (mat_ed_rank * age_cen) + (1 | id), 
    estoptions = list(resi.store = TRUE, EstM = 1),
    data = bin_data$data$dawba_anx, 
    D = "Binomial")


## ---- PLIKS ------------------------------------------------------------------
bin_data$fit$pliks <- runMLwiN(
	Formula = logit(pliks) ~ 1 + mat_ed_rank + age_cen + 
	               (mat_ed_rank * age_cen) + (1 | id), 
    estoptions = list(resi.store = TRUE, EstM = 1),
    data = bin_data$data$pliks,
    D = "Binomial")

names(spline_data$fit) <- spline_names
names(spline_data_z$fit) <- spline_names
names(bin_data$fit) <- bin_names


################################################################################
# 7. Get predicted values  
################################################################################

## ---- Spline -----------------------------------------------------------------
predSpline <- function(x, list_name){

pred <- data.frame(predict(list_name$fit[[x]]))
pred$age <- list_name$data[[x]]$age_cen

pred$mat_ed_rank <- factor(list_name$data[[x]]$mat_ed_rank, 
	labels = c("CSE/none", "Vocational", "O-level", "A-level", "Degree"))

pred %<>%
filter(!is.na(mat_ed_rank))

names(pred)[1] <- "predicted"

return(pred)
}

spline_data$predicted <- lapply(
	names(spline_data$data), predSpline, spline_data)

spline_data_z$predicted <- lapply(
	names(spline_data$data), predSpline, spline_data_z)


## ---- Binary -----------------------------------------------------------------

# Here we use the procedure outlined here: 
#
# https://www.cmm.bris.ac.uk/lemma/mod/lesson/view.php?id=594&pageid=970
#
# to simulate different values of the random effects and add these onto the
# predicted probabilities. The issue is that unlike with linear regression we
# can't set the value of the random effect to be the mean as this will be 
# skewed given that we are working in the logit distribution.

predBin <- function(x, list_name){

# First we make a new dataframe with values we want to predict at. For the
# age variable we generate a sequence from min to max age values. We create 
# seven versons of a dataframe, each with 1000 values for each age point, and
# each of the five values of mat_ed, plus 0 and 1 (we can use these values) to
# calcaulate SII.

max_age <- round(max(bin_data$data[[x]]$age_cen, na.rm = TRUE), 0)

pred <- data.frame(age_cen = rep(rep(0 : max_age, each = 1000), 7))

age_points <- nrow(pred) / 7

pred$mat_ed_rank <- rep(
	rep(c(0, 0.101, 0.2515, 0.474, 0.759, 0.9355, 1), 
		each = age_points))

pred$dawba_adhd <- rep(1, age_points)

pred$prob_median_log <- predict(bin_data$fit[[x]], newdata = pred)
              
# Now we simulate one set of values of u that we use for each combination of
# age and the levels of mat_ed. This reduces the noise that we get if we
# use a different set of simulated values of u at each age point.
pred$u <- rnorm(1000, mean = 0, sd = sqrt(bin_data$fit[[x]]@RP[1]))

# Add this value to the estimate excluding the random effect
pred$prob_mean <- inv.logit(pred$prob_median_log + pred$u)

# Now we aggregate across the simulated data
pred_agg <- aggregate(pred$prob_mean, 
	               by = list(pred$mat_ed_rank, pred$age_cen), 
	               FUN = mean)

colnames(pred_agg) <- c("mat_ed_rank", "age_cen", "predicted")

pred_agg$mat_ed_rank <- factor(pred_agg$mat_ed_rank, 
	labels = c("Min", "CSE/none", "Vocational", "O-level", "A-level", 
		       "Degree", "Max"))

return(pred_agg)}

plot(pred_agg$age_cen, pred_agg$predicted)



bin_data$predicted <- lapply(
	names(bin_data$data), predBin, bin_data)

names(spline_data$predicted) <- spline_names
names(spline_data_z$predicted) <- spline_names
names(bin_data$predicted) <- bin_names

################################################################################
# 6. Occasion average
################################################################################

## ---- Continuous data --------------------------------------------------------
occSpline <- function(x, list_name){

av <- data.frame(matrix(NA, 
	nrow = length(levels(as.factor(list_name$data[[x]]$occasion))), 
	ncol = 2))

colnames(av) <- c("occasion_num", "mean")

av$occasion_num <- tapply(list_name$data[[x]]$age_cen, 
                          list_name$data[[x]]$occasion, 
	                      mean, na.rm = TRUE)

av$mean <- tapply(list_name$data[[x]][[x]], list_name$data[[x]]$occasion, 
	              mean, na.rm = TRUE)

return(av)
}


spline_data$occ_av <- lapply(
	names(spline_data$data), occSpline, spline_data)

spline_data_z$occ_av <- lapply(
	names(spline_data$data), occSpline, spline_data_z)


## ---- Binary data ------------------------------------------------------------
bin_data$occ_av <- lapply(names(bin_data$data), function(x){

tab <- data.frame(matrix(NA, 
	nrow = length(levels(as.factor(bin_data$data[[x]]$occasion))), 
	ncol = 2))

colnames(tab) <- c("occasion_num", "proportion")

prop <- table(bin_data$data[[x]]$occasion, bin_data$data[[x]][[x]])

tab$occasion_num <- tapply(bin_data$data[[x]]$age_cen, 
	                        bin_data$data[[x]]$occasion, 
	                        mean, na.rm = TRUE)

tab$proportion <- prop[, 2] / (prop[, 1] + prop[, 2])

return(tab)

})

names(spline_data$occ_av) <- spline_names
names(spline_data_z$occ_av) <- spline_names
names(bin_data$occ_av) <- bin_names


################################################################################
# 7. Slope Index Inequality  
################################################################################

## ---- Spline models ----------------------------------------------------------
siiSpline <- function(x, list_name){

sii <- data.frame(matrix(NA, nrow = length(list_name$knots_cen[[x]]) + 2, 
	                         ncol = 2))

colnames(sii) <- c("age", "sii")

coefs <- list_name$fit[[x]]@FP[grep("mat_ed_rank", 
	names(list_name$fit[[x]]@FP))]

sii[1, 1] <- head(list_name$occ_av[[x]]$occasion_num, 1)
sii[2 : (length(list_name$knots_cen[[x]]) + 1), 1] <- list_name$knots_cen[[x]]
sii[nrow(sii), 1] <- tail(list_name$occ_av[[x]]$occasion_num, 1)
sii[1, 2] <- coefs[1]

for(i in 2 : nrow(sii)){

sii[i, 2] <- sii[i - 1, 2] + (sii[i, 1] - sii[i - 1, 1]) * coefs[i]

}

return(sii)
}


spline_data$sii <- lapply(names(spline_data$data), siiSpline, spline_data)
spline_data_z$sii <- lapply(names(spline_data$data), siiSpline, spline_data_z)

## ---- Binomial models --------------------------------------------------------

## Calculations based on this file:
## http://www.equidade.org/files/siilogit.ado
##
## SII at baseline calculated as inverse logit (intercept + mat_ed coefficient)
## - inverse logit (intercept)
##
## SII at last time point calculated as inverse logit (intercept + mat_ed
## coefficient + age_coefficient * age_gap + ageXmat_ed coefficient * age_gap) -
## inverse logit (intercept + age_coefficient * age_gap)

bin_data$sii <- lapply(names(bin_data$data), function(x){

sii <- data.frame(matrix(NA, nrow = 2, ncol = 2))

colnames(sii) <- c("age", "sii")

coefs <- bin_data$fit[[x]]@FP[grep("mat_ed_rank", names(bin_data$fit[[x]]@FP))]

intercept <- bin_data$fit[[x]]@FP[1]

age <- bin_data$fit[[x]]@FP[grep("FP_age_cen", names(bin_data$fit[[x]]@FP))]

sii[1, 1] <- head(bin_data$occ_av[[x]]$occasion_num, 1)
sii[2, 1] <- tail(bin_data$occ_av[[x]]$occasion_num, 1)
sii[1, 2] <- exp(intercept + coefs[1]) / (1 + exp(intercept + coefs[1])) - 
                 exp(intercept) / (1 + exp(intercept))

age_dif <- sii[2, 1] - sii[1, 1]

sii[2, 2] <- exp(intercept + coefs[1] + (age * age_dif) + (coefs[2] * age_dif)) / 
	         (1 + exp(intercept + coefs[1] + (age * age_dif) + (coefs[2] * age_dif))) -
             exp(intercept + (age * age_dif)) / (1 + exp(intercept + (age * age_dif)))

return(sii)
})

names(spline_data$sii) <- spline_names
names(spline_data_z$sii) <- spline_names
names(bin_data$sii) <- bin_names

################################################################################
# 8. Save output  
################################################################################
save(spline_data, file = paste0(data_path, "spline_out.RData"))
save(bin_data, file = paste0(data_path, "bin_out.RData"))