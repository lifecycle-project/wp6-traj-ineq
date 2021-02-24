################################################################################
## Project: mh-ineq
## Script purpose: Spline models of inequality
## Date: 7th October 2019
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

data_path <- "z:/projects/ieu2/p6/069/working/data/"

################################################################################
# 1. Get data  
################################################################################

## ---- Data -------------------------------------------------------------------
load(paste0(data_path, "spline.RData"))
load(paste0(data_path, "bin.RData"))

## ---- Functions --------------------------------------------------------------
source("./mh-ineq/code/mh-ineq-draft-functions.R")
source("./useful-code-r/code/functions/listN.R")


## ---- Theme ------------------------------------------------------------------
source("./mh-ineq/code/mh-ineq-draft-theme_traj.R")

################################################################################
# 2. MFQ spline model  
################################################################################

## ---- First have a look at the pattern ---------------------------------------
mfq_means <- occAv(
	data = spline_data$data$mfq, 
    outcome = "mfq", 
    type = "continuous")

mfq_mean.plot <- ggPlotOccAv(
    data = mfq_means,
    title = "MFQ mean score by measurement occasion",
    xlimits = c(100, 300),
    ylimits = c(0, 10))


## ---- Get extra power we need for Alex's model ------------------------------
spline_data$data$mfq$age_4 <- spline_data$data$mfq$age ^ 4

head(spline_data$data$mfq)


## ---- Fit Alex's polynomial model --------------------------------------------
mfq_alex_poly.fit <- runMLwiN(Formula = mfq ~ 1 + age + age_2 + age_3 + 
                                        age_4 + (1 + age + age_2 + 
                                        age_3 + age_4 | id),
                              estoptions = list(resi.store = TRUE),
                              data = spline_data$data$mfq)
                          

summary(mfq_alex_poly.fit)

## ---- Fit spline model based on Alex's knots ---------------------------------
mfq_knots <- c(12.8, 18.7, 21.9)

mfq_splines <- data.frame(lspline(x = spline_data$data$mfq$age, 
	                              knots = mfq_knots))

colnames(mfq_splines) <- c("s1", "s2", "s3", "s4")

mfq_splines.data <- cbind(spline_data$data$mfq, mfq_splines)

mfq_spline.fit <- runMLwiN(
	Formula = mfq ~ 1 + s1 + s2 + s3 + s4 + 
	          (1 + s1 + s2 + s3 + s4 | id), 
    estoptions = list(resi.store = TRUE),
    data = mfq_splines.data)

summary(mfq_spline.fit)

## ---- Get predicted values ---------------------------------------------------

mfq_poly_preds <- mlmPredTraj(
    fit = mfq_alex_poly.fit,
    data = spline_data$data$mfq)

mfq_spline_preds <- mlmPredTraj(
    fit = mfq_spline.fit,
    data = spline_data$data$mfq)

mfq_spline_preds$age <- mfq_spline_preds$age / 12
mfq_poly_preds$age <- mfq_poly_preds$age / 12
mfq_means$occasion_num <- mfq_means$occasion_num / 12

## ---- Get fit statistics for graph -------------------------------------------
dev_lab <- paste0("Polynomial deviance = ", round(deviance(mfq_alex_poly.fit), 2),
                  "\n",
                  "Spline deviance = ", round(deviance(mfq_spline.fit, 2)))


## ---- Overlay both in a nice plot --------------------------------------------
title <- paste0("Polynomial(s): age + age^2 + age^3 + age^4", "\n",
                "Knot(s): 12.8, 18.7, 21.9")

mfq_overlay <- ggplot() + 
geom_point(data = mfq_means, 
           aes(x = occasion_num, y = Mean), size = 1.2) +
geom_line(data = sample_frac(mfq_poly_preds, 0.10), 
          aes(x = age, y = predicted), size = 1.5, colour = "#197d78", 
              alpha = 0.4) +
geom_line(data = mfq_spline_preds, 
          aes(x = age, y = predicted), size = 0.5) +
theme_traj +
labs(title = title, x = "Age", y = "MFQ score") +
scale_x_continuous(limit = c(8, 26), expand = c(0, 0)) + 
scale_y_continuous(limit = c(0, 12), expand = c(0, 0)) +
annotate("text", x = 6, y = 0.5, label = dev_lab)


################################################################################
# 3. SDQ emotional spline model  
################################################################################

## ---- First have a look at the pattern ---------------------------------------
sdq_emo_means <- occAv(data = spline_data$data$sdq_emo, 
	                   outcome = "sdq_emo", 
	                   type = "continuous")

sdq_emo_mean.plot <- ggPlotOccAv(data = sdq_emo_means,
                                title = "SDQ Emotional Scale: mean score by questionnaire",
	                            xlimits = c(40, 220),
	                            ylimits = c(0, 4))
	                      
ggsave(filename="./mh-ineq/figures/sdq_emo_means.tiff", 
       plot=sdq_emo_mean.plot, 
       h = 20, w = 15.92, units="cm", dpi=80, type="cairo")


## ---- Fit models for each pair of fractional polynomial-----------------------
sdq_emo_poly.fit <- mlmPolyComb(spline_data$data$sdq_emo, "sdq_emo")

sdq_emo_poly.plot <- ggPolyTraj(
	polycomb = sdq_emo_poly.fit, 
    xlimit = c(3, 18), 
    ylimit = c(0, 4), 
    ylabel = "SDQ Emotional scale", 
    lab_x = 6,
    lab_y = 0.5, 
    convert_age = TRUE)

ggMultiPdf(sdq_emo_poly.plot, 
	"./mh-ineq/figures/SDQ Emotional polynomial plots.pdf")


## ---- Fit spline models and overlay on best fitting polynomial model ---------
sdq_emo_spline.fit <- mlmCompSpline(data = spline_data$data$sdq_emo, 
	                                outcome = "sdq_emo", 
	                                knots_1 = c(84, 85, 86))
	          
sdq_emo_overlay.plot <- ggSplinesTraj(splinescomb = sdq_emo_spline.fit, 
	                                  polycomb = sdq_emo_poly.fit, 
	                                  xlimit = c(3, 18), 
	                                  ylimit = c(0, 4), 
	                                  ylabel = "SDQ Emotional", 
	                                  lab_x = 6,
                                      lab_y = 0.5, 
                                      convert_age = TRUE)

ggMultiPdf(sdq_emo_overlay.plot, 
	"./mh-ineq/figures/SDQ emotional overlaid plots.pdf")


################################################################################
# 4. SDQ peer spline model  
################################################################################

## ---- First have a look at the pattern ---------------------------------------
sdq_peer_means <- occAv(data = spline_data$data$sdq_peer, 
	                      outcome = "sdq_peer",
	                      type = "continuous")

sdq_peer_mean.plot <- ggPlotOccAv(data = sdq_peer_means,
                                  title = "SDQ Peer Scale: mean score by questionnaire",
	                              xlimits = c(40, 220),
	                              ylimits = c(0, 4))
	                      
ggsave(filename="./mh-ineq/figures/sdq_peer_means.tiff", 
       plot=sdq_peer_mean.plot, 
       h = 20, w = 15.92, units="cm", dpi=80, type="cairo")


## ---- Fit models for each pair of fractional polynomial-----------------------
sdq_peer_poly.fit <- mlmPolyComb(spline_data$data$sdq_peer, "sdq_peer")

sdq_peer_poly.plot <- ggPolyTraj(
	polycomb = sdq_peer_poly.fit, 
    xlimit = c(3, 18), 
    ylimit = c(0, 4), 
    lab_x = 6,
    lab_y = 0.5,
    ylabel = "SDQ Peer scale", 
    convert_age = TRUE)

ggMultiPdf(sdq_peer_poly.plot, 
	"./mh-ineq/figures/SDQ Peer polynomial plots.pdf")


## ---- Fit spline models and overlay on best fitting polynomial model ---------
sdq_peer_spline.fit <- mlmCompSpline(data = spline_data$data$sdq_peer, 
	                                outcome = "sdq_peer", 
	                                knots_1 = c(60, 61, 62, 63))
	                                	          
sdq_peer_overlay.plot <- ggSplinesTraj(splinescomb = sdq_peer_spline.fit, 
	                                  polycomb = sdq_peer_poly.fit, 
	                                  xlimit = c(3, 18), 
	                                  ylimit = c(0, 4), 
	                                  lab_x = 6,
                                      lab_y = 0.5,
	                                  ylabel = "SDQ Peer",
	                                  convert_age = TRUE)

ggMultiPdf(sdq_peer_overlay.plot, 
	"./mh-ineq/figures/SDQ peer overlaid plots.pdf")

################################################################################
# 5. SDQ conduct spline model  
################################################################################

## ---- First have a look at the pattern ---------------------------------------
sdq_con_means <- occAv(data = spline_data$data$sdq_con, 
	                     outcome = "sdq_con", 
	                     type = "continuous")

sdq_con_mean.plot <- ggPlotOccAv(data = sdq_con_means,
                                title = "SDQ Conduct Scale: mean score by questionnaire",
	                            xlimits = c(40, 220),
	                            ylimits = c(0, 4))
	                      
ggsave(filename="./mh-ineq/figures/sdq_con_means.tiff", 
       plot=sdq_con_mean.plot, 
       h = 20, w = 15.92, units="cm", dpi=80, type="cairo")


## ---- Fit models for each pair of fractional polynomial-----------------------
sdq_con_poly.fit <- mlmPolyComb(spline_data$data$sdq_con, "sdq_con")

sdq_con_poly.plot <- ggPolyTraj(
	polycomb = sdq_con_poly.fit, 
    xlimit = c(3, 18), 
    ylimit = c(0, 4), 
    ylabel = "SDQ Conduct scale",
    lab_x = 6,
    lab_y = 0.5,
    convert_age = TRUE)

ggMultiPdf(sdq_con_poly.plot, 
	"o:/repos/mh-ineq/figures/SDQ Conduct polynomial plots.pdf")


## ---- Fit spline models and overlay on best fitting polynomial model ---------
sdq_con_spline.fit <- mlmCompSpline(
	data = spline_data$data$sdq_con, 
	outcome = "sdq_con", 
	knots_1 = c(96, 97, 98, 99, 100, 101))
	                                	          
sdq_con_overlay.plot <- ggSplinesTraj(
	splinescomb = sdq_con_spline.fit, 
	polycomb = sdq_con_poly.fit, 
	xlimit = c(3, 18), 
	ylimit = c(0, 4), 
	ylabel = "SDQ Conduct", 
	lab_x = 6,
    lab_y = 0.5,
    convert_age = TRUE)

ggMultiPdf(sdq_con_overlay.plot, 
	"./mh-ineq/figures/SDQ conduct overlaid plots.pdf")


################################################################################
# 6. SDQ hyperactivity spline model  
################################################################################

## ---- First have a look at the pattern ---------------------------------------
sdq_hyp_means <- occAv(
	data = spline_data$data$sdq_hyp, 
	outcome = "sdq_hyp", 
	type = "continuous")

sdq_hyp_mean.plot <- ggPlotOccAv(
	data = sdq_hyp_means,
    title = "SDQ hypderactive Scale: mean score by questionnaire",
    xlimits = c(40, 220),
	ylimits = c(0, 5))
	                      
ggsave(filename="./mh-ineq/figures/sdq_hyp_means.tiff", 
       plot=sdq_hyp_mean.plot, 
       h = 20, w = 15.92, units="cm", dpi=80, type="cairo")


## ---- Fit models for each pair of fractional polynomial-----------------------
sdq_hyp_poly.fit <- mlmPolyComb(spline_data$data$sdq_hyp, "sdq_hyp")

sdq_hyp_poly.plot <- ggPolyTraj(
	polycomb = sdq_hyp_poly.fit, 
    xlimit = c(3, 18), 
    ylimit = c(0, 5), 
    ylabel = "SDQ Hyperactivity scale",
    lab_x = 6,
    lab_y = 0.5,
    convert_age = TRUE)

ggMultiPdf(sdq_hyp_poly.plot, 
	"./mh-ineq/figures/SDQ Hyperactivity polynomial plots.pdf")


## ---- Fit spline models and overlay on best fitting polynomial model ---------
sdq_hyp_spline.fit <- mlmCompSpline(
	data = spline_data$data$sdq_hyp, 
	outcome = "sdq_hyp", 
	knots_1 = c(84, 85, 86, 87, 88, 89))
	          
sdq_hyp_overlay.plot <- ggSplinesTraj(
    splinescomb = sdq_hyp_spline.fit, 
    polycomb = sdq_hyp_poly.fit, 
    xlimit = c(3, 18), 
    ylimit = c(0, 5), 
    ylabel = "SDQ Hyperactive",
    lab_x = 6,
    lab_y = 0.5,
    convert_age = TRUE)

ggMultiPdf(sdq_hyp_overlay.plot, 
	"./mh-ineq/figures/SDQ Hyperactivity overlaid plots.pdf")


################################################################################
# 7. Maternal EPDS  
################################################################################

## ---- First have a look at the pattern ---------------------------------------
epds_mat_means <- occAv(
	data = spline_data$data$epds_mat, 
    outcome = "epds_mat", 
    type = "continuous")

epds_mat_mean.plot <- ggPlotOccAv(
	data = epds_mat_means,
    title = "Maternal EPDS: mean score by questionnaire",
    xlimits = c(0, 100),
    ylimits = c(2, 8))
	                      
ggsave(filename="./mh-ineq/figures/epds_mat_means.tiff", 
       plot=epds_mat_mean.plot, 
       h = 20, w = 15.92, units="cm", dpi=80, type="cairo")


## ---- Fit models for each pair of fractional polynomial-----------------------
epds_mat_poly.fit <- mlmPolyComb(spline_data$data$epds_mat, "epds_mat")

## Most of these converged, but some didn't
epds_mat_poly.plot <- ggPolyTraj(
	polycomb = epds_mat_poly.fit, 
    xlimit = c(0, 10), 
    ylimit = c(2, 8), 
    ylabel = "Maternal EPDS", 
    convert_age = TRUE, 
    lab_x = 2, 
    lab_y = 3)

ggMultiPdf(epds_mat_poly.plot, 
	"./mh-ineq/figures/Maternal epds polynomial plots.pdf")


## ---- Fit spline models and overlay on best fitting polynomial model ---------
epds_mat_spline.fit <- mlmCompSpline(
    data = spline_data$data$epds_mat, 
    outcome = "epds_mat", 
    knots_1 = c(seq(7, 9, by = 1)),
	knots_2 = c(seq(14, 16, by = 1)),
	knots_3 = c(seq(42, 44, by = 1)), 
	knots_4 = c(seq(111, 113, by = 1)),
	maxit = 50)

epds_mat_spline.plot <- ggSplinesTraj(
	splinescomb = epds_mat_spline.fit, 
    polycomb = epds_mat_poly.fit, 
    xlimit = c(0, 10), 
    ylimit = c(2, 8), 
    ylabel = "epds_mat", 
    convert_age = TRUE, 
    lab_x = 2,
    lab_y = 3)

ggMultiPdf(epds_mat_spline.plot, 
	"./mh-ineq/figures/Maternal EPDS overlaid plots.pdf")


################################################################################
# 8. Paternal EPDS  
################################################################################

## ---- First have a look at the pattern ---------------------------------------
epds_pat_means <- occAv(
	data = spline_data$data$epds_pat, 
    outcome = "epds_pat", 
    type = "continuous")

epds_pat_mean.plot <- ggPlotOccAv(
	data = epds_pat_means,
    title = "Paternal EPDS: mean score by questionnaire",
    xlimits = c(0, 100),
    ylimits = c(2, 8))
	                      
ggsave(filename="./mh-ineq/figures/epds_pat_means.tiff", 
       plot=epds_pat_mean.plot, 
       h = 20, w = 15.92, units="cm", dpi=80, type="cairo")


## ---- Fit models for each pair of fractional polynomial-----------------------
epds_pat_poly.fit <- mlmPolyComb(spline_data$data$epds_pat, "epds_pat")

## Most of these converged, but some didn't
epds_pat_poly.plot <- ggPolyTraj(
	polycomb = epds_pat_poly.fit, 
    xlimit = c(0, 10), 
    ylimit = c(2, 8), 
    ylabel = "Paternal EPDS", 
    convert_age = TRUE, 
    lab_x = 2, 
    lab_y = 3)

ggMultiPdf(epds_pat_poly.plot, 
	"./mh-ineq/figures/paternal epds polynomial plots.pdf")


## ---- Fit spline models and overlay on best fitting polynomial model ---------
epds_pat_spline.fit <- mlmCompSpline(
	data = spline_data$data$epds_pat, 
    outcome = "epds_pat", 
    knots_1 = c(seq(7, 9, by = 1)),
    knots_2 = c(seq(14, 16, by = 1)),
    knots_3 = c(seq(42, 44, by = 1)), 
	knots_4 = c(seq(111, 113, by = 1)), 
	maxit = 50)
	                             
epds_pat_spline.plot <- ggSplinesTraj(
	splinescomb = epds_pat_spline.fit, 
    polycomb = epds_pat_poly.fit, 
    xlimit = c(0, 10), 
    ylimit = c(2, 8), 
    ylabel = "epds_pat", 
    convert_age = TRUE, 
    lab_x = 2,
    lab_y = 3)

ggMultiPdf(epds_pat_spline.plot, 
	"./mh-ineq/figures/Paternal EPDS overlaid plots.pdf")


################################################################################
# 9. DAWBA ADHD  
################################################################################

## ---- Have a look at the pattern ---------------------------------------------
dawba_adhd_prop <- occAv(
	data = bin_data$data$dawba_adhd, 
    outcome = "dawba_adhd", 
    type = "categorical")

ggPlotOccAv(
	data = dawba_adhd_prop, 
    title = "Dawba ADHD proportions", 
    xlimits = c(90, 250),
    ylimits = c(0, 0.05))

# This looks very linear. Let's try just fitting a linear model

dawba_adhd_lin <- runMLwiN(
	Formula = logit(dawba_adhd) ~ 1 + age + (1 | id), 
    estoptions = list(resi.store = TRUE),
    data = arrange(bin_data$data$dawba_adhd, id), 
    D = "Binomial")

dawba_adhd_plot <- ggSplineLin(
	data = bin_data$data$dawba_adhd, 
	fit = dawba_adhd_lin,
	outcome = "dawba_adhd",
	title = "Dawba ADHD Linear model", 
	ylabel = "Dawba ADHD", 
	xlimits = c(50, 250),
    ylimits = c(0, 0.05))

ggsave(filename = "./mh-ineq/figures/adhd_dawba_lin.png",
       plot = dawba_adhd_plot,
       dpi = 300,
       type = "cairo")

################################################################################
# 10. DAWBA Conduct
################################################################################
dawba_con_prop <- occAv(
	data = bin_data$data$dawba_con, 
	outcome = "dawba_con", 
	type = "categorical")

ggPlotOccAv(
	data = dawba_con_prop, 
    title = "Dawba con proportions", 
    xlimits = c(50, 250),
    ylimits = c(0, 0.1))


## ---- Linear model -----------------------------------------------------------
dawba_con_lin <- runMLwiN(
	Formula = logit(dawba_con) ~ 1 + age + (1 | id), 
    estoptions = list(resi.store = TRUE),
    data = arrange(bin_data$data$dawba_con, id), 
    D = "Binomial")

dawba_con_plot <- ggSplineLin(
	data = bin_data$data$dawba_con, 
	fit = dawba_con_lin,
	outcome = "dawba_con",
	title = "Dawba Conduct Linear model", 
	ylabel = "Dawba Conduct", 
	xlimits = c(50, 250),
    ylimits = c(0, 0.05))

ggsave(filename = "./mh-ineq/figures/con_dawba_lin.png",
       plot = dawba_con_plot,
       dpi = 300,
       type = "cairo")

################################################################################
# 11. DAWBA ODD  
################################################################################
dawba_odd_prop <- 
occAv(data = bin_data$data$dawba_odd, 
      outcome = "dawba_odd", 
      type = "categorical")

ggPlotOccAv(
	data = dawba_odd_prop, 
    title = "Dawba odd proportions", 
    xlimits = c(50, 250),
    ylimits = c(0, 0.1))

# This looks very linear. Let's try just fitting a linear model
dawba_odd_lin <- runMLwiN(
	Formula = logit(dawba_odd) ~ 1 + age + (1 | id), 
    estoptions = list(resi.store = TRUE),
    data = arrange(bin_data$data$dawba_odd, id), 
    D = "Binomial")

dawba_odd_plot <- ggSplineLin(
	data = bin_data$data$dawba_odd, 
	fit = dawba_odd_lin,
	outcome = "dawba_odd",
	title = "Dawba ODD Linear model", 
	ylabel = "Dawba ODD", 
	xlimits = c(50, 250),
    ylimits = c(0, 0.05))

ggsave(filename = "./mh-ineq/figures/odd_dawba_lin.png",
       plot = dawba_odd_plot,
       dpi = 300,
       type = "cairo")

################################################################################
# 12. DAWBA depression  
################################################################################
dawba_dep_prop <- occAv(
	data = bin_data$data$dawba_dep, 
    outcome = "dawba_dep", 
    type = "categorical")

ggPlotOccAv(
	data = dawba_dep_prop, 
    title = "Dawba dep proportions", 
    xlimits = c(50, 250),
    ylimits = c(0, 0.1))

dawba_dep_lin <- runMLwiN(
	Formula = logit(dawba_dep) ~ 1 + age + (1 | id), 
    estoptions = list(resi.store = TRUE),
    data = arrange(bin_data$data$dawba_dep, id), 
    D = "Binomial")

dawba_dep_plot <- ggSplineLin(
	data = bin_data$data$dawba_dep, 
	fit = dawba_dep_lin,
	outcome = "dawba_dep",
	title = "Dawba depression Linear model", 
	ylabel = "Dawba dep", 
	xlimits = c(50, 250),
    ylimits = c(0, 0.05))

ggsave(filename = "./mh-ineq/figures/dep_dawba_lin.png",
       plot = dawba_dep_plot,
       dpi = 300,
       type = "cairo")


################################################################################
# 13. DAWBA anxiety  
################################################################################
dawba_anx_prop <- occAv(
	data = bin_data$data$dawba_anx, 
    outcome = "dawba_anx", 
    type = "categorical")

ggPlotOccAv(
	data = dawba_anx_prop, 
    title = "Dawba anx proportions", 
    xlimits = c(50, 250),
    ylimits = c(0, 0.1))


## ---- Linear model -----------------------------------------------------------
dawba_anx_lin <- runMLwiN(
	Formula = logit(dawba_anx) ~ 1 + age + (1 | id), 
    estoptions = list(resi.store = TRUE),
    data = arrange(bin_data$data$dawba_anx, id), 
    D = "Binomial")

dawba_anx_plot <- ggSplineLin(
	data = bin_data$data$dawba_anx, 
	fit = dawba_anx_lin,
	outcome = "dawba_anx",
	title = "Dawba anxiety Linear model", 
	ylabel = "Dawba anx", 
	xlimits = c(50, 250),
    ylimits = c(0, 0.05))

ggsave(filename = "./mh-ineq/figures/dep_anx_lin.png",
       plot = dawba_anx_plot,
       dpi = 300,
       type = "cairo")


################################################################################
# 14. PLIKS  
################################################################################
pliks_prop <- occAv(
	data = bin_data$data$pliks, 
    outcome = "pliks", 
    type = "categorical")

ggPlotOccAv(
	data = pliks_prop, 
    title = "PLIKS proportions", 
    xlimits = c(150, 300),
    ylimits = c(0, 0.3))

pliks_lin <- runMLwiN(
	Formula = logit(pliks) ~ 1 + age + (1 | id), 
    estoptions = list(resi.store = TRUE),
    data = arrange(bin_data$data$pliks, id), 
    D = "Binomial")

pliks_plot <- ggSplineLin(
	data = bin_data$data$pliks, 
	fit = pliks_lin,
	outcome = "pliks",
	title = "PLIKS linear model", 
	ylabel = "PLIKS", 
	xlimits = c(100, 400),
    ylimits = c(0, 0.2))


################################################################################
# 15. SCDC
################################################################################

## ---- First have a look at the pattern ---------------------------------------
asc_means <- occAv(data = spline_data$data$asc, 
                   outcome = "asc", 
                   type = "continuous")

asc_mean.plot <- ggPlotOccAv(data = asc_means,
                             title = "SCDC: mean score by questionnaire",
                             xlimits = c(40, 220),
                             ylimits = c(0, 4))
                          
ggsave(filename="./mh-ineq/figures/asc_means.tiff", 
       plot=asc_mean.plot, 
       h = 20, w = 15.92, units="cm", dpi=80, type="cairo")


## ---- Fit models for each pair of fractional polynomial-----------------------
asc_poly.fit <- mlmPolyComb(spline_data$data$asc, "asc")

asc_poly.plot <- ggPolyTraj(
    polycomb = asc_poly.fit, 
    xlimit = c(3, 18), 
    ylimit = c(0, 4), 
    ylabel = "SCDC score",
    lab_x = 6,
    lab_y = 0.5,
    convert_age = TRUE)

ggMultiPdf(asc_poly.plot, 
    "c:/repos/mh-ineq/figures/asc polynomial plots.pdf")


## ---- Fit spline models and overlay on best fitting polynomial model ---------
asc_spline.fit <- mlmCompSpline(
    data = spline_data$data$asc, 
    outcome = "asc", 
    knots_1 = seq(from = 126, to = 138, by = 1),
    knots_2 = seq(from = 162, to = 174, by = 1))
                                                  
asc_overlay.plot <- ggSplinesTraj(
    splinescomb = asc_spline.fit, 
    polycomb = asc_poly.fit, 
    xlimit = c(3, 18), 
    ylimit = c(0, 4), 
    ylabel = "SDQ Conduct", 
    lab_x = 6,
    lab_y = 0.5,
    convert_age = TRUE)

ggMultiPdf(asc_overlay.plot, 
    "./mh-ineq/figures/ASC overlaid plots.pdf")



################################################################################
# Create list of plots for best fitting models for supplementary  
################################################################################

best_plots <- list(
    mfq_overlay, sdq_emo_overlay.plot[[1]], sdq_peer_overlay.plot[[1]],
    sdq_con_overlay.plot[[1]], sdq_hyp_overlay.plot[[1]], 
    epds_mat_spline.plot[[1]], epds_pat_spline.plot[[1]], dawba_adhd_plot,
    dawba_con_plot, dawba_odd_plot, dawba_dep_plot, dawba_anx_plot, pliks_plot,
    asc_overlay.plot)

