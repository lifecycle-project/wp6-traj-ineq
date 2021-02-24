################################################################################
## Project: mh-ineq
## Script purpose: Plot graphs
## Date: 8th October 2019
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
library(grid)
library(gridExtra)

################################################################################
# 1. Get data  
################################################################################

## ---- Functions --------------------------------------------------------------
source("./mh-ineq/code/mh-ineq-draft-functions.R")
source("./mh-ineq/code/mh-ineq-draft-theme_traj.R")

## ---- Data -------------------------------------------------------------------
data_path <- "z:/projects/ieu2/p6/069/working/data/"

load(paste0(data_path, "spline_out.RData"))
load(paste0(data_path, "bin_out.RData"))


################################################################################
# 2. Trajectory plots  
################################################################################
traj_plots <- list()

traj_plots$mfq <- ggFigA(
  data = spline_data$data$mfq, 
  fit = spline_data$fit$mfq,
  pred = spline_data$predicted$mfq, 
  outcome = "mfq", 
  title = "MFQ Depressive symptoms", 
  ylabel = "Score", 
  xlimits = c(0, 26), 
  ylimits = c(0, 12), 
  centerVal = spline_data$centre_val$mfq)

traj_plots$sdq_emo <- ggFigA(
  data = spline_data$data$sdq_emo, 
  fit = spline_data$fit$sdq_emo,
  pred = spline_data$predicted$sdq_emo, 
  outcome = "sdq_emo", 
  title = "SDQ Emotional Scale", 
  ylabel = "Score", 
  xlimits = c(0, 26), 
  ylimits = c(0, 4), 
  centerVal = spline_data$centre_val$sdq_emo)

traj_plots$sdq_peer <- ggFigA(
  data = spline_data$data$sdq_peer, 
  fit = spline_data$fit$sdq_peer,
  pred = spline_data$predicted$sdq_peer, 
  outcome = "sdq_peer", 
  title = "SDQ Peer Scale", 
  ylabel = "Score", 
  xlimits = c(4, 18), 
  ylimits = c(0, 4), 
  centerVal = spline_data$centre_val$sdq_peer)

traj_plots$sdq_con <- ggFigA(
  data = spline_data$data$sdq_con, 
  fit = spline_data$fit$sdq_con,
  pred = spline_data$predicted$sdq_con, 
  outcome = "sdq_con", 
  title = "SDQ Conduct Scale", 
  ylabel = "Score", 
  xlimits = c(4, 18), 
  ylimits = c(0, 4), 
  centerVal = spline_data$centre_val$sdq_con)

traj_plots$sdq_hyp <- ggFigA(
  data = spline_data$data$sdq_hyp, 
  fit = spline_data$fit$sdq_hyp,
  pred = spline_data$predicted$sdq_hyp, 
  outcome = "sdq_hyp", 
  title = "SDQ Hyperactivity Scale", 
  ylabel = "Score", 
  xlimits = c(4, 18), 
  ylimits = c(0, 6), 
  centerVal = spline_data$centre_val$sdq_hyp)

traj_plots$epds_mat <- ggFigA(
  data = spline_data$data$epds_mat, 
  fit = spline_data$fit$epds_mat,
  pred = spline_data$predicted$epds_mat, 
  outcome = "epds_mat", 
  title = "Maternal EPDS Depressive Symptoms", 
  ylabel = "Score", 
  xlimits = c(0, 14), 
  ylimits = c(0, 10), 
  centerVal = spline_data$centre_val$epds_mat)

traj_plots$epds_pat <- ggFigA(
  data = spline_data$data$epds_pat, 
  fit = spline_data$fit$epds_pat,
  pred = spline_data$predicted$epds_pat, 
  outcome = "epds_pat", 
  title = "Paternal EPDS Depressive Symptoms", 
  ylabel = "Score", 
  xlimits = c(0, 14), 
  ylimits = c(0, 10), 
  centerVal = spline_data$centre_val$epds_pat)

traj_plots$asc <- ggFigA(
  data = spline_data$data$asc, 
  fit = spline_data$fit$asc,
  pred = spline_data$predicted$asc, 
  outcome = "asc", 
  title = "ASC Symptoms", 
  ylabel = "Score", 
  xlimits = c(6, 16), 
  ylimits = c(0, 10), 
  centerVal = spline_data$centre_val$asc)

head(spline_data$data$asc)

traj_plots$dawba_adhd <- ggFigC(
  fit = bin_data$fit$dawba_adhd,
  pred = bin_data$predicted$dawba_adhd %>%
    filter(mat_ed_rank!= "Min" & mat_ed_rank != "Max"), 
  outcome = "dawba_adhd", 
  title = "DAWBA ADHD", 
  ylabel = "Probability", 
  xlimits = c(7, 16), 
  ylimits = c(0, 0.10), 
  centerVal = bin_data$centre_val$dawba_adhd)

traj_plots$dawba_con <- ggFigC(
  fit = bin_data$fit$dawba_con,
  pred = bin_data$predicted$dawba_con %>%
    filter(mat_ed_rank!= "Min" & mat_ed_rank != "Max"), 
  outcome = "dawba_con", 
  title = "DAWBA Conduct", 
  ylabel = "Probability", 
  xlimits = c(7, 16), 
  ylimits = c(0, 0.10), 
  centerVal = bin_data$centre_val$dawba_con)

traj_plots$dawba_odd <- ggFigC(
  fit = bin_data$fit$dawba_odd,
  pred = bin_data$predicted$dawba_odd %>%
    filter(mat_ed_rank!= "Min" & mat_ed_rank != "Max"), 
  outcome = "dawba_odd", 
  title = "DAWBA ODD", 
  ylabel = "Probability", 
  xlimits = c(7, 16), 
  ylimits = c(0, 0.10), 
  centerVal = bin_data$centre_val$dawba_odd)

traj_plots$dawba_dep <- ggFigC(
  fit = bin_data$fit$dawba_dep, 
  pred = bin_data$predicted$dawba_dep %>%
    filter(mat_ed_rank!= "Min" & mat_ed_rank != "Max"), 
  outcome = "dawba_con", 
  title = "DAWBA Depression", 
  ylabel = "Probability", 
  xlimits = c(0, 26), 
  ylimits = c(0, 0.1), 
  centerVal = bin_data$centre_val$dawba_dep)

traj_plots$dawba_anx <- ggFigC(
  fit = bin_data$fit$dawba_anx,
  pred = bin_data$predicted$dawba_anx %>%
    filter(mat_ed_rank!= "Min" & mat_ed_rank != "Max"), 
  outcome = "dawba_anx", 
  title = "DAWBA Anxiety", 
  ylabel = "Probability", 
  xlimits = c(0, 26), 
  ylimits = c(0, 0.1), 
  centerVal = bin_data$centre_val$dawba_anx)

traj_plots$pliks <- ggFigC(
  fit = bin_data$fit$pliks,
  pred = bin_data$predicted$pliks %>%
    filter(mat_ed_rank!= "Min" & mat_ed_rank != "Max"), 
  outcome = "pliks", 
  title = "PLIKS", 
  ylabel = "Probability", 
  xlimits = c(10, 28), 
  ylimits = c(0, 0.5), 
  centerVal = bin_data$centre_val$pliks)


################################################################################
# 3. SII plots  
################################################################################
sii_plots <- list()

sii_range <- c(-1, 1)

sii_plots$mfq <- ggFigB(
  sii_tab = spline_data_z$sii$mfq, 
  xlimits = c(0, 26), 
  ylimits = sii_range, 
  centerVal = spline_data_z$centre_val$mfq)

sii_plots$sdq_emo <- ggFigB(
  sii_tab = spline_data_z$sii$sdq_emo, 
  xlimits = c(0, 26), 
  ylimits = sii_range, 
  centerVal = spline_data_z$centre_val$sdq_emo)

sii_plots$sdq_peer <- ggFigB(
  sii_tab = spline_data_z$sii$sdq_peer, 
  xlimits = c(0, 18), 
  ylimits = sii_range, 
  centerVal = spline_data_z$centre_val$sdq_peer)

sii_plots$sdq_con <- ggFigB(
  sii_tab = spline_data_z$sii$sdq_con, 
  xlimits = c(0, 18), 
  ylimits = sii_range, 
  centerVal = spline_data_z$centre_val$sdq_con)

sii_plots$sdq_hyp <- ggFigB(
  sii_tab = spline_data_z$sii$sdq_hyp, 
  xlimits = c(0, 18), 
  ylimits = sii_range, 
  centerVal = spline_data_z$centre_val$sdq_hyp)

sii_plots$epds_mat <- ggFigB(
  sii_tab = spline_data_z$sii$epds_mat, 
  xlimits = c(0, 14), 
  ylimits = sii_range, 
  centerVal = spline_data_z$centre_val$epds_mat)

sii_plots$epds_pat <- ggFigB(
  sii_tab = spline_data_z$sii$epds_pat, 
  xlimits = c(0, 14), 
  ylimits = sii_range, 
  centerVal = spline_data_z$centre_val$epds_pat)

sii_plots$dawba_adhd <- ggFigB(
  sii_tab = bin_data$sii$dawba_adhd, 
  xlimits = c(6, 18), 
  ylimits = c(-0.1, 0.1), 
  centerVal = bin_data$centre_val$dawba_adhd)

sii_plots$dawba_con <- ggFigB(
  sii_tab = bin_data$sii$dawba_con, 
  xlimits = c(6, 18), 
  ylimits = sii_range, 
  centerVal = bin_data$centre_val$dawba_con)

sii_plots$dawba_odd <- ggFigB(
  sii_tab = bin_data$sii$dawba_odd, 
  xlimits = c(6, 18), 
  ylimits = sii_range, 
  centerVal = bin_data$centre_val$dawba_odd)

sii_plots$dawba_dep <- ggFigB(
  sii_tab = bin_data$sii$dawba_dep, 
  xlimits = c(0, 26), 
  ylimits = c(-0.1, 0.1), 
  centerVal = bin_data$centre_val$dawba_dep)

sii_plots$dawba_anx <- ggFigB(
  sii_tab = bin_data$sii$dawba_anx, 
  xlimits = c(0, 26), 
  ylimits = c(-0.1, 0.1), 
  centerVal = bin_data$centre_val$dawba_anx)

sii_plots$pliks <- ggFigB(
  sii_tab = bin_data$sii$pliks, 
  xlimits = c(8, 28), 
  ylimits = sii_range, 
  centerVal = bin_data$centre_val$pliks)

sii_plots$asc <- ggFigB(
  sii_tab = spline_data$sii$asc, 
  xlimits = c(6, 26), 
  ylimits = sii_range, 
  centerVal = spline_data$centre_val$asc)



################################################################################
# 4. Combine into plots  
################################################################################

## ---- Internalising ----------------------------------------------------------
blank <- rectGrob(gp = gpar(col="white"))
                             
int_grob <- arrangeGrob(
  grobs = list(traj_plots$mfq, blank, traj_plots$sdq_emo,
               sii_plots$mfq, blank, sii_plots$sdq_emo, 
               blank, blank, blank,
               traj_plots$dawba_dep, blank, traj_plots$dawba_anx,
               sii_plots$dawba_dep, blank, sii_plots$dawba_anx), 
  ncol = 3, 
  nrow = 5,
  widths = c(1, 0.05, 1), 
  heights = c(1, 1, 0.13, 1, 1))

ggsave(filename = "./mh-ineq/figures/int_comb.png",
       plot = int_grob,
       dpi = 300,
       type="cairo-png", 
       h = 12, 
       w = 15.92)

## Add peer?

## ---- Externalising ----------------------------------------------------------
ext_grob <- arrangeGrob(
  grobs = list(traj_plots$sdq_con, blank, traj_plots$dawba_con, 
               sii_plots$sdq_con, blank, sii_plots$dawba_con)
               blank, blank, blank, 
               traj_plots$dawba_odd, blank, blank,
               sii_plots$dawba_odd, blank, blank, 
  ncol = 3, 
  nrow = 5,
  widths = c(1, 0.05, 1), 
  heights = c(1, 1, 0.13, 1, 1))
  

## ---- Neurodevelopmental -----------------------------------------------------
"sdq_hyp"  "dawba_adhd", "asc"


## ---- Psychosis --------------------------------------------------------------
"pliks"


## ---- Parental ---------------------------------------------------------------
"epds_mat" "epds_pat"







summary(bin_data$fit[[x]])

https://journals.lww.com/epidem/fulltext/2015/07000/Relative_Index_of_Inequality_and_Slope_Index_of.12.aspx






lay <- rbind(c(1,1,2,2),
             c(1,1,2,2),
             c(3,3,4,4))
             

mfq_grob <- arrangeGrob(
  grobs = list(mfq_fig_a, sdq_emo_fig_a, mfq_fig_b, sdq_emo_fig_b), 
  layout_matrix = lay)

ggsave(filename = "./mh-ineq/figures/mfq.png",
       plot = mfq_grob,
       dpi = 300,
       type="cairo-png")



## ---- SDQ peer ---------------------------------------------------------------

## ---- DAWBA dep --------------------------------------------------------------

## ---- DAWBA anx --------------------------------------------------------------





## ---- Plot graphs ------------------------------------------------------------


################################################################################
# Supplementary materials  
################################################################################

# First we need a plot containing the best fitting polynomial and spline models
# for each outcome.






