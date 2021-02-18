################################################################################
## Project: wp6-traj-ineq 
## Script purpose: Tables for LS conference presentation
## Date: 18th February 2021
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(tidyr)
library(dsHelper)
library(dplyr)
library(ggplot2)

################################################################################
# 1. Maternal education distributions 
################################################################################

## ---- Plot -------------------------------------------------------------------
mat.plot <- mat_ed_stats$categorical %>%
  filter(cohort != "combined") %>%
  ggplot(aes(x = category, y = valid_perc, colour = cohort)) +
  geom_bar(stat = "identity") +
  facet_wrap(~cohort, ncol = 5)

## ---- Save plot --------------------------------------------------------------
ggsave(
  filename="./figures/mat_dist.png", 
  plot = mat.plot,
  h = 15, w = 20, units="cm", dpi=1200,
  device="png")


################################################################################
# 2. Available outcome data  
################################################################################

## ---- Prepare data for plotting ----------------------------------------------
ref <- tibble(
  cohort = ext_pc_coh_lin,
  table = c(rep("ext_pc__derived", 5))
)

ns_for_plot <- ref %>% 
  pmap(function(cohort, table){
    
    dh.getStats(
      df = table, 
      vars = ds.colnames(table, datasources = conns[cohort])[[1]][
        str_detect(ds.colnames(table, datasources = conns[cohort])[[1]], "age")],
      conns = conns[cohort])$continuous %>%
      mutate(outcome = table)
    
  })

ns <- ns_for_plot %>%
  bind_rows %>%
  separate(variable, c("var", "age"), sep = "([.])") %>%
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "dnbc" ~ "DNBC",
      cohort == "inma" ~ "INMA",
      cohort == "moba" ~ "MoBa", 
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined"), 
    cohort = factor(
      cohort, 
      levels = rev(c("CHOP", "DNBC", "INMA", "MoBa", "Raine", "Combined")),
      ordered = TRUE), 
    outcome = case_when(
      outcome == "ext_pc__derived" ~ "Externalising")) %>%
  select(cohort, mean, valid_n, outcome) %>%
  dplyr::rename("N" = valid_n)

## ---- Plot -------------------------------------------------------------------
palette_n <- c("#ff2600", rep("#005690", 10))

n.plot <- ggplot(data = ns, aes(x = mean, y = cohort, size = N, colour = cohort)) +
  geom_point() + 
  xlab("Child age (years)") +
  ylab("Cohort") +
  forest_theme + 
  theme(panel.grid.major.x = element_line(colour="white"),
        panel.grid.minor.x =element_line(colour="white"),
        axis.ticks.x = element_line(colour = "grey"), 
        legend.position = "top") +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 1), expand = c(0, 0)) +
  scale_colour_manual(values = palette_n) +
  guides(colour = FALSE)

## ---- Save plots --------------------------------------------------------------
ggsave(
  filename="./figures/sample_n.png", 
  plot = n.plot,
  h = 18, w = 25, units="cm", dpi=1200,
  device="png")

################################################################################
# 3. Linear models  
################################################################################

## ---- Prepare data for plotting ----------------------------------------------
ext_lin_labs <- c("CHOP", "DNBC", "INMA", "MoBa", "Raine", "Combined")
int_lin_labs <- c("CHOP", "INMA", "MoBa", "Raine", "Combined")

ext_lin_plot.pred %<>%
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "dnbc" ~ "DNBC",
      cohort == "inma" ~ "INMA",
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined")) %>%
  mutate(
    cohort = factor(cohort, labels = ext_lin_labs, levels = ext_lin_labs, ordered = TRUE)
  )

int_lin_plot.pred %<>%
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "inma" ~ "INMA",
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined")) %>%
  mutate(
    cohort = factor(cohort, labels = int_lin_labs, levels = int_lin_labs, ordered = TRUE)
  )

## ---- Plot externalising model -----------------------------------------------
ext_lin.plot <- ggplot() + 
  geom_line(data = ext_lin_plot.pred, aes(x = age, y = sii, colour = cohort)) +
  geom_ribbon(data = ext_lin_plot.pred, aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-20, 40), breaks = seq(-20, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality")

## ---- Plot internalising model -----------------------------------------------
int_lin.plot <- ggplot() + 
  geom_line(data = int_lin_pred_coh, aes(x = age, y = sii, colour = cohort)) +
  geom_ribbon(data = int_lin_pred_coh, aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-20, 40), breaks = seq(-20, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality")

## ---- Save plots -------------------------------------------------------------
ggsave(
  filename="./figures/ext_lin.png", 
  plot = ext_lin.plot,
  h = 18, w = 25, units="cm", dpi=1200,
  device="png")

ggsave(
  filename="./figures/int_lin.png", 
  plot = int_lin.plot,
  h = 18, w = 25, units="cm", dpi=1200,
  device="png")

################################################################################
# 4. Non-linear models 
################################################################################

## ---- Prepare data for plotting ----------------------------------------------
nl_labs <- c("CHOP", "MoBa", "Raine", "Combined")

ext_nl_plot.pred %<>%
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined")) %>%
  mutate(
    cohort = factor(cohort, labels = nl_labs, levels = nl_labs, ordered = TRUE)
  )

int_nl_plot.pred %<>%
  mutate(
    cohort = case_when(
      cohort == "chop" ~ "CHOP",
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined")) %>%
  mutate(
    cohort = factor(cohort, labels = nl_labs, levels = nl_labs, ordered = TRUE)
  )

## ---- Plot externalising model -----------------------------------------------
ext_nl.plot <- ggplot() + 
  geom_line(data = ext_nl_plot.pred, aes(x = age, y = sii, colour = cohort)) +
  geom_ribbon(data = ext_nl_plot.pred,  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-20, 40), breaks = seq(-20, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality")


## ---- Plot internalising model -----------------------------------------------
int_nl.plot <- ggplot() + 
  geom_line(data = int_nl_plot.pred, aes(x = age, y = sii, colour = cohort)) +
  geom_ribbon(data = int_nl_plot.pred,  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-20, 40), breaks = seq(-20, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality")

## ---- Save plots -------------------------------------------------------------
ggsave(
  filename="./figures/ext_nl.png", 
  plot = ext_nl.plot,
  h = 18, w = 25, units="cm", dpi=1200,
  device="png")

ggsave(
  filename="./figures/int_nl.png", 
  plot = int_nl.plot,
  h = 18, w = 25, units="cm", dpi=1200,
  device="png")



