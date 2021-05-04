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
# 1. Theme for plot
################################################################################
## ---- Theme ------------------------------------------------------------------
theme_traj <- theme(
  plot.background = element_rect(fill =scales::alpha("#CCCCCC", 0.3)),  #Background outside of plot
  panel.background = element_rect(fill="white"),  #Background for plot
  panel.grid.major=element_line(colour="grey"), #Major and minor gridlines
  panel.grid.minor=element_line(colour="white"), 
  panel.spacing = unit(1, "lines"),
  plot.title = element_text(hjust = 0.5, vjust=0, size=12, face="bold"), #Plot title, thought don't tend to use
  text=element_text(size=9), #General text 
  axis.title.y = element_text(size=14, margin = margin(t = 0, r = 10, b = 0, l = 0)), #Axis labels
  axis.title.x = element_text(size=14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.text.x = element_text(size=11, margin = margin(t = 4, r=0, b=0, l=0), colour="black"), #Axis text
  axis.text.y = element_text(size=11, margin = margin(t = 0, r=4, b=0, l=0), colour="black"),
  axis.ticks.length=unit(0.3, "cm"),
  axis.ticks = element_line(colour = "grey"),
  strip.text.x = element_text(size = 8, face = "bold"),
  strip.background = element_blank(),
  legend.background= element_rect(fill=scales::alpha("#CCCCCC", 0.03)), #Legend background colour
  legend.title=element_text(size=8, face="bold"), #Legend title
  legend.text=element_text(size=8), #Legend text
  legend.position="right", #Legend position
  legend.direction="vertical", #Legend stacking
  legend.justification = "left", #Left justify legend
  legend.key.width = unit(3, "line"), #Make amount of line displayed in legend longer
  legend.margin=margin(t=0.2, r=0, b=0.2, l=0, unit="cm"), #Margin around legend
  plot.margin = unit(c(0.5, 0.5, 0.2, 0.5),"cm"),
  panel.grid.minor.y=element_blank(),
  panel.grid.major.y=element_blank())


ineq_palette <- c("#264653", "#2a9d8f", "#E9C46A", "#F4A261", "#E76F51", "#873156", "#000000")

################################################################################
# 2. Numbers for each cohort  
################################################################################
## ---- Cohort numbers ---------------------------------------------------------
included_n <- paste0("study", c(1:6)) %>%
  map(function(x){
    
    ext_lin.fit$output.summary[[x]]$ngrps    
    
  })

names(included_n) <- coh_lin


################################################################################
# 1. Maternal education distributions 
################################################################################
mat_ed.tab <- mat_ed_stats$categorical %>%
mutate(
  cohort = case_when(
    cohort == "alspac" ~ "ALSPAC",
    cohort == "chop" ~ "CHOP",
    cohort == "dnbc" ~ "DNBC",
    cohort == "inma" ~ "INMA",
    cohort == "moba" ~ "MoBa", 
    cohort == "raine" ~ "Raine")) %>%
  filter(cohort != "combined") 

## ---- Plot -------------------------------------------------------------------
mat.plot <- mat_ed.tab %>%
  ggplot(aes(x = category, y = valid_perc, fill = cohort)) +
  geom_bar(stat = "identity") +
  facet_wrap(~cohort, ncol = 6) +
  theme_traj +
  xlab("Maternal education category (1 = high)") +
  ylab("Percent") +
  theme(legend.position = "none") +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = ineq_palette)
  

## ---- Save plot --------------------------------------------------------------
ggsave(
  filename="./figures/GA21Spring/mat_dist.png", 
  plot = mat.plot,
  h = 12, w = 20, units="cm", dpi=1200,
  device="png")


################################################################################
# 2. Available outcome data  
################################################################################

## ---- Prepare data for plotting ----------------------------------------------
ref <- tibble(
  cohort = coh_lin,
  table = c(rep("ext_pc__derived", 6))
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
  guides(colour = FALSE) +
  scale_colour_manual(values = ineq_palette)

## ---- Save plots --------------------------------------------------------------
ggsave(
  filename="./figures/sample_n.png", 
  plot = n.plot,
  h = 12, w = 30, units="cm", dpi=1200,
  device="png")

################################################################################
# 3. Linear models  
################################################################################

## ---- Prepare data for plotting ----------------------------------------------
lin_labs <- c("ALSPAC", "CHOP", "DNBC", "INMA", "MoBa", "Raine", "Combined")

ext_lin_plot.pred %<>%
  mutate(
    cohort = case_when(
      cohort == "alspac" ~ "ALSPAC",
      cohort == "chop" ~ "CHOP",
      cohort == "dnbc" ~ "DNBC",
      cohort == "inma" ~ "INMA",
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined")) %>%
  mutate(
    cohort = factor(cohort, labels = lin_labs, levels = lin_labs, ordered = TRUE)
  )

int_lin_plot.pred %<>%
  mutate(
    cohort = case_when(
      cohort == "alspac" ~ "ALSPAC",
      cohort == "chop" ~ "CHOP",
      cohort == "dnbc" ~ "DNBC",
      cohort == "inma" ~ "INMA",
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined")) %>%
  mutate(
    cohort = factor(cohort, labels = lin_labs, levels = lin_labs, ordered = TRUE)
  )

## ---- Plot externalising model -----------------------------------------------
ext_lin.plot <- ggplot() + 
  geom_line(data = ext_lin_plot.pred, aes(x = age, y = sii, colour = cohort), size = 0.8) +
  geom_ribbon(data = ext_lin_plot.pred, aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~cohort, ncol = 2) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-20, 40), breaks = seq(-20, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality") +
  scale_colour_manual(values = ineq_palette)

## ---- Plot internalising model -----------------------------------------------
int_lin.plot <- ggplot() + 
  geom_line(data = int_lin_plot.pred, aes(x = age, y = sii, colour = cohort), size = 0.8) +
  geom_ribbon(data = int_lin_plot.pred, aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~cohort, ncol = 2) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-20, 40), breaks = seq(-20, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality") +
  scale_colour_manual(values = ineq_palette)

## ---- Save plots -------------------------------------------------------------
ggsave(
  filename="./figures/GA21Spring/ext_lin.png", 
  plot = ext_lin.plot,
  h = 12, w = 20, units="cm", dpi=1000,
  device="png")

ggsave(
  filename="./figures/GA21Spring/int_lin.png", 
  plot = int_lin.plot,
  h = 12, w = 20, units="cm", dpi=1200,
  device="png")

################################################################################
# 4. Non-linear models 
################################################################################

## ---- Prepare data for plotting ----------------------------------------------
nl_labs <- c("ALSPAC", "CHOP", "MoBa", "Raine", "Combined")

ext_nl_plot.pred %<>%
  mutate(
    cohort = case_when(
      cohort == "alspac" ~ "ALSPAC",
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
      cohort == "alspac" ~ "ALSPAC",
      cohort == "chop" ~ "CHOP",
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined")) %>%
  mutate(
    cohort = factor(cohort, labels = nl_labs, levels = nl_labs, ordered = TRUE)
  )

## ---- Plot externalising model -----------------------------------------------
ext_nl.plot <- ggplot() + 
  geom_line(data = ext_nl_plot.pred, aes(x = age, y = sii, colour = cohort), size = 0.8) +
  geom_ribbon(data = ext_nl_plot.pred,  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~cohort, ncol = 2) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-20, 40), breaks = seq(-20, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality") +
  scale_colour_manual(values = ineq_palette[c(1:2, 5:7)])


## ---- Plot internalising model -----------------------------------------------
int_nl.plot <- ggplot() + 
  geom_line(data = int_nl_plot.pred, aes(x = age, y = sii, colour = cohort), size = 0.8) +
  geom_ribbon(data = int_nl_plot.pred,  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.1) +
  facet_wrap(~cohort, ncol = 2) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-20, 40), breaks = seq(-20, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality") +
  scale_colour_manual(values = ineq_palette[c(1:2, 5:7)])

## ---- Save plots -------------------------------------------------------------
ggsave(
  filename="./figures/GA21Spring/ext_nl.png", 
  plot = ext_nl.plot,
  h = 12, w = 20, units="cm", dpi=1200,
  device="png")

ggsave(
  filename="./figures/GA21Spring/int_nl.png", 
  plot = int_nl.plot,
  h = 12, w = 20, units="cm", dpi=1200,
  device="png")



