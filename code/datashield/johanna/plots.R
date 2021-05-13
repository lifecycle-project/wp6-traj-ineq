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
theme_jn <- theme(
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
  plot.margin = unit(c(0.5, 0.5, 0.2, 0.5),"cm"))


johanna_palette <- c("#264653", "#2a9d8f", "#E9C46A", "#F4A261", "#E76F51", "#873156", "#000000")

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
# 3. CBCL
################################################################################

## ---- Prepare data for plotting ----------------------------------------------
cbcl_labs <- c("MoBa", "Raine")

cbcl.plotdata <- cbcl.pred %>%
  mutate(
    cohort = case_when(
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine"),
    sex = factor(sex, labels = c("Male", "Female"), ordered = TRUE), 
    cohort = factor(cohort, labels = cbcl_labs, levels = cbcl_labs, ordered = TRUE)
  )

## ---- Plot -------------------------------------------------------------------
cbcl_joh.plot <- ggplot() + 
  geom_line(data = cbcl.plotdata, aes(x = age, y = predicted, colour = sex), size = 0.8) +
  geom_ribbon(data = cbcl.plotdata %>% filter(sex == "Male"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.2) +
  geom_ribbon(data = cbcl.plotdata %>% filter(sex == "Female"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.2) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 4), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(0, 20), breaks = seq(0, 20, 4), expand = c(0, 0)) +
  theme_jn +
  xlab("Child age") +
  ylab("Predicted CBCL score") +
  labs(colour = "Child sex") + 
  scale_colour_manual(values = ineq_palette[c(1, 5)])

################################################################################
# 4. SDQ  
################################################################################

## ---- Prepare data for plotting ----------------------------------------------
sdq_labs <- c("ALSPAC", "CHOP", "DNBC")

sdq.plotdata <- sdq.pred %>%
  mutate(
    cohort = case_when(
      cohort == "alspac" ~ "ALSPAC",
      cohort == "chop" ~ "CHOP", 
      cohort == "dnbc" ~ "DNBC"),
    sex = factor(sex, labels = c("Male", "Female"), ordered = TRUE), 
    cohort = factor(cohort, labels = sdq_labs, levels = sdq_labs, ordered = TRUE)
  )

## ---- Plot -------------------------------------------------------------------
sdq_joh.plot <- ggplot() + 
  geom_line(data = sdq.plotdata, aes(x = age, y = predicted, colour = sex), size = 0.8) +
  geom_ribbon(data = sdq.plotdata %>% filter(sex == "Male"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.2) +
  geom_ribbon(data = sdq.plotdata %>% filter(sex == "Female"),  aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.2) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 4), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(0, 12), breaks = seq(0, 12, 4), expand = c(0, 0)) +
  theme_jn+
  xlab("Child age") +
  ylab("Predicted SDQ score") +
  labs(colour = "Child sex") + 
  scale_colour_manual(values = ineq_palette[c(1, 5)])


################################################################################
# 5. Save plots  
################################################################################
ggsave(
  filename="./figures/GA21Spring/cbcl_nl.png", 
  plot = cbcl_joh.plot,
  h = 12, w = 16, units="cm", dpi=1200,
  device="png")

ggsave(
  filename="./figures/GA21Spring/sdq_nl.png", 
  plot = sdq_joh.plot,
  h = 12, w = 16, units="cm", dpi=1200,
  device="png")



