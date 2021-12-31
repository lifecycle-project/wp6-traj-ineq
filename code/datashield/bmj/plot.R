################################################################################
## Project: traj-ineq
## Script purpose: Plots
## Date: 19th October 21
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################
library(here)
library(dsHelper)
source("https://raw.githubusercontent.com/timcadman/useful-code-r/master/code/themes/themes.R")

conns <- datashield.login(logindata, restore = "mhtraj_9")
################################################################################
# 1. Prep  
################################################################################

## ---- Function to get Ns -----------------------------------------------------
dt.lmerN <- function(x){
  paste0("study", seq(1, x$num.valid.studies)) %>%
    map(~x$output.summary[[.]]$ngrps) %>%
    unlist %>%
    sum()
}
      
## ---- ADHD -------------------------------------------------------------------
adhd.pred <- dt.prepIneqPlots(
  model = adhd.fit$models["adhd_age_,adhd_age_m_0_5"], 
  df = "analysis_df", 
  conns = conns[adhd_coh_rev]) %>%
  mutate(outcome = "ADHD")


    
## ---- ASD --------------------------------------------------------------------
asd.pred <- dt.prepIneqPlots(
  model = asd.fit$models["asd_age_m_2,asd_age_m_1"], 
  df = "analysis_df", 
  conns = conns[asd_coh_rev]) %>%
  mutate(outcome = "ASD")

## ---- Language ---------------------------------------------------------------
lan.pred <- dt.prepIneqPlots(
  model = lan.fit$models["lan_age_m_2,lan_age_m_1"], 
  df = "analysis_df", 
  conns = conns[lan_coh_rev]) %>%
  mutate(outcome = "Language")

lan.fit$fit

## ---- NVI --------------------------------------------------------------------
nvi.pred <- dt.prepIneqPlots(
  model = nvi.fit$models["nvi_age_m_2,nvi_age_m_1"], 
  df = "analysis_df", 
  conns = conns[nvi_coh]) %>%
  mutate(outcome = "Non-verbal intelligence")

## ---- Combined ---------------------------------------------------------------
comb.pred <- bind_rows(list(adhd.pred, asd.pred, lan.pred, nvi.pred))


################################################################################
# 2. Plots  
################################################################################
ineq_palette <- c("#264653", "#2a9d8f", "#E9C46A", "#F4A261", "#E76F51", 
                  "#873156", "#000000")
## ---- ADHD -------------------------------------------------------------------
adhd.plot <- ggplot() + 
  geom_line(data = adhd.pred %>% filter(cohort != "combined"), aes(x = age, y = sii_pred, colour = cohort_neat), size = 0.8) +
  geom_ribbon(data = adhd.pred %>% filter(cohort != "combined"),  aes(x = age, ymin = sii_low, ymax = sii_upp), alpha = 0.1) +
  facet_wrap(~cohort_neat, ncol = 2) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-20, 40), breaks = seq(-20, 40, 20), expand = c(0, 0)) +
  theme_std +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality") +
  scale_colour_manual(values = ineq_palette)

ggsave(
  filename = here("figures/GA21autumn", "adhd.png"), 
  plot = adhd.plot,
  h = 10, w = 20, units="cm", dpi=1200,
  device="png")


## ---- ASC --------------------------------------------------------------------
asd.plot <- ggplot() + 
  geom_line(data = asd.pred %>% dplyr::filter(cohort != "combined"), aes(x = age, y = sii_pred, colour = cohort), size = 0.8) +
  geom_ribbon(data = asd.pred %>% dplyr::filter(cohort != "combined"),  aes(x = age, ymin = sii_low, ymax = sii_upp), alpha = 0.1) +
  scale_x_continuous(limit = c(0, 10), breaks = seq(0, 10, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-10, 40), breaks = seq(-10, 50, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality") +
  scale_colour_manual(values = ineq_palette)


## ---- Language -------------------------------------------------------------------
lan.plot <- ggplot() + 
  geom_line(data = lan.pred %>% dplyr::filter(cohort != "combined"), aes(x = age, y = sii_pred, colour = cohort), size = 0.8) +
  geom_ribbon(data = lan.pred %>% dplyr::filter(cohort != "combined"),  aes(x = age, ymin = sii_low, ymax = sii_upp), alpha = 0.1) +
  facet_wrap(~cohort, ncol = 2) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 40), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality")


## ---- Non-verbal intelligence ------------------------------------------------
nvi.plot <- ggplot() + 
  geom_line(data = nvi.pred %>% dplyr::filter(cohort != "combined"), aes(x = age, y = sii_pred, colour = cohort_neat), size = 0.8) +
  geom_ribbon(data = nvi.pred %>% dplyr::filter(cohort != "combined"),  aes(x = age, ymin = sii_low, ymax = sii_upp), alpha = 0.1) +
  facet_wrap(~outcome, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 40), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality") +
  scale_colour_manual(values = ineq_palette)

ggsave(
  filename = here("figures/GA21autumn", "nvi.png"), 
  plot = nvi.plot,
  h = 12, w = 20, units="cm", dpi=1200,
  device="png")


## ---- Combined ---------------------------------------------------------------
comb.plot <- ggplot() + 
  geom_line(
    data = comb.pred %>% dplyr::filter(cohort == "combined" & outcome != "Language"), 
    aes(x = age, y = sii_pred, colour = outcome), size = 0.8) +
  geom_ribbon(
    data = comb.pred %>% dplyr::filter(cohort == "combined" & outcome != "Language"), 
    aes(x = age, ymin = sii_low, ymax = sii_upp), alpha = 0.1) +
  facet_wrap(~outcome, ncol = 1) +
  scale_x_continuous(limit = c(0, 12), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 40), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  xlab("Child age") +
  ylab("Slope Index of Inequality") +
  scale_colour_manual(values = ineq_palette)

ggsave(
  filename = here("figures/GA21autumn", "comb.png"), 
  plot = comb.plot,
  h = 12, w = 16, units="cm", dpi=1200,
  device="png")

dt.lmerN(adhd.fit$models[["adhd_age_,adhd_age_m_0_5"]])
dt.lmerN(asd.fit$models[["asd_age_m_2,asd_age_m_1"]])
dt.lmerN(nvi.fit$models[["nvi_age_m_2,nvi_age_m_1"]])


## ---- NVI --------------------------------------------------------------------

  model = , 

pred_data_2_trim <- pred_data_2_trim %>%
  mutate(sex = factor(sex1, levels = c(0, 1), labels = c("Male", "Female")))

adhd_coh <- adhd_coh_any[!adhd_coh_any %in% c("genr")]

################################################################################
# 2. ASC  
################################################################################
avail_asd <- dh.getStats(
  df = "analysis_df",
  vars = c("asd_pc_", "asd_raw_", "asd_instr_"),
  conns = conns[names(conns) != "dnbc"]
)

asd_coh_any <- avail_asd$continuous %>% cohAvail("asd_pc_")

asd_scatter <- ds.scatterPlot(
  x = "analysis_df$asd_age_", 
  y = "analysis_df$asd_pc_", 
  datasources = conns[asd_coh_any])

asd_coh <- c("elfe", "inma", "moba")

################################################################################
# 3. Language  
################################################################################
avail_lan <- dh.getStats(
  df = "analysis_df",
  vars = c("lan_pc_", "lan_raw_", "lan_instr_"), 
  conns = conns[names(conns) != "dnbc"]
)

lan_coh_any <- avail_lan$continuous %>% cohAvail("lan_pc_")

lan_scatter <- ds.scatterPlot(
  x = "analysis_df$lan_age_", 
  y = "analysis_df$lan_pc_", 
  datasources = conns[lan_coh_any])

lan_coh <- c("alspac", "inma", "rhea")


################################################################################
# 4. NVI  
################################################################################
avail_nvi <- dh.getStats(
  df = "analysis_df",
  vars = c("nvi_pc_", "nvi_raw_", "nvi_instr_"), 
  conns = conns[names(conns) != "dnbc"]
)

nvi_coh_any <- avail_nvi$continuous %>% cohAvail("nvi_pc_")

nvi_scatter <- ds.scatterPlot(
  x = "analysis_df$nvi_age_", 
  y = "analysis_df$nvi_pc_", 
  datasources = conns[nvi_coh_any])

nvi_coh <- c("alspac", "inma", "rhea")
