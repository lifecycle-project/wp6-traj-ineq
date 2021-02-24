

ext_pc.pred %<>% mutate(se = ext_se_vec)

dh.metaMSii <- function(fit){
  tmp <- fit %>% 
    group_by(age) %>% 
    group_map(
      
      ~rma(
        yi = .x$sii,
        sei = .x$se, 
        method = "ML")
    )
  
  combined <- tibble(
    sii = tmp %>% map_dbl(function(x){x[["beta"]]}),
    se = tmp %>% map_dbl(function(x){x[["se"]]}),
    cohort = "combined", 
    age = seq(1, 10, by = 0.1)
  ) 
}

ext_pc_comb.pred <- dh.metaMSii(ext_pc.pred)

ext_pc.pred %<>%
  select(cohort, age, sii, se) %>%
  bind_rows(., ext_pc_comb.pred) %>%
  mutate(low_ci = sii - 1.96*se, 
         upper_ci = sii + 1.96*se)

ext_pc.pred %>% print(n = Inf)

ggplot() + 
  geom_line(data = ext_pc.pred, aes(x = age, y = sii, colour = cohort)) +
  geom_ribbon(data = ext_pc.pred, aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.2) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 80), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  geom_hline(yintercept = -20, alpha = 0.05) +
  scale_colour_manual(values = palette_ext)


## ---- Standard errors --------------------------------------------------------



ext_pc.pred %>%
  pivot_wider(
    names_from = cohort,
    values_from = sii
  )


str(list(x, y, z))

x <- c(1, 2, 3)
y <- c(10, 20, 30)
z <- c(3, 2, 1)

pmap(list(x, y, z), ~ ..1 + ..3)


test <- ext_pc.pred

names(test) <- c("a", "b", "c", "d")

test %>% pmap(function(a, b, c, d){
  
  a["sii"]
  
})

ext_pc.pred %>% pmap(~ paste0(..1["sii"], ..2["sii"]))

ext_pc.pred <- bind_rows(ext_pc.pred) %>% 
  filter(cohort != "combined") %>%
  mutate(cohort = factor(cohort))

ext_pc.pred %>% distinct %>% print(n = Inf)
# Now we reshape to wide format




ext_pc.pred %<>%
  mutate(se = ext_se_vec,
         low_ci = sii - 1.96*se, 
         upper_ci = sii + 1.96*se) 

# Now we meta-analyse the predicted values
dh.metaManual <- function(terms, betas, ses){
  
  nvar <- seq(1, dim(betas)[1], 1)
  
  ma <- nvar %>%
    map(function(x){
      rma(
        yi = betas[x, ],
        sei = ses[x, ], 
        method = "ML")
    })
  
  out <- tibble(
    term = terms,
    coef = ma %>% map_chr(function(x){x[["beta"]]}),
    se = ma %>% map_chr(function(x){x[["se"]]})
  )
  
  return(out)
  
}

## ---- Internalising ----------------------------------------------------------
int_pc_ref <- dh.predRefFull(
  df = "analysis_df",
  core_agevar = "int_age_",
  coeftab = int_pc.tab,
  fixed = "edu_rank_num",
  conns = conns[int_pc_coh])

int_pc.pred <- int_pc_ref %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_2 = age^-2,
        age_m_1 = age^-1,
        sii = edu_rank_num + age_m_2*poly_1_ed + age_m_1*poly_2_ed
      )
      
      return(pred)
    }
  )

int_pc.pred <- bind_rows(int_pc.pred) 

ext_nl.tab
ext_pc_ref


## This gives sensible answers
ext_pc_ref %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_1 = age^-1,
        age_m_0_5 = age^-0.5,
        sii = edu_rank_num + age_m_1*poly_1_ed + age_m_0_5*poly_2_ed
      )
      
      return(pred)
    }
  )

## This gives sensible answers
tmp <- ext_nl.tab %>%
  pmap(
    function(cohort, low_age = 1, high_age = 18, ext_age_m_1_ed, ext_age_m_0_5_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_1 = age^-1,
        age_m_0_5 = age^-0.5,
        sii = edu_rank_num + age_m_1*ext_age_m_1_ed + age_m_0_5*ext_age_m_0_5_ed
      )
      
      return(pred)
    }
  ) %>% bind_rows()

age_min_max %>% filter(cohort_ref %in% c("chop", "moba", "raine")) %>%
  pmap(function(cohort_ref, perc_5, perc_95, ...){
    
    tmp %>%
      filter(cohort == cohort_ref & between(age, perc_5, perc_95))
  })
################################################################################
# Calculate standard errors  
################################################################################
ext_pc_se.pred <- ext_pc.pred %>% filter(cohort != "Combined") %>%
  mutate(study_ref = case_when(
    cohort == "DNBC" ~ "study1", 
    cohort == "MoBa" ~ "study2", 
    cohort == "Raine" ~ "study3"))


ext_pc.fit

ext_pc_coh

## ---- Marginal means and standard errors for each cohort ---------------------
study_ref = "study1"
age_m_1 = 0.143
age_m_0_5 = 0.378

se_vec <- ext_pc_se.pred %>%
  pmap(function(study_ref, age_m_1, age_m_0_5, ...){
    
    vcov <- ext_pc.fit$output.summary[[study_ref]]$vcov
    C <- c(0, 0, 0, 1, 0, age_m_1, age_m_0_5)
    std.er <- sqrt(t(C) %*% vcov %*% C)
    out <- std.er@x
    return(out)}) %>%
  unlist


# These SEs look correct

ext_pc.pred %>% filter(age == 7)
ext_pc_se.pred %<>%
  mutate(se = se_vec,
         low_ci = sii - 1.96*se, 
         upper_ci = sii + 1.96*se) 

ext_sdq_com <- dh.metaManual(
  terms = alsp_ext.tab$term,
  betas = ext_sdq_betas, 
  ses = ext_sdq_ses)

ggplot() + 
  geom_line(data = ext_pc_se.pred, aes(x = age, y = sii, colour = cohort)) +
  geom_ribbon(data = ext_pc_se.pred, aes(x = age, ymin = low_ci, ymax = upper_ci), alpha = 0.2) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 40), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  geom_hline(yintercept = -20, alpha = 0.05) +
  scale_colour_manual(values = palette_ext)

%>%
  select(cohort, age, sii, low_ci, upper_ci, everything())

################################################################################
# 8. Plot SII  
################################################################################

## ---- Theme ------------------------------------------------------------------
theme_traj <- theme(
  plot.background = element_rect(fill =scales::alpha("#CCCCCC", 0.3)),  #Background outside of plot
  panel.background = element_rect(fill="white"),  #Background for plot
  panel.grid.major=element_line(colour="grey"), #Major and minor gridlines
  panel.grid.minor=element_line(colour="white"), 
  panel.spacing = unit(1, "lines"),
  plot.title = element_text(hjust = 0.5, vjust=0, size=16, face="bold"), #Plot title, thought don't tend to use
  text=element_text(size=9), #General text 
  axis.title.y = element_text(size=14, margin = margin(t = 0, r = 10, b = 0, l = 0)), #Axis labels
  axis.title.x = element_text(size=14, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.text.x = element_text(size=11, margin = margin(t = 4, r=0, b=0, l=0), colour="black"), #Axis text
  axis.text.y = element_text(size=11, margin = margin(t = 0, r=4, b=0, l=0), colour="black"),
  axis.ticks.length=unit(0.3, "cm"),
  axis.ticks = element_line(colour = "grey"),
  strip.text.x = element_text(size=11),
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


palette_ext <- c(rep("#005690", 3), "#ff2600")
palette_int <- c(rep("#005690", 2), "#ff2600")


## ---- Arrange cohorts better -------------------------------------------------
ext_labs <- c("DNBC", "MoBa", "Raine", "Combined")

ext_pc.pred %<>%
  mutate(
    cohort = case_when(
      cohort == "dnbc" ~ "DNBC",
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined")) %>%
  mutate(
    cohort = factor(cohort, labels = ext_labs, levels = ext_labs, ordered = TRUE)
  )

int_pc.pred %<>%
  mutate(
    cohort = case_when(
      cohort == "moba" ~ "MoBa",
      cohort == "raine" ~ "Raine",
      cohort == "combined" ~ "Combined")) %>%
  mutate(
    cohort = factor(cohort, labels = ext_labs, levels = ext_labs, ordered = TRUE)
  )


## ---- Externalising ----------------------------------------------------------
ext_pc.plot <- ggplot() + 
  geom_line(data = ext_pc.pred, aes(x = age, y = sii, colour = cohort)) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 40), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  geom_hline(yintercept = -20, alpha = 0.05) +
  scale_colour_manual(values = palette_ext)


## ---- Internalising ----------------------------------------------------------
int_pc.plot <- ggplot() + 
  geom_line(data = int_pc.pred, aes(x = age, y = sii, colour = cohort)) +
  facet_wrap(~cohort, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 40), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  theme(legend.position = "none") +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  geom_hline(yintercept = -20, alpha = 0.05) +
  scale_colour_manual(values = palette_int)


## ---- Save plots --------------------------------------------------------------
ggsave(
  filename="./figures/ext_pc.png", 
  plot = ext_pc.plot,
  h = 18, w = 25, units="cm", dpi=1200,
  device="png")

ggsave(
  filename="./figures/int_pc.png", 
  plot = int_pc.plot,
  h = 18, w = 25, units="cm", dpi=1200,
  device="png")


################################################################################
# 9. Repeat analyses stratified by sex  
################################################################################

## Hard to formally test for this within datashield, at least without doing a
## lot of work. Let's do it by eye to start

################################################################################
# Create subsets  
################################################################################

## ---- Males ------------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$sex", 
  V2.name = "1",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "analysis_df_m")

## ---- Females ----------------------------------------------------------------
ds.dataFrameSubset(
  df.name = "analysis_df", 
  V1.name = "analysis_df$sex", 
  V2.name = "2",
  Boolean.operator = "==",
  keep.NAs = FALSE,
  newobj = "analysis_df_f")

datashield.workspace_save(conns, "mhtraj_11")
conns <- datashield.login(logindata, restore = "mhtraj_11")

################################################################################
# Revised models 
################################################################################

## ---- Externalising males ----------------------------------------------------
ext_pc_m.fit <- ds.lmerSLMA(
  dataName = "analysis_df_m",
  formula = "ext_pc_ ~ 1 + ext_age_m_1 + ext_age_m_0_5 + edu_rank_num + 
  edu_rank_num*ext_age_m_1 + edu_rank_num*ext_age_m_0_5 + (1|child_id_int)",
  datasources = conns[ext_pc_coh])

ext_pc_m.tab <- dh.lmerTab(ext_pc_m.fit)
colnames(ext_pc_m.tab) <- c("coefficient", ext_pc_coh, "combined")


## ---- Externalising females ----------------------------------------------------
ext_pc_f.fit <- ds.lmerSLMA(
  dataName = "analysis_df_f",
  formula = "ext_pc_ ~ 1 + ext_age_m_1 + ext_age_m_0_5 + edu_rank_num + 
  edu_rank_num*ext_age_m_1 + edu_rank_num*ext_age_m_0_5 + (1|child_id_int)",
  datasources = conns[ext_pc_coh])

ext_pc_f.tab <- dh.lmerTab(ext_pc_f.fit)
colnames(ext_pc_f.tab) <- c("coefficient", ext_pc_coh, "combined")


## ---- Internalising males ----------------------------------------------------
int_pc_m.fit <- ds.lmerSLMA(
  dataName = "analysis_df_m",
  formula = "int_pc_ ~ 1 + int_age_m_2 + int_age_m_1 + edu_rank_num +
  edu_rank_num*int_age_m_2 + edu_rank_num*int_age_m_1 + (1|child_id_int)",
  datasources = conns[int_pc_coh])

int_pc_m.tab <- dh.lmerTab(int_pc_m.fit)
colnames(int_pc_m.tab) <- c("coefficient", int_pc_coh, "combined")


## ---- Internalising females ----------------------------------------------------
int_pc_f.fit <- ds.lmerSLMA(
  dataName = "analysis_df_f",
  formula = "int_pc_ ~ 1 + int_age_m_2 + int_age_m_1 + edu_rank_num +
  edu_rank_num*int_age_m_2 + edu_rank_num*int_age_m_1 + (1|child_id_int)",
  datasources = conns[int_pc_coh])

int_pc_f.tab <- dh.lmerTab(int_pc_f.fit)
colnames(int_pc_f.tab) <- c("coefficient", int_pc_coh, "combined")



################################################################################
# Define new function 
################################################################################

dh.predRefSex <- function(df, core_agevar, coeftab, fixed, conns){
  
  
  ## ---- Make factor version of rank variable -----------------------------------
  fix_fac <- paste0(fixed, "_fac")
  
  ds.asFactor(paste0(df, "$", fixed), newobj = fix_fac, datasources = conns)
  ds.cbind(c(df, fix_fac), newobj = df, datasources = conns)
  
  ## ---- Get age info for each cohort -------------------------------------------
  ages_ref <- dh.getStats(
    df = df,
    vars = c(core_agevar, fix_fac),
    conns = conns
  )
  
  coefs <- as_tibble(cbind(cohort = names(coeftab), t(coeftab))) %>%
    filter(cohort != "coefficient")
  
  ages <- ages_ref$continuous %>%
    select(cohort, perc_5, perc_95) %>%
    filter(cohort != "combined")
  
  comb_ages <- tibble(
    cohort = "combined",
    perc_5 = min(ages$perc_5, na.rm = TRUE),
    perc_95 = max(ages$perc_95, na.rm = TRUE)
  )
  
  fixed <- ages_ref$categorical %>%
    filter(value != 0 & cohort != "combined") %>%
    group_by(cohort) %>%
    mutate(fact_levels = paste(category, collapse = ",")) %>%
    slice(1) %>%
    select(cohort, fact_levels)
  
  ages <- ages_ref$continuous %>%
    select(cohort, perc_5, perc_95) %>%
    filter(cohort != "combined")
  
  comb_ages <- tibble(
    cohort = "combined",
    perc_5 = min(ages$perc_5, na.rm = TRUE),
    perc_95 = max(ages$perc_95, na.rm = TRUE)
  )
  
  ages <- bind_rows(ages, comb_ages)
  
  predref <- left_join(coefs, fixed, by = "cohort") %>%
    left_join(., ages, by = "cohort")
  
  colnames(predref) <- c("cohort", "intercept", coeftab$coefficient[2:4], 
                         "poly_1_ed", "poly_2_ed", "fact_levels", "low_age", "high_age")
  
  predref %<>% mutate_at(vars(intercept, coeftab$coefficient[2:4], poly_1_ed, poly_2_ed), as.numeric)
  
  
  return(predref)
}

################################################################################
# Sex-stratified SII  
################################################################################

## ---- Externalising males ----------------------------------------------------
ext_pc_ref_m <- dh.predRefSex(
  df = "analysis_df_m",
  core_agevar = "ext_age_",
  coeftab = ext_pc_m.tab,
  fixed = "edu_rank_num",
  conns = conns[ext_pc_coh])

ext_pc_m.pred <- ext_pc_ref_m %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_1 = age^-1,
        age_m_0_5 = age^-0.5,
        sii = edu_rank_num + age_m_1*poly_1_ed + age_m_0_5*poly_2_ed
      )
      
      return(pred)
    }
  )

ext_pc_m.pred <- bind_rows(ext_pc_m.pred) %>% mutate(sex = "male")

## ---- Externalising females --------------------------------------------------
ext_pc_ref_f <- dh.predRefSex(
  df = "analysis_df_f",
  core_agevar = "ext_age_",
  coeftab = ext_pc_f.tab,
  fixed = "edu_rank_num",
  conns = conns[ext_pc_coh])

ext_pc_f.pred <- ext_pc_ref_f %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_1 = age^-1,
        age_m_0_5 = age^-0.5,
        sii = edu_rank_num + age_m_1*poly_1_ed + age_m_0_5*poly_2_ed
      )
      
      return(pred)
    }
  )

ext_pc_f.pred <- bind_rows(ext_pc_f.pred) %>% mutate(sex = "female")


## ---- Internalising males ----------------------------------------------------
int_pc_ref_m <- dh.predRefSex(
  df = "analysis_df_m",
  core_agevar = "int_age_",
  coeftab = int_pc_m.tab,
  fixed = "edu_rank_num",
  conns = conns[int_pc_coh])

int_pc_m.pred <- int_pc_ref_m %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_2 = age^-2,
        age_m_1 = age^-1,
        sii = edu_rank_num + age_m_2*poly_1_ed + age_m_1*poly_2_ed
      )
      
      return(pred)
    }
  )

int_pc_m.pred <- bind_rows(int_pc_m.pred) %>% mutate(sex = "male")


## ---- Internalising females ----------------------------------------------------
int_pc_ref_f <- dh.predRefSex(
  df = "analysis_df_f",
  core_agevar = "int_age_",
  coeftab = int_pc_f.tab,
  fixed = "edu_rank_num",
  conns = conns[int_pc_coh])

int_pc_f.pred <- int_pc_ref_f %>%
  pmap(
    function(cohort, low_age, high_age, poly_1_ed, poly_2_ed, edu_rank_num, ...) {
      agevec <- seq(low_age, high_age, by = 0.01)
      
      pred <- tibble(
        cohort = cohort,
        age = rep(agevec, 3),
        age_m_2 = age^-2,
        age_m_1 = age^-1,
        sii = edu_rank_num + age_m_2*poly_1_ed + age_m_1*poly_2_ed
      )
      
      return(pred)
    }
  )

int_pc_f.pred <- bind_rows(int_pc_f.pred) %>% mutate(sex = "female")


## ---- Combine male and female estimates --------------------------------------
ext_pc_sex.pred <- bind_rows(ext_pc_m.pred, ext_pc_f.pred) %>% 
  mutate(outcome =  "Externalising")

int_pc_sex.pred <- bind_rows(int_pc_m.pred, int_pc_f.pred) %>% 
  mutate(outcome =  "Internalising")

sex_pc <- bind_rows(ext_pc_sex.pred, int_pc_sex.pred) %>% 
  select(cohort, age, sii, outcome, sex) %>%
  filter(cohort == "combined") %>%
  mutate(
    cohort = case_when(cohort == "combined" ~ "Combined"), 
    sex = case_when(sex == "male" ~ "Male", sex == "female" ~  "Female"), 
    sex = factor(sex, levels = c("Male", "Female"), ordered = TRUE),
    outcome =  factor(outcome, levels = c("Externalising", "Internalising"), 
                      ordered = TRUE))


################################################################################
# Plot sex-stratified SII  
################################################################################
sex_pc.plot <- ggplot() + 
  geom_line(data = sex_pc, aes(x = age, y = sii, colour = sex)) +
  facet_wrap(~outcome, ncol = 1) +
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 2), expand = c(0, 0)) + 
  scale_y_continuous(limit = c(-40, 40), breaks = seq(-40, 40, 20), expand = c(0, 0)) +
  theme_traj +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
  geom_hline(yintercept = 20, alpha = 0.05) +
  geom_hline(yintercept = -20, alpha = 0.05) +
  scale_colour_manual(values = c("#325573", "#d74632"))


## ---- Save plots --------------------------------------------------------------
ggsave(
  filename="./figures/sex_pc.png", 
  plot = sex_pc.plot,
  h = 18, w = 25, units="cm", dpi=1200,
  device="png")





# I think first thing is to make slides based on these results
# Then I can add things like (i) confidence bands, (ii) predicted differences


## ---- Cohort numbers ---------------------------------------------------------
included_n <- paste0("study", c(1:3)) %>%
  map(function(x){
    
    ext_pc.fit$output.summary[[x]]$ngrps    
    
  })

names(included_n) <- ext_pc_coh

