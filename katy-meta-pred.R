################################################################################
# Hack of a function but it seems to work  
################################################################################
katyMetaPred <- function(model, vars, new_data, coh_names, method){
  
  coef_names <- model %>% 
    map(function(x){
      
      dimnames(x$output.summary$study1$coefficients)[[1]]
      
    }) %>%
    set_names(coh_names)
  
  var_exists <- coef_names %>%
    map(~ vars %in% . == TRUE) %>%
    unlist
  
  if(all(var_exists) != TRUE){
    
    stop("The variable(s) you want to meta-analyse don't exist in all models")
  }
  
  ref <- tibble(
    model = model, 
    coh_names = coh_names)
  
  coefs <- ref %>%
    pmap(function(model, coh_names){
      
      dh.lmTab(
        model = model,
        type = "lmer_slma",
        coh_names = coh_names,
        direction = "wide", 
        ci_format = "separate")$fixed %>%
        dplyr::filter(cohort != "combined")
    }) %>%
    bind_rows %>%
    dplyr::filter(variable %in% vars)
  
  model_holder <- coefs %>%
    group_by(variable) %>%
    group_keys 
  
  ## ---- Meta-analyse ---------------------------------------------------------
  ma.fit <- coefs %>%
    group_by(variable) %>%
    group_split() %>%
    map(
      ~rma.uni(
        yi = .x$est, 
        sei = .x$se, 
        method = method, 
        control=list(stepadj=0.5, maxiter=1000)))
  
  ## ---- Put back together ----------------------------------------------------
  model_holder <- model_holder %>%
    mutate(meta = ma.fit)
  
  ma.out <- model_holder %>%
    pmap_df(function(variable, meta){
      
      tibble(
        variable = variable,
        est = meta$beta[1],
        se = meta$se,
        lowci = meta$ci.lb,
        uppci = meta$ci.ub,
        i2 = round(meta$I2, 1), 
        n_coh = meta$k)
      
    }) %>%
    mutate(metafor_obj = model_holder$meta)
  
  coef_names <- ma.out$variable 
  
  ## ---- Now we get the coefficients for each cohort ----------------------------
  coefs_by_cohort <- ma.out %>%
    dplyr::select(variable, est) %>%
    pivot_wider(
      names_from = variable,
      values_from = est) 
  
  ## ---- Make sure the columns are in the correct order -------------------------
  coefs_by_cohort %<>% dplyr::select(all_of(coef_names))
  newdata_min <- new_data %>% select(all_of(coef_names))
  
  ## ---- Now we multiply the coefficients by new data ---------------------------
  fixed <- newdata_min %>%
    pmap_df(function(...) {
      out <- c(...) * coefs_by_cohort
      return(out)
    }) %>% as_tibble
  
  
  ## ---- Now do the business ----------------------------------------------------
  pred <- new_data %>%
    mutate(predicted = rowSums(fixed))
  
  
  return(list(coefs = ma.out, predicted = pred))
  
}

################################################################################
# 2. Fit separate models for each cohort with differing covariates  
################################################################################
cohort_1.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + ext_age_ + ext_age__2 + sex + (1|child_id_int)", 
  datasources = conns["alspac"])

cohort_2.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + ext_age_ + ext_age__2 + agebirth_m_y + (1|child_id_int)", 
  datasources = conns["genr"])

cohort_3.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + ext_age_ + ext_age__2 + int_raw_ + (1|child_id_int)", 
  datasources = conns["moba"])

newdata <- tibble(
  ext_age_ = seq(1, 100, 1), 
  ext_age__2 = seq(1, 100, 1))

################################################################################
# 3. Get predicted values  
################################################################################

## This returns a list containing (1) meta-analysed coefficients and (2) the 
## predicted values

predicted <- katyMetaPred(
  model = list(alspac.fit, genr.fit, dnbc.fit, eden.fit, inma.fit),
  vars = c("fcentre_care", "int_age_m_0_5", "int_age_m_2", 
           "fcentre_care*int_age_m_0_5", "fcentre_care*int_age_m_2"),
  new_data = newdata,
  coh_names = c("alspac", "genr", "dnbc", "eden", "inma"),
  method = "REML")

predicted$pred %>% print(n = Inf)

