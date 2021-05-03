################################################################################
## Project: trajectory-tutorial
## Script purpose: Show how to do simple linear and fractional polynomial models
## Date: 19th March 21
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

################################################################################
# 1. Fitting simple linear trajectory  
################################################################################
ext_lin.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + edu_rank_num + ext_age_ + sex + 
  (1|child_id_int)")

## Here your outcome is externalising percentiles, and your fixed effects are
## maternal education, child age and child sex. This model also specified a
## random intercept, in other words specifying a cluster for each individual
## whereby they have a different starting value. Should make sense if you read
## the papers I sent.

################################################################################
# 2. Fitting non-linear trajectories using fractional polynomials  
################################################################################
################################################################################
# First make transformations of age term  
################################################################################
## This is more complicated! What I do in my analysis is to try lots of 
## different combinations of fractional polynomials and see which one fits
## the best. 

## ---- Define function --------------------------------------------------------
dh.makeAgePolys <- function(df, agevars, conns) {
  
  poly_names <- c("m_2", "m_1", "m_0_5", "log", "0_5", "2", "3")
  
  poly_form <- c("^-2", "^-1", "^-0.5", "^0", "^0.5", "^2", "^3")
  
  df_age <- c(paste0(df, "$", agevars))
  
  polys <- tibble(
    poly = cross2(agevars, poly_names) %>% map_chr(paste, sep = "", collapse = ""),
    form = cross2(df_age, poly_form) %>% map_chr(paste, sep = "", collapse = "")
  )
  
  polys %>%
    pmap(function(poly, form, ...) {
      ds.assign(
        toAssign = form,
        newobj = poly,
        datasources = conns
      )
    })
  
  ds.cbind(x = c(df, polys$poly), newobj = df)
  
  cat("\nThe following transformations of age have been created in
    dataframe:", df, "\n\n", polys$poly)
}

## ---- Run function -----------------------------------------------------------
dh.makeAgePolys(
  df = "analysis_df", 
  agevars = c("ext_age_", "int_age_"),
  conns = conns
)

################################################################################
# Now we need to define formulas to for the models we're going to run  
################################################################################

## ---- Define function --------------------------------------------------------
dh.makeLmerForm <- function(
  outcome, idvar, agevars, random = "intercept", fixed = NULL, age_interactions = NULL) {
  random_eff <- ifelse(random == "intercept", "(1|child_id_int)")
  
  poly_fixed <- combn(agevars, 2, paste, collapse = "+")
  
  
  if (is.null(fixed) & !is.null(age_interactions)) {
    stop("You must specify fixed effects if you want to include age X fixed effects interactions")
  } else if (is.null(fixed) & is.null(age_interactions)) {
    forms <- paste0(outcome, "~1+", poly_fixed, "+", random_eff)
  } else if (!is.null(fixed) & is.null(age_interactions)) {
    forms <- paste0(outcome, "~1+", poly_fixed, "+", fixed, "+", random_eff)
  } else if (!is.null(fixed) & !is.null(age_interactions)) {
    fixed_tmp <- paste0(fixed, "*", agevars)
    fixed_int <- combn(fixed_tmp, 2, paste, collapse = "+")
    
    forms <- paste0(outcome, "~1+", poly_fixed, "+", fixed, "+", fixed_int, "+", random_eff)
  }
  
  out <- tibble(
    polys = combn(agevars, 2, paste, collapse = ","),
    formulae = forms
  )
  
  return(out)
}

## ---- Run function -----------------------------------------------------------
ext_form_pc <- dh.makeLmerForm(
  outcome = "ext_pc_", 
  idvar = "child_id_int", 
  agevars = c("ext_age_", "ext_age_m_2", "ext_age_m_1", "ext_age_m_0_5", 
              "ext_age_log", "ext_age_0_5", "ext_age_2", "ext_age_3"), 
  fixed = "edu_rank_num",
  age_interactions = "edu_rank_num")


################################################################################
# Next we need to run lots of models using different fractional polynomials  
################################################################################

## ---- Define function --------------------------------------------------------
dh.lmeMultPoly <- function(conns = conns, df, formulae) {
  log_lik <- NULL
  
  ## ---- Run the models ---------------------------------------------------------
  models <- formulae$formulae %>%
    map(
      ~ ds.lmerSLMA(
        dataName = df,
        formula = .x,
        datasources = conns
      )
    )
  
  ## ---- Summarise convergence info ---------------------------------------------
  convergence <- models %>% map(~ .x$Convergence.error.message)
  names(convergence) <- formulae$polys
  
  if (all(str_detect(flatten_chr(convergence), "no convergence error reported") != TRUE)) {
    warning("Not all models have converged for all cohorts. Check 'convergence' table for more details")
  }
  
  ## ---- Summarise fit info -----------------------------------------------------
  nstudies <- paste0("study", seq(1, length(conns), 1))
  
  fit.tab <- models %>% map(function(x){
    
    nstudies %>% map(function(y){  
      
      tibble(
        loglik = x$output.summary[[y]]$logLik
      )
    })
    
  })
  
  fit.tab <- fit.tab %>% map(function(x){set_names(x, nstudies)})
  names(fit.tab) <- formulae$polys
  fit.tab <- fit.tab %>% map(unlist)
  
  fit.tab <- bind_rows(fit.tab, .id = "model")
  
  fit.tab %<>% pivot_longer(
    cols = !model, 
    names_to = "study",
    values_to = "loglik"
  ) %>% group_split(study)
  
  
  fit.tab %<>% map(function(x){
    mutate(x, log_rank = dense_rank(desc(x$loglik))) %>%
      arrange(log_rank)
    
  })
  
  fit.tab <- bind_rows(fit.tab) %>% pivot_wider(
    names_from = study, 
    values_from = c(loglik, log_rank)) 
  
  colnames(fit.tab) <- str_remove(colnames(fit.tab), ".loglik")
  
  fit.tab %<>% mutate(av_rank = rowMeans(select(., starts_with("log_rank")), na.rm = TRUE)) %>%
    arrange(av_rank)
  
  out <- list(models = models, convergence = convergence, fit = fit.tab)
  
  return(out)
}

## ---- Run function -----------------------------------------------------------
ext_poly_pc.fit <- dh.lmeMultPoly(
  df = "analysis_df", 
  formulae = ext_form_pc, 
  conns = conns)

################################################################################
# Now lets look to see which had the best fit
################################################################################
ext_poly_pc.fit$models[[which.min(ext_poly_pc.fit$fit$av_rank)]]


################################################################################
# Rerun best fitting models  
################################################################################

## ---- Externalising ----------------------------------------------------------
ext_pc.fit <- ds.lmerSLMA(
  dataName = "analysis_df",
  formula = "ext_pc_ ~ 1 + ext_age_m_1 + ext_age_m_0_5 + edu_rank_num + sex + 
  edu_rank_num*ext_age_m_1 + edu_rank_num*ext_age_m_0_5 + (1|child_id_int)",
  datasources = conns)

## At this point you can put in whichever covariates you want


################################################################################
# Predicted values  
################################################################################

## This is much more difficult! Once you've got this far lets chat about it.
