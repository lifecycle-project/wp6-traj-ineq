################################################################################
## Project: wp6-traj-ineq
## Script purpose: Test MLM and write related functions 
## Date: 30th September 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

################################################################################
# 1. Load workspace  
################################################################################

## Here we will start by just using MoBa
opals <- datashield.login(logindata, restore = "traj_assigned_30_09_20") 

opals <- opals["moba"]


ds.dataFrameSubset(
  df.name = "cbcl_sub",
  V1.name = "cbcl_sub$ext_raw_", 
  V2.name = "0", 
  Boolean.operator = ">=", 
  keep.NAs = FALSE,
  newobj = "cbcl_comp"
)

ds.dim("cbcl_comp")


ds.summary("cbcl_sub$int_raw_")

################################################################################
# 2. Show available data  
################################################################################
ds.scatterPlot(
  x = "cbcl_sub$int_age_", 
  y = "cbcl_sub$int_raw_"
)


################################################################################
# 3. Multi-level model  
################################################################################
ds.asInteger("cbcl_sub$child_id", "child_id_int")

ds.dataFrame(
  x = c("cbcl_sub", "child_id_int"), 
  newobj = "cbcl_sub"
)

test.mod <- ds.lmerSLMA(
  dataName = "cbcl_sub",
  formula = "ext_raw_~ ext_age_ + (1|child_id_int)"
)




################################################################################
# 4. Create polynomial terms  
################################################################################
df <- "cbcl_sub"
agevar <- "ext_age_"


age_comb <- paste0("cbcl_sub", "$", agevar)

polys = tibble(
  poly = c("age", "age_m_2", "age_m_1", "age_m_0_5", "age_log", "age_0_5", "age_2", "age_3"),
  form = c(
    age_comb, 
    paste0(age_comb, "^-2"), 
    paste0(age_comb, "^-1"), 
    paste0(age_comb, "^-0.5"), 
    paste0(age_comb, "^0"), 
    paste0(age_comb, "^0.5"), 
    paste0(age_comb, "^2"), 
    paste0(age_comb, "^3"))
  ) 
    
polys %>%
  pmap(function(poly, form){
    ds.assign(
      toAssign = form, 
      newobj = poly
    )
  })

ds.cbind(x = c(df, polys$poly), newobj = df)

################################################################################
# 5. Fit all polynomials   
################################################################################
ext_all <- dh.lmeMultPoly(data = "cbcl_sub", outcome = "ext_raw_")

best <- ext_all[[1]][order(ext_all[[2]]$log_rank)]

str(best[[1]])

################################################################################
# 6. Extract predicted values for best fitting model
################################################################################
predicted <- tibble(
  age = seq(3, 7, 0.001), 
  predicted = 98.16 + (age^-0.5)*-69.23 + (age^0.5)*-23.11)

################################################################################
# 7. Plot  
################################################################################
ggplot(data = predicted, aes(x = age, y = predicted)) + 
  geom_line(size = 1, colour = "#197d78", alpha = 0.4) 


################################################################################
# 8. Spline approach  
################################################################################






  
plot(predicted$age, predicted$predicted)


ext_all[2]



ds.ls()

  })
    
    
  )

  ds.assign(
    toAssign='cbcl_sub$ext_age_^-2', 
    newobj='test'
  )
  
  ds.summary("test")
  
correct_names <- data.frame(matrix(NA, nrow = length(polys), ncol = 2))
correct_names[, 1] <- polys
correct_names[, 2] <- c("age", "age^-2", "age^-1", "age^-0.5", "log(age)", 
                        "age^0.5", "age^2", "age^3")




test.mod



ds.lmerSLMA(
  dataName = "mhrep",
  formula = "int_pc_~ int_age_ + (1|child_id)",
  datasources = opals["inma"]
)

ds.mean("sdq_sub$int_raw", datasources = opals["chop"])

################################################################################
# 4. MLM  
################################################################################

## Let's try and run a basic mlm

################################################################################
# 3. Write spline function  
################################################################################
install.packages("lspline")
library(lspline)

data <- seq(1, 100)
knots <- c(3, 5)


## ---- Find out what the possible values are of age variable ------------------
ds.Boole(
  V1 = "mhrep$int_age_", 
  V2 = 3, 
  Boolean.operator = ">", 
  newobj = "s1"
)

ds.recodeValues(
  var.name = "s1",
  values2replace.vector = 1,
  new.values.vector = s1_vals_b,
  newobj = "s1", 
  datasources = opals["raine"]
)





value_info <- ds.asFactor("mhrep$int_age_", "age_fac")[[1]] %>%
  as_tibble() 

value_info %<>%
  mutate(value = as.numeric(value)) %>%
  arrange(value)

s1_vals_a <- value_info %>%
  filter(value > 3) %>%
  pull()

s1_vals_b <- rep(3, length(s1_vals_a))

ds.recodeValues(
  var.name = "mhrep$int_age",
  values2replace.vector = s1_vals_a,
  new.values.vector = s1_vals_b,
  newobj = "s1", 
  datasources = opals["inma"]
)

str(s1_vals_b)

ds.summary("mhrep$int_age")



ds.ls()



str(s1_vals_a)
str(s1_vals_b)


$value[value_info]

min(value_info$value)
max(value_info$value)


ds.summary("age_fac")

str(levels)


ds.recodeValues(
  var.name = "mhrep$int_age",
  values2replace.vector = 
)

n <- length(data)
nvars <- length(knots) + 1
knots <- sort(knots)

rval <- matrix(0, nrow = n, ncol = nvars)

rval[, 1] <- pmin(data, knots[1])

rval[, nvars] <- pmax(data, knots[length(knots)]) - knots[length(knots)]

test2 <- seq(1, 100)

lspline(test2, c(10, 30))

pmin(test, test2)

################################################################################
# 2. Create wide dataset with variable for each year
################################################################################

## This might be way overthinking it. I wonder if I could just use ds.reShape?
## I guess the issue is that because age has fractions 

## ---- Internalising ----------------------------------------------------------
dh.makeOutcome(
  df = "mhrep", 
  outcome = "int_raw_", 
  age_var = "int_age_", 
  bands = c(0, 1, 2, 3),
  mult_action = "earliest")


ds.dataFrameFill("int_raw__derived", "int_raw__derived")

bands = c(0, rep(seq(2, 17, 2), each = 2), 18),
################################################################################
# 3. Get summary stats  
################################################################################
practice <- dh.getStats(
  df = "int_raw__derived", 
  vars = ds.colnames("int_raw__derived")[[1]])

to_plot <- separate(
  data = practice$continuous, 
  col = "variable",
  into = c("type", "age"), 
  sep = "\\.") %>%
  mutate(age = as.numeric(age))

prelim <- ggplot(
  data = to_plot %>% filter(type == "int_raw_" & cohort != "Combined"), aes(x = age, y = mean)) +
  geom_point(aes(size = valid_n)) +
  facet_wrap(vars(cohort), ncol = 1) +
  theme_khl +
  labs(title = "test", y = "Internalising", x = "Age") +
  scale_x_continuous(limit = c(0, 18)) + 
  scale_size_continuous(range = c(0.5, 2))





print(practice$continuous, n = 128)

pivot_longer(
  data = practice$continuous,
  cols = "variable",
  names_prefix = "age,",
  names_to = "value", 
  names_repair = "unique")
,


)

ds.summary("test")

ds.summary("int_raw__derived", datasources = opals[c("inma", "raine")])

ds.summary("int_raw__derived")



ds.dataFrameFill("int_raw__derived", "int_raw__derived")



## Ok progress, just need to deal with the NAs created

print(test[[2]], n = 54)




ds.summary("mhrep$int_age_")
ds.summary("mhrep$int_raw_age_")


ds.summary("int_raw__derived")

ds.summary("mhrep")

ds.tapply(X.name = "mhrep$int_raw_", 
          INDEX.names = "test",
          FUN.name = "mean")

ds.summary("summary")

ds.asFactor("mhrep$int_age_", "test")

## ---- Check that this has worked ---------------------------------------------
ds.ls()
ds.summary("nonrep")
ds.summary("yearrep")
ds.summary("mhrep")


## Additional variables needed for moba

#nonrep
child_id
cohort_id
cohort_country
ethn1_m
ethn2_m
ethn3_m
sex

#yearrep



mhrep

################################################################################
# 2. Sort out cases where not all cohorts have these variables
################################################################################

# Later on we will need to use ds.summary to get information about variables.
# However, at present it requires that all cohorts have every variable. We make
# things simpler therefore by creating empty variables where they are missing.


## ---- Fill blank variables ---------------------------------------------------
ds.dataFrameFill("nonrep", "nonrep")
ds.dataFrameFill("yearrep", "yearrep")
ds.dataFrameFill("yearrep", "mhrep")


## ---- Check for class discrepancies ------------------------------------------

# At present ds.dataFrameFill doesn't make the filled variable the same class
# as the original. Again this can cause problems later, so here we correct the 
# class of the blank variable. 

## First we check for discrepancies using a function I wrote 
## ("cs.classDiscrepancy")
cs.classDescrepancy(
  df = "nonrep", 
  vars = nonrep.vars)

cs.classDescrepancy(
  df = "yearrep", 
  vars = yearrep.vars)

cs.classDescrepancy(
  df = "mhrep", 
  vars = yearrep.vars)

## ---- Fix problem variables --------------------------------------------------

# Now we fix the problem variables and combine them with their original data
# frame

## Non-repeated
ds.asFactor("nonrep$ethn3_m", newobj.name = "ethn3_m_rev")
ds.asInteger("nonrep$ga_bj", newobj = "ga_bj_rev")

names(opals) %>%
  map(
    ~ds.dataFrame(
      x= c("nonrep", "ethn3_m_rev", "ga_bj_rev"), 
      newobj = "nonrep", 
      datasources = opals[.])
  )

## Yearly repeated
ds.asFactor("yearrep$greenyn300_", newobj.name = "greenyn300_rev_")
ds.asFactor("yearrep$areases_tert_", newobj.name = "areases_tert_rev_")

names(opals) %>%
  map(
    ~ds.dataFrame(
      x = c("yearrep", "greenyn300_rev_", "areases_tert_rev_"), 
      newobj = "yearrep", datasources = opals[.])
  )

## Check this has worked
cs.classDescrepancy(
  df = "nonrep", 
  vars = c("ethn3_m_rev", "ga_bj_rev"))

cs.classDescrepancy(
  df = "yearrep", 
  vars = c("greenyn300_rev_", "areases_tert_rev_"))

cs.rmLots(c("areases_tert_rev_", "ethn3_m_rev", "ga_bj_rev", "greenyn300_rev_"))


################################################################################
# 3. Create baseline variables from non repeated tables 
################################################################################

## ---- Gestational age at birth -----------------------------------------------

# Moba has ga_us, whilst the other cohorts have ga_bj. Here we create one ga
# variable from these separate variables.

ds.assign(
  toAssign = "nonrep$ga_bj_rev", 
  newobj = "ga_all",
  datasources = opals[opals!="moba"]
) 

ds.assign(
  toAssign = "nonrep$ga_us", 
  newobj = "ga_all",
  datasources = opals["moba"]
)


## ---- Maternal pre-pregnancy BMI ---------------------------------------------
ds.assign(
  toAssign='nonrep$prepreg_weight/(((nonrep$height_m/100))^2)', 
  newobj='prepreg_bmi'
)  


## ---- Parity -----------------------------------------------------------------

# We need to recode parity as a binary variable as there are issues later with 
# disclosive information when we run the models if we leave it ordinal.

ds.recodeLevels(
  "nonrep$parity_m", 
  newCategories = c(0, 1, 1, 1, 1),
  newobj = "parity_bin")


## ---- Combine these new variables with non-repeated dataframe ----------------

# Datashield always creates new objects rather than adding variables to 
# existing objects. Therefore whenever we create new variables we need to join
# them back with the main dataframe.
#
# There is also currently a problem with the functions "ds.dataFrame" and 
# "ds.cbind" whereby if you try to apply them to all cohorts at once the 
# resulting dataframe can become corrupted. To solve this at the moment we
# manually iterate through each cohort.

names(opals) %>%
  map(
    ~ds.dataFrame(
      x = c("nonrep", "ga_all", "prepreg_bmi", "parity_bin"),
      newobj = "nonrep_2", 
      datasources = opals[.])
  )

cs.classDescrepancy(df_name = "nonrep_2")
cs.rmLots(c("ga_all", "prepreg_bmi", "parity_bin"))


################################################################################
# 4. Create baseline variables from yearly repeated tables
################################################################################

## ---- Subset to keep observations where child's age == 0 ---------------------
ds.subset(
  x = 'yearrep', 
  subset = "baseline_vars", 
  logicalOperator = 'age_years==', 
  threshold = 0)


## ---- Convert to wide format -------------------------------------------------

# For the actual analysis we will want our dataset to be in wide format 

ds.reShape(
  data.name = "baseline_vars",
  timevar.name = "age_years",
  idvar.name = "child_id",
  v.names = c("edu_m_", "edu_f1_", "greenyn300_rev_", "green_dist_", 
              "green_size_", "ndvi300_", "areases_tert_rev_"), 
  direction = "wide", 
  newobj = "baseline_wide")


## ---- Rename baseline_vars more sensible names -------------------------------

# Currently the baseline variables we've made don't have great names because
# they've been generated automatically by the reshape function. So we give them
# some better names using a function I wrote ("cs.renameVars"). This is a short-
# cut which creates the new variables using information provided in a table and
# joins these together in a dataframe.

## First create a list with old and new variable names
old_new <- tribble(
  ~oldvar, ~newvar,
  "edu_f1_.0", "edu_f",
  "edu_m_.0", "edu_m",
  "greenyn300_rev_.0", "greenyn300",
  "green_dist_.0", "green_dist",
  "green_size_.0", "green_size", 
  "ndvi300_.0", "ndvi300",
  "areases_tert_rev_.0", "areases_tert")

## Now rename them
cs.renameVars(
  df = "baseline_wide", 
  names = old_new,
  new_df_name = "baseline_renamed")

## Merge back in
names(opals) %>%
  map(
    ~ds.dataFrame(
      x = c("baseline_wide", "baseline_renamed"),
      newobj = "baseline_wide_2",
      datasources = opals[.]
    ))

old_new %>% pull(newvar) %>% cs.rmLots

################################################################################
# 5. Calculate BMI scores from monthly repeated measures data
################################################################################

## ---- First we derive BMI scores ---------------------------------------------
ds.assign(
  toAssign='monthrep$weight_/((monthrep$height_/100)^2)', 
  newobj='bmi'
)  


## ---- Now join these back to the dataframe -----------------------------------
names(opals) %>%
  map(
    ~ds.dataFrame(
      x = c('bmi', 'monthrep'), 
      newobj = 'monthrep',
      datasources = opals[.]
    )
  )

ds.rm("bmi")

################################################################################
# 6. Create BMI variables corresponding to age brackets 
################################################################################

# This is a bit fiddly to do, as currently you can't specify multiple conditions
# to subset. What we do is first create variables (0/1) indicating for each
# row whether the subject is above or below specified age values. We
# then multiply these together, which gives a new variable indicating whether
# both criteria are met (0*0 = 0, 0*1 = 0, 1*0 = 1*1 = 1). We then use these
# variables to create our subsets.

## ---- First make a table of our upper and lower values -----------------------
bmi_cats_a <- bind_rows(
  tibble(
    value = c(0, 25, 49, 97, 169),
    op = ">="),
  tibble(
    value = c(24, 48, 96, 168, 215),
    op = "<=")) %>% 
  mutate(new_df_name = paste0("bmi_", value))


## ---- Now create objects indicating whether conditions are met ---------------

# Although we can't see them directly, they will be vectors of 1s and 0s of the
# same length as the original dataframe in each cohort (monthrep)

bmi_cats_a %>%
  pmap(function(value, op, new_df_name){
    
    ds.Boole(
      V1 = "monthrep$age_months", 
      V2 = value, 
      Boolean.operator = op, 
      newobj = new_df_name)
  })

## ---- Now create objects showing whether age is between two points -----------

## Again we first make a table with the different values we will want to feed
## to our function

bmi_cats_b <- tibble(
  formula = paste0(
    bmi_cats_a %>% filter(op == ">=") %>% pull(new_df_name),
    "*",
    bmi_cats_a %>% filter(op == "<=") %>% pull(new_df_name)), 
  varname = paste0(
    "bmi_",
    bmi_cats_a %>% filter(op == ">=") %>% pull(value),
    "_",
    bmi_cats_a %>% filter(op == "<=") %>% pull(value)))


## Now we create the variables. Again what we are creating is a series of 
## vectors of 1s and 0s indicating whether each row falls between a specified
## age range

bmi_cats_b %>%
  pmap(function(formula, varname){
    
    ds.assign(
      toAssign = formula, 
      newobj = varname)
    
  })


## ---- Now we want to find out which cohorts have data ------------------------
bmi_available <- bmi_cats_b %>%
  pmap(function(varname, ...){ds.mean(varname)}) 

names(bmi_available) <- bmi_cats_b$varname

bmi_available <-map_dfr(
  bmi_available, ~.x$Mean.by.Study[, "EstimatedMean"]) %>%
  map_dfr(~ifelse(.x == 0, "no", "yes")) %>%
  mutate(cohort = names(opals)) %>%
  select(cohort, everything())

bmi_available


## ---- Create a new table listing which subsets to create ---------------------
bmi_to_subset <- bmi_available %>%
  pivot_longer(
    cols = -cohort, 
    names_to = "varname", 
    values_to = "available") %>%
  filter(available == "yes") %>%
  select(-available) %>%
  mutate(new_subset_name = paste0(varname, "_sub"))


## ---- Now create the subsets -------------------------------------------------
bmi_to_subset %>%
  pmap(
    function(cohort, varname, new_subset_name){
      ds.dataFrameSubset(
        df.name = "monthrep", 
        V1.name = varname, 
        V2.name = "1", 
        Boolean.operator = "==", 
        keep.NAs = FALSE, 
        newobj = new_subset_name,
        datasources = opals[cohort])
    })

ds.ls()
## ---- Sort subsets by age ----------------------------------------------------

# This next step sorts the subsets by age (youngest first). This is required
# a couple of stages later when we make sure that if a subject has multiple
# observations we select the youngest.

bmi_to_subset %>%
  pmap(
    function(cohort, new_subset_name, ...){
      ds.dataFrameSort(
        df.name = new_subset_name, 
        sort.key.name = paste0(new_subset_name, "$age_months"), 
        newobj = new_subset_name, 
        sort.descending = FALSE, 
        datasources = opals[cohort])
    })


ds.dataFrameSort(
  df.name = "bmi_0_24_sub", 
  sort.key.name = "bmi_0_24_sub$age_months", 
  newobj = "bmi_0_24_sub_sort", 
  sort.descending = FALSE, 
  datasources = opals["genr"])

## Doesn't work for some reason

## ---- Create new variables indicating the age category -----------------------

# Now within each subset we create a variable indicating the age category.
# Again the way I've done it is very clunky but it works: you multiple their
# age in months by 0 (to give 0), then add the upper threshold value from the
# bmi_cats dataframe we made above.

# This is required for when we reshape back to wide format. Currently the only 
# way I can find to do this is quite clunky but it works.
bmi_to_subset %<>%
  mutate(value = str_extract(varname, '[^_]+$'), 
         age_cat_name = paste0(new_subset_name, "_age_cat"))

bmi_to_subset %>%
  pmap(
    function(cohort, new_subset_name, value, age_cat_name, ...){
      ds.assign(
        toAssign = paste0("(", new_subset_name, "$age_months * 0)+", value), 
        newobj = age_cat_name, 
        datasources = opals[cohort])
    })


## Join back with dataframe
bmi_to_subset %>%
  pmap(
    function(cohort, new_subset_name, age_cat_name, ...){
      ds.dataFrame(
        x = c(new_subset_name, age_cat_name),
        newobj = new_subset_name,
        datasources = opals[cohort])
    })

## ---- Convert subsets to wide form -------------------------------------------

# Up to now all the bmi subsets are in long form. Here we convert to wide form.
# Usefully (as specified in the help file for "ds.reshape") if multiple 
# observations exist per subject the first will be kept and subsequent dropped.
# As we earlier sorted ascending by age this means in the case a subject has
# multiple observations we keep the earliest.
bmi_to_subset %>%
  pmap(
    function(cohort, new_subset_name, age_cat_name, ...){
      ds.reShape(
        data.name = new_subset_name,
        timevar.name = age_cat_name,
        idvar.name = "child_id",
        v.names = c("bmi", "age_months"), 
        direction = "wide", 
        newobj = paste0(new_subset_name, "_wide"),
        datasources = opals[cohort])
    })


################################################################################
# 9. Merge various datasets  
################################################################################

## ---- First we merge the non repeated and yearly repeated --------------------
ds.merge(
  x.name = "nonrep_2",
  y.name = "baseline_wide_2",
  by.x.names = "child_id",
  by.y.names = "child_id",
  all.x = TRUE,
  newobj = "bmi_poc"
)


## ---- Now where there was available BMI data we merge this back in -----------
bmi_to_subset %>%
  pmap(
    function(cohort, new_subset_name, ...){
      ds.merge(
        x.name = "bmi_poc",
        y.name = paste0(new_subset_name, "_wide"),
        by.x.names = "child_id",
        by.y.names = "child_id",
        all.x = TRUE,
        newobj = "bmi_poc", 
        datasources = opals[cohort])
    })

## ---- Create blank columns where data not available --------------------------
ds.dataFrameFill("bmi_poc", "bmi_poc")

################################################################################
# 10. Create analysis dataset  
################################################################################

# So when it comes to write up the analysis, we need to be able to specify an
# analysis dataset as a subset of all data, e.g. "contained all participants 
# with at least one exposure and outcome"


## ---- First we specify vectors of exposures and outcomes ---------------------
exp.vars <- c("edu_m", "ga_all", "preg_dia", "greenyn300", "green_dist", 
              "green_size", "ndvi300")  

out.vars <- c("bmi.24", "bmi.48", "bmi.96", "bmi.168")

cov.vars <- c("sex", "preg_smk", "preg_ht", "parity_bin", "ethn3_m_rev", 
              "height_m", "prepreg_bmi", "agebirth_m_y", "areases_tert")

other.vars <- c("age_months.24", "age_months.48", "age_months.96", 
                "age_months.168")


## ---- Now we create vars indicating whether any non-missing values are present
cs.anyVarExists(
  df = "bmi_poc", 
  vars = exp.vars, 
  new_label = "exposure")

cs.anyVarExists(
  df = "bmi_poc", 
  vars = out.vars, 
  new_label = "outcome")


## ---- Next create another variable indicating whether a valid case -----------
ds.make(
  toAssign = "any_exposure+any_outcome", 
  newobj = "n_exp_out")

ds.Boole(
  V1 = "n_exp_out", 
  V2 = "2", 
  Boolean.operator = "==", 
  na.assign = 0, 
  newobj = "valid_case")

## Check how many valid cases to make sure it's plausible
ds.summary("valid_case")


## ---- Now we create a vector of all the variables we want to keep ------------
keep_vars <- c(exp.vars, out.vars, cov.vars, other.vars)


## ---- Drop variables we don't need -------------------------------------------
var_index <- names(opals) %>%
  map(
    ~cs.findVarsIndex(
      df = "bmi_poc", 
      vars = keep_vars, 
      cohorts = .))

## Now finally we subset based on valid cases and required variables
var_index %>%
  imap(
    ~ds.dataFrameSubset(df.name = "bmi_poc", 
                        V1.name = "valid_case", 
                        V2.name = "1", 
                        Boolean.operator = "==", 
                        keep.cols = .x,
                        keep.NAs = FALSE, 
                        newobj = "analysis_df", 
                        datasources = opals[.y]))


## ---- Check that this has worked ok ------------------------------------------
ds.summary("bmi_poc")
ds.summary("analysis_df")