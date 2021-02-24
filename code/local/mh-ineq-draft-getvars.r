################################################################################
## Project: mh-ineq
## Script purpose: Get variables for spline trajectories
## Date: 3rd October 2019
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(alspac)
library(dplyr)
library(magrittr)
library(tableone)

data(current)

source("./useful-code-r/code/functions/mlmQC.r")
source("./mh-ineq/code/mh-ineq-draft-functions.R")
source("./useful-code-r/code/functions/d-load-save.R")

data_path <- "z:/projects/ieu2/p6/069/working/data/"
################################################################################
# 1. Variable lists  
################################################################################

## ---- MFQ --------------------------------------------------------------------
mfq.vars <- c("mfq_10_chi", "mfq_12_chi", "mfq_13_chi", "mfq_16_chi", 
	          "mfq_17_chi", "mfq_18_chi", "mfq_21_chi", "mfq_22_chi", 
	          "mfq_23_chi")

mfq_age.vars <- c("mfq_10_chi_age", "mfq_12_chi_age", "mfq_13_chi_age", 
	         "mfq_16_chi_age", "mfq_17_chi_age", "mfq_18_chi_age", 
	         "mfq_21_chi_age", "mfq_22_chi_age", "mfq_23_chi_age")


## ---- DAWBA ------------------------------------------------------------------

# For some variables both the binary and band variables are available at all
# four time points

## ADHD
dawba_adhd_bin.vars <- c("kr803a", "kv8619", "tb8620", "fh6886")
dawba_adhd_band.vars <- c("kr850", "kv8600", "tb8600", "fh6870")

## Conduct
dawba_conduct_bin.vars <- c("kr811", "kv8621", "tb8622", "fh6890")
dawba_conduct_band.vars <- c("kr856", "kv8602", "tb8602", "fh6874")

## ODD
dawba_odd_bin.vars <- c("kr810", "kv8620", "tb8621", "fh6889")
dawba_odd_band.vars <- c("kr862", "kv8606", "tb8607", "fh6873")

## Behavioural disorder
dawba_beh_bin.vars <- c("kr813a", "kv8622", "tb8623", "fh6888")
dawba_beh_band.vars <- c("kr874", "kv8614", "tb8615", "fh6872")

## Depression
dawba_dep_bin.vars <- c("kr832a", "kv8618", "tb8619", "fh6892")
dawba_dep_band.vars <- c("kr859", "kv8603", "tb8603", "fh6876")

# There is also a parent-reported summary variable for anxiety available at the 
# first three time points

## Anxiety disorder
dawba_anx_bin.vars <- c("kr827a", "kv8617", "tb8618")
dawba_anx_band.vars <- c("kr873", "kv8613", "tb8614")

# For some of the specific anxieties parent-report bands are available at the
# first 3 time points

## Specific Phobia
dawba_spec_band.vars <- c("kr867", "kv8610", "tb8611")

## Social Phobia
dawba_soc_band.vars <- c("kr868", "kv8609", "tb8610")

## OCD
dawba_ocd_band.vars <- c("kr861", "kv8605", "tb8606")

## GAD
dawba_gad_band.vars <- c("kr860", "kv8604", "tb8605")

## Emotional disorder
dawba_emo_band.vars <- c("kr872", "kv8612", "tb8613")

# In addition there are inconsistent PTSD indicators, and self-report variables
# at tf3

## Age variables

dawba_age.vars <- c("kr991a", "kv9991a", "tb9991a", "tc9991a")


## ---- SDQ --------------------------------------------------------------------

## Emotional
sdq_emo.vars <- c("j557a", "kq348c", "n8365a", "ku707b", "kw6602b", "ta7025a",
                  "tc4025a")

## Peer problems
sdq_peer.vars <- c("j557d", "kq348e", "n8365d", "ku709b", "kw6604b", "ta7025d",
	               "tc4025d")

## Conduct
sdq_con.vars <- c("j557b", "kq348d", "n8365b", "ku708b", "kw6603b", "ta7025b", 
	              "tc4025b")

## Hyperactivity
sdq_hyp.vars <- c("j557c", "kq348b", "n8365c", "ku706b", "kw6601b", "ta7025c", 
	              "tc4025c")

## Age variables
sdq_age.vars <- c("j914", "kq998a", "n9991a", "ku991a", "kw9991a", "ta9991a",
	              "tc9991a")

## ---- PLIKS-------------------------------------------------------------------
pliks.vars <- c("ff5261", "FJPL161", "FKPL2000")
pliks_age.vars <- c("ff0011a", "FJ003a", "FKAR0010")


## ---- EPDS -------------------------------------------------------------------

## Maternal
epds_mat.vars <- c("mat_epds_b_sum", "mat_epds_c_sum", "mat_epds_e_sum", 
	               "mat_epds_f_sum", "mat_epds_g_sum", "mat_epds_h_sum", 
	               "mat_epds_k_sum", "mat_epds_l_sum", "mat_epds_n_sum", 
	               "mat_epds_r_sum")

## Maternal age variables

# Note using mother's age.
epds_mat_age.vars <- c("b925", "c994", "e694", "f992", "g994", "h992", "k9996a", 
	                   "l9996a", "n9992", "r9996a")


epds_chi_age.vars <- c("c991", "e699", "f993", "g990", "h991a", 
	                   "k9991a", "l9991a", "n9991a", "r9991a", "b924")

## Paternal
epds_pat.vars <- c("epds_pat_t1", "epds_pat_t2", "epds_pat_t3", "epds_pat_t4",
                   "epds_pat_t5", "epds_pat_t6", "epds_pat_t7", "epds_pat_t8",
                   "epds_pat_t9")


## Paternal age variables
epds_pat_age.vars <- c("epds_pat_age_t1", "epds_pat_age_t2", "epds_pat_age_t3", 
	                   "epds_pat_age_t4", "epds_pat_age_t5", "epds_pat_age_t6", 
	                   "epds_pat_age_t7", "epds_pat_age_t8", "epds_pat_age_t9")


## ---- SCDC -------------------------------------------------------------------
asc_scdc_kr.vars <- c("kr539", "kr540", "kr541", "kr542", "kr543", "kr544", 
	                  "kr545", "kr546", "kr547", "kr548", "kr549", "kr550")

asc_scdc_kv.vars <- c("kv8520", "kv8521", "kv8522", "kv8523", "kv8524", "kv8525", 
	              "kv8526", "kv8527", "kv8528", "kv8529", "kv8530", "kv8531")

asc_scdc_tb.vars <- c("tb8520", "tb8521", "tb8522", "tb8523", "tb8524", "tb8525", 
	              "tb8526", "tb8527", "tb8528", "tb8529", "tb8530", "tb8531")

asc_scdc_tc.vars <- c("tc4050", "tc4051", "tc4052", "tc4053", "tc4054", "tc4055", 
	              "tc4056", "tc4057", "tc4058", "tc4059", "tc4060", "tc4061")

asc_age.vars <- c("kr991a", "kv9991a", "tb9991a", "tc9991a")

## ---- Maternal education -----------------------------------------------------
mat_ed.vars <- "c645a"

## ---- Gestational age at birth -----------------------------------------------
ga.vars <- "kz029"


################################################################################
# 2. Get variables using ALSPAC function  
################################################################################

## ---- Get variables details --------------------------------------------------
extract.vars <- c(mfq.vars, mfq_age.vars, dawba_adhd_bin.vars, 
	dawba_adhd_band.vars, dawba_conduct_bin.vars, dawba_conduct_band.vars, 
	dawba_odd_bin.vars, dawba_odd_band.vars, dawba_beh_bin.vars, 
	dawba_beh_band.vars, dawba_dep_bin.vars, dawba_dep_band.vars, 
	dawba_anx_bin.vars, dawba_anx_band.vars, dawba_spec_band.vars, 
	dawba_soc_band.vars, dawba_ocd_band.vars, dawba_gad_band.vars, 
	dawba_emo_band.vars, dawba_age.vars, sdq_emo.vars, sdq_peer.vars, 
	sdq_con.vars, sdq_hyp.vars, sdq_age.vars, epds_mat_age.vars, 
	epds_chi_age.vars, mat_ed.vars, ga.vars, pliks.vars, pliks_age.vars, 
	asc_scdc_kr.vars, asc_scdc_kv.vars, asc_scdc_tb.vars, asc_scdc_tc.vars,
	asc_age.vars)

current.vars <-  subset(current, name %in% extract.vars)

## ---- Extract variables ------------------------------------------------------
mh_ineq.alspdata <- extractVars(current.vars)

################################################################################
# 3. Get derived variables  
################################################################################
source("c:/repos/useful-code-r/code/derived/useful-code-r-derived-child-mh.R")
source("c:/repos/useful-code-r/code/derived/useful-code-r-derived-mat-epds.R")
source("c:/repos/useful-code-r/code/derived/useful-code-r-derived-pat-epds.R")
source("c:/repos/useful-code-r/code/derived/useful-code-r-derived-child-cov.R")

mh_ineq.derdata <- merge(chi_mh.data, mat_epds.data, 
	                     by = "aln", 
	                     all.x = TRUE)

mh_ineq.derdata <- merge(mh_ineq.derdata, pat_epds.data, 
	                     by = "aln", 
	                     all.x = TRUE)

mh_ineq.derdata$qlet <- mh_ineq.derdata$qlet.x

mh_ineq.derdata <- merge(mh_ineq.derdata, chi_cov.data, 
	                     by = c("aln", "qlet"), 
	                     all.x = TRUE) %>%
dplyr::select(aln, qlet, mfq.vars, mfq_age.vars, epds_mat.vars, epds_pat.vars, 
	          epds_pat_age.vars, sex)


################################################################################
# 4. Merge datasets  
################################################################################
mh_ineq.mastdata <- merge(mh_ineq.alspdata, mh_ineq.derdata, 
	                      by = c("aln", "qlet"), all.x = TRUE)

mh_ineq.mastdata %<>% mutate_if(is.numeric, funs(ifelse( . < 0, NA, .)))

save(mh_ineq.mastdata, 
	file = "z:/projects/ieu2/p6/069/working/data/mh_ineq_mastdata.RData")

load("z:/projects/ieu2/p6/069/working/data/mh_ineq_mastdata.RData")
mh_ineq.data <- mh_ineq.mastdata


################################################################################
# 5. Recode DAWBA  
################################################################################

## ---- ADHD -------------------------------------------------------------------
mh_ineq.data %<>%
mutate(dawba_adhd_t1 = factor(2 - kr803a),
	   dawba_adhd_t2 = factor(kv8619),
	   dawba_adhd_t3 = factor(tb8620),
	   dawba_adhd_t4 = factor(2 - fh6886))

dawba_adhd_bin_rev.vars <- c("dawba_adhd_t1", "dawba_adhd_t2", "dawba_adhd_t3", 
	                         "dawba_adhd_t4")

## ---- Conduct ----------------------------------------------------------------
mh_ineq.data %<>%
mutate(dawba_con_t1 = factor(2 - kr811),
	   dawba_con_t2 = factor(kv8621),
	   dawba_con_t3 = factor(tb8622),
	   dawba_con_t4 = factor(2 - fh6890))

dawba_con_bin_rev.vars <- c("dawba_con_t1", "dawba_con_t2", "dawba_con_t3", 
	                         "dawba_con_t4")

## ---- ODD --------------------------------------------------------------------
mh_ineq.data %<>%
mutate(dawba_odd_t1 = factor(2 - kr810),
	   dawba_odd_t2 = factor(kv8620),
	   dawba_odd_t3 = factor(tb8621),
	   dawba_odd_t4 = factor(2 - fh6889))

dawba_odd_bin_rev.vars <- c("dawba_odd_t1", "dawba_odd_t2", "dawba_odd_t3", 
	                         "dawba_odd_t4")

## ---- Behavioural ------------------------------------------------------------
mh_ineq.data %<>%
mutate(dawba_beh_t1 = factor(2 - kr813a),
	   dawba_beh_t2 = factor(kv8622),
	   dawba_beh_t3 = factor(tb8623),
	   dawba_beh_t4 = factor(2 - fh6888))

dawba_beh_bin_rev.vars <- c("dawba_beh_t1", "dawba_beh_t2", "dawba_beh_t3", 
	                         "dawba_beh_t4")

## ---- Depression -------------------------------------------------------------
mh_ineq.data %<>%
mutate(dawba_dep_t1 = factor(2 - kr832a),
	   dawba_dep_t2 = factor(kv8618),
	   dawba_dep_t3 = factor(tb8619),
	   dawba_dep_t4 = factor(2 - fh6892))

dawba_dep_bin_rev.vars <- c("dawba_dep_t1", "dawba_dep_t2", "dawba_dep_t3", 
	                         "dawba_dep_t4")

## ---- Anxiety ----------------------------------------------------------------
mh_ineq.data %<>%
mutate(dawba_anx_t1 = factor(2 - kr827a),
	   dawba_anx_t2 = factor(kv8617),
	   dawba_anx_t3 = factor(tb8618))

dawba_anx_bin_rev.vars <- c("dawba_anx_t1", "dawba_anx_t2", "dawba_anx_t3")


## ---- PLIKS ------------------------------------------------------------------
mh_ineq.data %<>%
mutate(pliks_t1 = factor(ff5261), 
	   pliks_t2 = factor(FJPL161 - 1),
	   pliks_t3 = factor(FKPL2000))

pliks_rev.vars <- c("pliks_t1", "pliks_t2", "pliks_t3")


################################################################################
# 6. Score SCDC
################################################################################

## ---- Raw score --------------------------------------------------------------
mh_ineq.data %<>%
mutate_at(vars(asc_scdc_kr.vars, asc_scdc_kv.vars, asc_scdc_tb.vars, 
	           asc_scdc_tc.vars), list(~ .-1))

mh_ineq.data %<>%
  mutate(asc_t1 = rowSums(.[, asc_scdc_kr.vars], na.rm = FALSE),
         asc_t2 = rowSums(.[, asc_scdc_kv.vars], na.rm = FALSE),
         asc_t3 = rowSums(.[, asc_scdc_tb.vars], na.rm = FALSE),
         asc_t4 = rowSums(.[, asc_scdc_tc.vars], na.rm = FALSE))

asc_rev.vars <- c("asc_t1", "asc_t2", "asc_t3", "asc_t4")

################################################################################
# 5. Create age variables in months  
################################################################################

## ---- SDQ --------------------------------------------------------------------
mh_ineq.data %>%
dplyr::select(sdq_age.vars) %>%
CreateTableOne(data = .)

mh_ineq.data %<>%
mutate(sdq_04_age = j914 * 12,
	   sdq_07_age = kq998a,
	   sdq_08_age = n9991a,
	   sdq_10_age = ku991a,
	   sdq_12_age = kw9991a,
	   sdq_13_age = ta9991a,
	   sdq_17_age = tc9991a)

sdq_age_rev.vars <- c("sdq_04_age", "sdq_07_age", "sdq_08_age", "sdq_10_age", 
	                  "sdq_12_age", "sdq_13_age", "sdq_17_age")


## ---- Maternal EPDS ----------------------------------------------------------

## Age variable based on child age, conception as baseline

mh_ineq.data %<>%
mutate(ga_months = kz029 / 4.3481,
	   epds_mat_t01_age = b924 / 4.3481,
	   epds_mat_t02_age = c991 / 4.3481,
       epds_mat_t03_age = (e699 / 4.3481) + ga_months,
	   epds_mat_t04_age = f993 + ga_months,
	   epds_mat_t05_age = g990 + ga_months,
	   epds_mat_t06_age = h991a + ga_months,
	   epds_mat_t07_age = k9991a + ga_months,
	   epds_mat_t08_age = l9991a + ga_months,
	   epds_mat_t09_age = n9991a + ga_months,
	   epds_mat_t10_age = r9991a + ga_months)

epds_mat_age_rev.vars <- mh_ineq.data %>%
dplyr::select(epds_mat_t01_age : epds_mat_t10_age) %>%
colnames()

mh_ineq.data %>%
dplyr::select(epds_mat_age_rev.vars) %>%
CreateTableOne(data = .)

mh_ineq.data %>%
dplyr::select(epds_mat_age_rev.vars) %>%
Hmisc::describe(.)


## ---- Paternal epds ----------------------------------------------------------
mh_ineq.data %<>%
mutate(epds_pat_age_t1 = epds_pat_age_t1 / 4.3481,
	   epds_pat_age_t2 = epds_pat_age_t2 + ga_months,
	   epds_pat_age_t3 = epds_pat_age_t3 + ga_months,
       epds_pat_age_t4 = epds_pat_age_t4 + ga_months,
       epds_pat_age_t5 = epds_pat_age_t5 + ga_months,
       epds_pat_age_t6 = epds_pat_age_t6 + ga_months,
       epds_pat_age_t7 = epds_pat_age_t7 + ga_months,
       epds_pat_age_t8 = epds_pat_age_t8 + ga_months,
       epds_pat_age_t9 = epds_pat_age_t9 + ga_months)

epds_pat_age_rev.vars <- mh_ineq.data %>%
dplyr::select(epds_pat_age_t1 : epds_pat_age_t9) %>%
colnames()

mh_ineq.data %>%
dplyr::select(epds_pat_age_rev.vars) %>%
CreateTableOne(data = .)

mh_ineq.data %>%
dplyr::select(epds_pat_age_rev.vars) %>%
Hmisc::describe(.)


## ---- DAWBA ------------------------------------------------------------------
mh_ineq.data %<>%
mutate(dawba_age_t1 = kr991a, 
	   dawba_age_t2 = kv9991a,
	   dawba_age_t3 = tb9991a,
	   dawba_age_t4 = tc9991a)

mh_ineq.data %>%
dplyr::select(dawba_age_t1 : dawba_age_t4) %>%
CreateTableOne(data = .)

mh_ineq.data %>%
dplyr::select(dawba_age_t1 : dawba_age_t4) %>%
Hmisc::describe(.)


## ---- PLIKS ------------------------------------------------------------------
mh_ineq.data %<>%
mutate(pliks_age_t1 = ff0011a, 
	   pliks_age_t2 = FJ003a,
	   pliks_age_t3 = FKAR0010)

pliks_age_rev.vars <- c("pliks_age_t1", "pliks_age_t2", "pliks_age_t3")


## ---- SCDC -------------------------------------------------------------------
mh_ineq.data %<>%
mutate(asc_age_t1 = kr991a,
	   asc_age_t2 = kv9991a,
	   asc_age_t3 = tb9991a,
	   asc_age_t4 = tc9991a)

asc_age_rev.vars <- c("asc_age_t1", "asc_age_t2", "asc_age_t3", "asc_age_t4")


################################################################################
# 6. Calculate rank of maternal education for SII  
################################################################################
mh_ineq.data$c645a <- factor(mh_ineq.data$c645a)

## ---- Calculate proportion of individuals in each education category ---------
CreateTableOne(data = mh_ineq.data, vars = "c645a")

## ---- Create rank of maternal education --------------------------------------
mh_ineq.data %<>%
mutate(mat_ed_tmp = case_when(
c645a == 1 ~ (0 + 20.2),
c645a == 2 ~ (20.2 + 9.9),
c645a == 3 ~ (20.2 + 9.9 + 34.6),
c645a == 4 ~ (20.2 + 9.9 + 34.6 + 22.4),
c645a == 5 ~ (20.2 + 9.9 + 34.6 + 22.4 + 12.9)))

mh_ineq.data %<>%
mutate(mat_ed_rank_hun = case_when(
mat_ed_tmp == 20.2 ~ 20.2 / 2,
mat_ed_tmp == 30.1 ~ (20.2 + 30.1) / 2,
mat_ed_tmp == 64.7 ~ (30.1 + 64.7) / 2,
mat_ed_tmp == 87.1 ~ (64.7 + 87.1) / 2,
mat_ed_tmp == 100 ~ (87.1 + 100) / 2))

################################################################################
# 6. Reshape data to long format  
################################################################################

## ---- MFQ --------------------------------------------------------------------
mfq.data <- mhIneqLong(data = mh_ineq.data, 
	                   outcome = "mfq", 
                       outcome_vars = mfq.vars, 
                       age_vars = mfq_age.vars, 
                       zscore = TRUE)

## ---- SDQ emotional ----------------------------------------------------------
sdq_emo.data <- mhIneqLong(data = mh_ineq.data, 
                           outcome = "sdq_emo",
                           outcome_vars = sdq_emo.vars, 
                           age_vars = sdq_age_rev.vars, 
                           zscore = TRUE)
	                          
## ---- SDQ peer ---------------------------------------------------------------
sdq_peer.data <- mhIneqLong(data = mh_ineq.data, 
	                        outcome = "sdq_peer", 
                            outcome_vars = sdq_peer.vars, 
                            age_vars = sdq_age_rev.vars, 
                            zscore = TRUE)

## ---- SDQ conduct ------------------------------------------------------------
sdq_con.data <- mhIneqLong(data = mh_ineq.data, 
	                       outcome = "sdq_con", 
                           outcome_vars = sdq_con.vars, 
                           age_vars = sdq_age_rev.vars, 
                            zscore = TRUE)

## ---- SDQ hyperactivity ------------------------------------------------------
sdq_hyp.data <- mhIneqLong(data = mh_ineq.data, 
	                       outcome = "sdq_hyp", 
                           outcome_vars = sdq_hyp.vars, 
                           age_vars = sdq_age_rev.vars, 
                            zscore = TRUE)

## ---- Maternal EPDS ----------------------------------------------------------
epds_mat.data <- mhIneqLong(data = mh_ineq.data, 
	                        outcome = "epds_mat", 
                            outcome_vars = epds_mat.vars, 
                            age_vars = epds_mat_age_rev.vars, 
                            zscore = TRUE)

## ---- Paternal EPDS ----------------------------------------------------------
epds_pat.data <- mhIneqLong(data = mh_ineq.data, 
	                        outcome = "epds_pat", 
                            outcome_vars = epds_pat.vars, 
                            age_vars = epds_pat_age_rev.vars, 
                            zscore = TRUE)

## ---- DAWBA ADHD -------------------------------------------------------------
dawba_adhd.data <- mhIneqLong(data = mh_ineq.data, 
	                          outcome = "dawba_adhd", 
                              outcome_vars = dawba_adhd_bin_rev.vars, 
                              age_vars = dawba_age.vars)

## ---- DAWBA conduct ----------------------------------------------------------
dawba_con.data <- mhIneqLong(data = mh_ineq.data, 
	                         outcome = "dawba_con", 
                             outcome_vars = dawba_con_bin_rev.vars, 
                             age_vars = dawba_age.vars)

## ---- DAWBA ODD --------------------------------------------------------------
dawba_odd.data <- mhIneqLong(data = mh_ineq.data, 
	                         outcome = "dawba_odd", 
                             outcome_vars = dawba_odd_bin_rev.vars, 
                             age_vars = dawba_age.vars)

## ---- DAWBA behavioural ------------------------------------------------------
dawba_beh.data <- mhIneqLong(data = mh_ineq.data, 
	                         outcome = "dawba_beh", 
                             outcome_vars = dawba_beh_bin_rev.vars, 
                             age_vars = dawba_age.vars)

## ---- DAWBA depression -------------------------------------------------------
dawba_dep.data <- mhIneqLong(data = mh_ineq.data, 
	                         outcome = "dawba_dep", 
                             outcome_vars = dawba_dep_bin_rev.vars, 
                             age_vars = dawba_age.vars)

## ---- DAWBA anxiety ----------------------------------------------------------
dawba_anx.data <- mhIneqLong(data = mh_ineq.data, 
	                         outcome = "dawba_anx", 
                             outcome_vars = dawba_anx_bin_rev.vars, 
                             age_vars = dawba_age.vars[1:3])


## ---- PLIKS ------------------------------------------------------------------
pliks.data <- mhIneqLong(data = mh_ineq.data, 
                         outcome = "pliks", 
                         outcome_vars = pliks_rev.vars, 
                         age_vars = pliks_age_rev.vars)


## ---- SCDC -------------------------------------------------------------------
asc.data <- mhIneqLong(data = mh_ineq.data, 
                       outcome = "asc", 
                       outcome_vars = asc_rev.vars, 
                       age_vars = asc_age_rev.vars)

################################################################################
# 7. QC  
################################################################################

## ---- MFQ --------------------------------------------------------------------

## QC descriptives
mlmQC(long_data = mfq.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "mfq")

## Apply QC
mfq_qc.data <- mlmQC(long_data = mfq.data,
	                 type = "delete",
	                 age = "age",
	                 occasion = "occasion",
	                 outcome = "mfq")

## Check QC
mlmQC(long_data = mfq_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "mfq")

save(mfq_qc.data, file = paste0(data_path, "mfq_qc.RData"))

## ---- SDQ emotional ----------------------------------------------------------

## Display descriptives
mlmQC(long_data = sdq_emo.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "sdq_emo")

## Apply QC
sdq_emo_qc.data <- mlmQC(long_data = sdq_emo.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "sdq_emo")

## Check QC
mlmQC(long_data = sdq_emo_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "sdq_emo")

save(sdq_emo_qc.data, file = paste0(data_path, "sdq_emo_qc.RData"))

## ---- SDQ peer ---------------------------------------------------------------

## QC descriptives
mlmQC(long_data = sdq_peer.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "sdq_peer")

## Apply QC
sdq_peer_qc.data <- mlmQC(long_data = sdq_peer.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "sdq_peer")

## Check QC
mlmQC(long_data = sdq_peer_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "sdq_peer")

save(sdq_peer_qc.data, file = paste0(data_path, "sdq_peer_qc.RData"))


## ---- SDQ conduct ------------------------------------------------------------

## QC descriptives
mlmQC(long_data = sdq_con.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "sdq_con")

## Apply QC
sdq_con_qc.data <- mlmQC(long_data = sdq_con.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "sdq_con")

## Check QC
mlmQC(long_data = sdq_con_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "sdq_con")

save(sdq_con_qc.data, file = paste0(data_path, "sdq_con_qc.RData"))


## ---- SDQ hyperactivity ------------------------------------------------------

## QC descriptives
mlmQC(long_data = sdq_hyp.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "sdq_hyp")

## Apply QC
sdq_hyp_qc.data <- mlmQC(long_data = sdq_hyp.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "sdq_hyp")

## Check QC
mlmQC(long_data = sdq_hyp_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "sdq_hyp")

save(sdq_hyp_qc.data, file = paste0(data_path, "sdq_hyp_qc.RData"))


## ---- DAWBA ADHD -------------------------------------------------------------

## QC descriptives
mlmQC(long_data = dawba_adhd.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_adhd")

## Apply QC
dawba_adhd_qc.data <- mlmQC(long_data = dawba_adhd.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "dawba_adhd")

## Check QC
mlmQC(long_data = dawba_adhd_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_adhd")

save(dawba_adhd_qc.data, file = paste0(data_path, "dawba_adhd_qc.RData"))


## ---- DAWBA conduct ----------------------------------------------------------

## QC descriptives
mlmQC(long_data = dawba_con.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_con")

## Apply QC
dawba_con_qc.data <- mlmQC(long_data = dawba_con.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "dawba_con")

## Check QC
mlmQC(long_data = dawba_con_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_con")

save(dawba_con_qc.data, file = paste0(data_path, "dawba_con_qc.RData"))


## ---- DAWBA ODD --------------------------------------------------------------

## QC descriptives
mlmQC(long_data = dawba_odd.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_odd")

## Apply QC
dawba_odd_qc.data <- mlmQC(long_data = dawba_odd.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "dawba_odd")

## Check QC
mlmQC(long_data = dawba_odd_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_odd")

save(dawba_odd_qc.data, file = paste0(data_path, "dawba_odd_qc.RData"))


## ---- DAWBA behavioural ------------------------------------------------------

## QC descriptives
mlmQC(long_data = dawba_beh.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_beh")

## Apply QC
dawba_beh_qc.data <- mlmQC(long_data = dawba_beh.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "dawba_beh")

## Check QC
mlmQC(long_data = dawba_beh_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_beh")

save(dawba_beh_qc.data, file = paste0(data_path, "dawba_beh_qc.RData"))


## ---- DAWBA depression -------------------------------------------------------

## QC descriptives
mlmQC(long_data = dawba_dep.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_dep")

## Apply QC
dawba_dep_qc.data <- mlmQC(long_data = dawba_dep.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "dawba_dep")

## Check QC
mlmQC(long_data = dawba_dep_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_dep")

save(dawba_dep_qc.data, file = paste0(data_path, "dawba_dep_qc.RData"))


## ---- DAWBA anxiety ----------------------------------------------------------

## QC descriptives
mlmQC(long_data = dawba_anx.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_anx")

## Apply QC
dawba_anx_qc.data <- mlmQC(long_data = dawba_anx.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "dawba_anx")

## Check QC
mlmQC(long_data = dawba_anx_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "dawba_anx")

save(dawba_anx_qc.data, file = paste0(data_path, "dawba_anx_qc.RData"))


## ---- PLIKS ------------------------------------------------------------------
## QC descriptives
mlmQC(long_data = pliks.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "pliks")

## Apply QC
pliks_qc.data <- mlmQC(long_data = pliks.data,
	                   type = "delete",
	                   age = "age",
	                   occasion = "occasion",
	                   outcome = "pliks")

## Check QC
mlmQC(long_data = pliks_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "pliks")

save(pliks_qc.data, file = paste0(data_path, "pliks_qc.RData"))



## ---- Maternal EPDS ----------------------------------------------------------

## QC descriptives
mlmQC(long_data = epds_mat.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "epds_mat")

## Check polynomials
epds_mat.data %>%
select(age : age_3) %>%
Hmisc::describe(.)

# There are some which are infinity - must be due to an age value of 0. We
# can remove this observation.
epds_mat.data %<>%
filter(age != 0)

## Apply QC
epds_mat_qc.data <- mlmQC(long_data = epds_mat.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "epds_mat")

## Check QC
mlmQC(long_data = epds_mat_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "epds_mat")

save(epds_mat_qc.data, 	file = paste0(data_path, "epds_mat_qc.RData"))


## ---- Paternal epds ----------------------------------------------------------

## QC descriptives
mlmQC(long_data = epds_pat.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "epds_pat")

## Check polynomials
epds_pat.data %>%
dplyr::select(age : age_3) %>%
Hmisc::describe(.)

# There are some which are infinity - must be due to an age value of 0. We
# can remove this observation.
epds_pat.data %<>%
filter(age != 0)

## Apply QC
epds_pat_qc.data <- mlmQC(long_data = epds_pat.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "epds_pat")

## Check QC
mlmQC(long_data = epds_pat_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "epds_pat")

save(epds_pat_qc.data, file = paste0(data_path, "epds_pat_qc.RData"))


## ---- ASC --------------------------------------------------------------------

## QC descriptives
mlmQC(long_data = asc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "asc")

## Apply QC
asc_qc.data <- mlmQC(long_data = asc.data,
	                     type = "delete",
	                     age = "age",
	                     occasion = "occasion",
	                     outcome = "asc")

## Check QC
mlmQC(long_data = asc_qc.data,
	  type = "display",
	  age = "age",
	  occasion = "occasion",
	  outcome = "asc")

save(asc_qc.data, file = paste0(data_path, "asc_qc.RData"))

################################################################################
# 8. Save as lists
################################################################################

## ---- Continuous data --------------------------------------------------------
spline_data <- list()

spline_data$data$mfq <- mfq_qc.data
spline_data$data$sdq_emo <- sdq_emo_qc.data
spline_data$data$sdq_peer <- sdq_peer_qc.data
spline_data$data$sdq_con <- sdq_con_qc.data
spline_data$data$sdq_hyp <- sdq_hyp_qc.data
spline_data$data$epds_mat <- epds_mat_qc.data
spline_data$data$epds_pat <- epds_pat_qc.data
spline_data$data$asc <- asc_qc.data

names(spline_data$data) <- c("mfq", "sdq_emo", "sdq_peer", "sdq_con", "sdq_hyp", 
        	                 "epds_mat", "epds_pat", "asc")

save(spline_data, file = paste0(data_path, "spline.RData"))

## ---- Categorical data -------------------------------------------------------
bin_data <- list()

bin_data$data$dawba_adhd <- dawba_adhd_qc.data
bin_data$data$dawba_con <- dawba_con_qc.data
bin_data$data$dawba_odd <- dawba_odd_qc.data
bin_data$data$dawba_dep <- dawba_dep_qc.data
bin_data$data$dawba_anx <- dawba_anx_qc.data
bin_data$data$pliks <- pliks_qc.data

names(bin_data$data) <- c("dawba_adhd", "dawba_con", "dawba_odd", "dawba_dep", 
	                      "dawba_anx", "pliks")

save(bin_data, file = paste0(data_path, "bin.RData"))