################################################################################
## Project: wp6-traj-ineq
## Script purpose: Describe available data
## Date: 30th September 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################


################################################################################
# 1. Load workspace  
################################################################################
opals <- datashield.login(logindata, restore = "traj_assigned_22_10_20")


################################################################################
# 2. Visualise data using DS scatter plots  
################################################################################
ds.scatterPlot(
  x = "sdq_sub$ext_age_", 
  y = "sdq_sub$ext_raw_", 
  datasources = opals[sdq_opals])

ds.scatterPlot(
  x = "cbcl_sub$ext_age_", 
  y = "cbcl_sub$ext_raw_", 
  datasources = opals[cbcl_opals])

ds.scatterPlot(
  x = "mhrep$ext_age_", 
  y = "mhrep$ext_pc_")


## >1 time points SDQ = 
## >1 time points CBCL = 

################################################################################
# 3. Visualise data using my method  
################################################################################
inma_bins <- c()
moba_bins <- c()
dnbc_bins <- c()
chop_bins <- c()






################################################################################
# 2. First we visualise just with MoBa  
################################################################################
ds.scatterPlot(
  x = "cbcl_ext_rep$ext_age_", 
  y = "cbcl_ext_rep$ext_raw_")
  
ds.table("cbcl_ext_rep$n_repeated_obs")

## Also using my function
dh.binRM(
  df = "cbcl_ext", 
  outcome = "ext_raw_", 
  age_var = "ext_age_", 
  upper = 0,
  lower = 17,
  increment = 2
)


h1

## I think the counts



################################################################################
# 3. Now run a really simple model  
################################################################################
ds.asInteger("cbcl_ext_rep$child_id", "child_id_int")

ds.dataFrame(
  x = c("cbcl_ext_rep", "child_id_int"), 
  newobj = "cbcl_ext_rep"
)

ds.lmerSLMA(
  dataName = "cbcl_ext_rep",
  formula = "ext_raw_~ ext_age_ + (1|child_id_int)"
)


################################################################################
# 4. Get predicted values  
################################################################################


ds.summary("cbcl_sub$ext_age_")



################################################################################
# 2. Use ds scatter function  
################################################################################
sdq_coh <- c("ninfea", "chop", "inma", "dnbc")
cbcl_coh <- c("chop", "raine", "moba", "inma")

## ---- SDQ -------------------------------------------------------------------
ds.scatterPlot(
  x = "sdq_sub$int_age_", 
  y = "sdq_sub$int_raw_", 
  datasources = opals[sdq_coh])

ds.scatterPlot(
  x = "cbcl_sub$int_age_", 
  y = "cbcl_sub$int_raw_", 
  datasources = opals[cbcl_coh])

test <- ds.scatterPlot(
  x = "cbcl_sub$int_age_", 
  y = "cbcl_sub$int_raw_")

ds.summary("cbcl_sub$adhd_age")

str(test)

ds.scatterPlot(
  x = "mhrep$int_age_", 
  y = "mhrep$int_pc_", 
  method = "probabilistic")


################################################################################
# 3. Write function to show average value within age bins  
################################################################################

## ---- Function to get data ---------------------------------------------------
dh.binRM <- function(df, outcome_var, age_var, upper, lower, increment){
  
  dh.makeOutcome(
    df = df, 
    outcome = outcome_var, 
    age_var = age_var, 
    bands = c(lower, rep(seq(lower+increment, upper-increment, increment), each = 2), upper), 
    mult_action = "earliest")
  
  outvar <- paste0(outcome_var, "_derived")
  
  ds.dataFrameFill(outvar, "filled")
  
  stats <- dh.getStats(
    df = "filled", 
    vars = ds.colnames("filled")[[1]])
  
  to_plot <- separate(
    data = stats$continuous, 
    col = "variable",
    into = c("type", "age"), 
    sep = "\\.") %>%
    mutate(age = as.numeric(age))
  
  return(to_plot)
  
}
 

## ---- Function to plot data --------------------------------------------------
dh.plotBin <- function(obj, outcome, title, x_label, y_label, x_range, x_inc, y_range, y_inc){
  
require(ggplot2)

## Create theme 
theme_khl <- theme(
  plot.background = element_rect(fill =scales::alpha("#CCCCCC", 0.3)),  #Background outside of plot
  panel.background = element_rect(fill="white"),  #Background for plot
  panel.grid.major=element_line(colour="grey"), #Major and minor gridlines
  panel.grid.minor=element_line(colour="white"), 
  panel.spacing = unit(1, "lines"),
  plot.title = element_text(hjust = 0, vjust=0, size=11, face="bold"), #Plot title, thought don't tend to use
  text=element_text(size=10), #General text 
  axis.title.y = element_text(size=11, margin = margin(t = 0, r = 10, b = 0, l = 0)), #Axis labels
  axis.title.x = element_text(size=11, margin = margin(t = 10, r = 0, b = 0, l = 0)),
  axis.text.x = element_text(size=8, margin = margin(t = 4, r=0, b=0, l=0), colour="black"), #Axis text
  axis.text.y = element_text(size=8, margin = margin(t = 0, r=4, b=0, l=0), colour="black"),
  axis.ticks.length=unit(0.3, "cm"),
  axis.ticks = element_line(colour = "grey"),
  strip.text.x = element_text(size=11),
  strip.background = element_blank(),
  legend.background= element_rect(fill=scales::alpha("#CCCCCC", 0.03)), #Legend background colour
  legend.title=element_text(size=8, face="bold"), #Legend title
  legend.text=element_text(size=8), #Legend text
  legend.position="top", #Legend position
  legend.direction="vertical", #Legend stacking
  legend.justification = "left", #Left justify legend
  legend.key.width = unit(3, "line"), #Make amount of line displayed in legend longer
  legend.margin=margin(t=-1, r=0, b=0.2, l=0, unit="cm"), #Margin around legend
  plot.margin=unit(c(0.5,0.5,0.5,0.5),"cm")) #Margin around (everything) in the plot
  

## Create palette 
palette_khl <- c("#197d78", "#d79632", "#325573", "#d74632", "#d77896", 
                   "#827d78", "#4by6be", "#1e8c32")
  
## Create plot 
  
plot <- ggplot(
  data = obj %>% filter(type == outcome & cohort != "Combined"), aes(x = age, y = mean)) +
  geom_point(aes(size = valid_n)) +
  facet_wrap(vars(cohort), ncol = 1) +
  theme_khl +
  labs(title = title, y = y_label, x = x_label) +
  scale_x_continuous(limit = x_range, breaks = seq(x_range[1], x_range[2], x_inc), expand = c(0, 0)) + 
  scale_y_continuous(limit = y_range, breaks = seq(y_range[1], y_range[2], y_inc), expand = c(0, 0)) +
  scale_size_continuous(range = c(0.5, 2)) 
  

} 
  
  prelim <- ggplot(
    data = to_plot %>% filter(type == outvar & cohort != "Combined"), aes(x = age, y = mean)) +
    geom_point(aes(size = valid_n)) +
    facet_wrap(vars(cohort), ncol = 1) +
    theme_khl +
    labs(title = "test", y = "Internalising", x = "Age") +
    scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 1)) + 
    scale_y_continuous(limit = c(0, 4), breaks = seq(0, 4, 1)) +
    scale_size_continuous(range = c(0.5, 2))  
  

}
  
  
  
  
  
  
  
  

dh.makeOutcome(
  df = "sdq_sub", 
  outcome = "int_raw_", 
  age_var = "int_age_", 
  bands = c(0, rep(seq(2, 17, 2), each = 2), 18), 
  mult_action = "earliest")

ds.dataFrameFill("int_raw__derived", "test")

practice <- dh.getStats(
  df = "test", 
  vars = ds.colnames("test")[[1]])

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
  scale_x_continuous(limit = c(0, 18), breaks = seq(0, 18, 1)) + 
  scale_y_continuous(limit = c(0, 4), breaks = seq(0, 4, 1)) +
  scale_size_continuous(range = c(0.5, 2))



################################################################################
# Show available data based on percentiles  
################################################################################
ds.summary("mhrep$int_pc")

ds.summary("mhrep")

ds.scatterPlot(
  x = "sdq_sub$int_age_", 
  y = "sdq_sub$int_pc_", 
  datasources = opals[c("ninfea", "chop", "raine", "moba", "inma", "dnbc")])

################################################################################
# 4. Save workspace  
################################################################################
datashield.workspace_save(opals, "traj_show_data_30_09_20")
