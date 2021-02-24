################################################################################
## Project: mh-traj
## Script purpose: Functions 
## Date: 8th October 2019
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################


require(gridExtra)
require(R2MLwiN)
require(lspline)
require(Cairo)
require(dplyr)

source("./useful-code-r/code/functions/match-lab.R")
source("./useful-code-r/code/themes/useful-code-r-ggtheme-khl.R")

################################################################################
# 1. mlmPredTraj
################################################################################

## Get predicted values from runmlwin object
mlmPredTraj <- function(fit, data, mat_ed = FALSE, centred = FALSE){

if(centred == TRUE){

age_var <- "age_cen"	
} else if(centred == FALSE){

age_var <- "age"	
}

tab <- data.frame(predict(fit))
tab$age <- dplyr::pull(data, age_var)
names(tab)[1] <- "predicted"

if(mat_ed == TRUE){
tab$mat_ed_rank <- factor(data$mat_ed_rank, 
	                            labels = c("CSE/none", "Vocational", "O-level",
	                            	       "A-level", "Degree"))

tab %<>%
filter(!is.na(mat_ed_rank))

return(tab)
} else if(mat_ed == FALSE){

return(tab)	
}
}



################################################################################
# 2. mhIneqLong  
################################################################################

## Wrapper function to select vars and reshape 
mhIneqLong <- function(data, outcome, outcome_vars, 
	other_vars = c("aln", "qlet", "sex", "mat_ed_rank"),
	age_vars, zscore = FALSE){

long <- data %>%
dplyr::select(other_vars, outcome_vars, age_vars)

long <- reshape(data = long,
                varying = list(outcome_vars, age_vars),                                    
                direction = "long",
                v.names = c(outcome, "age"),
                idvar = "id", 
                ids = 1:nrow(long),
                timevar = "occasion", 
                times = age_vars)

long %<>%
mutate(age_m_2 = age ^ -2,
	   age_m_1 = age ^ -1,
	   age_m_0_5 = age ^ -1,
	   age_log = log(age),
	   age_0_5 = age ^ 0.5,
	   age_2 = age ^ 2,
	   age_3 = age ^ 3)

## Sort by ID as this is needed for the binomial models and shouldn't harm
## the other models

long %<>%
arrange(., id)

if(zscore == TRUE){
## Create a z-score version of the outcome
long[, paste0(outcome, "_z")] <- ave(
	long[[outcome]], long[["occasion"]], 
	FUN=scale, center = TRUE, scale = TRUE, na.rm = TRUE)

return(long)
} else

return(long)

}


################################################################################
# 3. occAv  
################################################################################

## Function to calculate mean score per measurement occasion
occAv <- function(data, outcome, type, centred = FALSE){

if(centred == TRUE){

age_var <- "age_cen"

} else if(centred == FALSE){

age_var <- "age"

}

tab <- data.frame(matrix(NA, nrow = length(levels(as.factor(data$occasion))), 
	                         ncol = 2))

if(type == "continuous"){

colnames(tab) <- c("occasion_num", "Mean")

tab$occasion_num <- tapply(dplyr::pull(data, age_var), data$occasion, 
	                       mean, na.rm = TRUE)
tab$Mean <- tapply(dplyr::pull(data, outcome), data$occasion, 
	               mean, na.rm = TRUE)

} else if(type == "categorical"){

prop <- table(data$occasion, dplyr::pull(data, outcome))
colnames(tab) <- c("occasion_num", "proportion")

tab$occasion_num <- tapply(dplyr::pull(data, age_var), 
	                       data$occasion, mean, na.rm = TRUE)
tab$proportion <- prop[, 2] / (prop[, 1] + prop[, 2])

}

tab <- arrange(tab, occasion_num)

return(tab)
}


ggPredProp <- function(data, outcome){

prop.tab <- data.frame(matrix(NA, 
	nrow = length(levels(as.factor(data$occasion))), 
	ncol = 2))

colnames(prop.tab) <- c("occasion_num", "proportion")

prop.tab$occasion_num <- tapply(data$age, data$occasion, mean, na.rm = TRUE)
prop.tab$proportion <- prop[, 2] / (prop[, 1] + prop[, 2])

return(prop.tab)
}

################################################################################
# 4. ggPlotMean   
################################################################################

## Function to plot mean score against measurement occasion
ggPlotOccAv <- function(data, title, xlimits, ylimits){

print(ggplot(data, aes(x = occasion_num, y = data[, 2]))) +
geom_point(size = 1.4) +
theme_khl +
labs(title = title, y = colnames(data)[2]) +
scale_x_continuous(limit = xlimits) + 
scale_y_continuous(limit = ylimits)

}

################################################################################
# 5. ggPlotMeanVsTraj  
################################################################################

## Function to overlay predicted trajectory with mean scores 
ggPlotMeanVsTraj <- function(mean_data, pred_data, title, ylabel, xlimit, ylimit){

print(ggplot()) + 
geom_line(data = pred_data, aes(x = age, y = predicted), size=0.4) +
geom_point(data = mean_data, aes(x = occasion_num, y = Mean), size = 1.2) +
theme_khl +
labs(title = "", x = "Age", y = ylabel) +
scale_x_continuous(limit = xlimit, expand = c(0, 0)) + 
scale_y_continuous(limit = ylimit, expand = c(0, 0))

}


################################################################################
# 6. mlmPolyComb  
################################################################################

## Function to perform every combination of MLM fractional polynomials
mlmPolyComb <- function(data, outcome, type = "Normal"){

## First arrange the data to keep it happy when fitting binomial model
#data <- arrange(data, id)

# Next get all combinations of polynomials. Here we have to make it work
# so model is fit with single polynomials and combinations

## Vector of polynomials
polys <- c("age", "age_m_2", "age_m_1", "age_m_0_5", "age_log", "age_0_5", 
	       "age_2", "age_3")

## Create all combinations of these
comb <- t(combn(combn(polys, 1, paste, collapse=""), 2))

## Create terms of these for model formulas
comb_form <- paste(comb[, 1], "+", comb[, 2])

## Combine these with each of the single polynomials
comb_form <- c(comb_form, polys)

# Finally we need to add the single polynomials to the df with the 
# combinations of polynomials. This is used later on for the table
# of fit statistics
comb <- rbind(comb, cbind(polys, NA))

## Run the models
poly.fit <- list()
converged <- vector()

for(i in 1:length(comb_form)){

if(type == "Normal"){
form <- paste0(outcome, " ~ ", "1 + ", comb_form[i], "+ ", 
	           "(1 + ", comb_form[i], "| id)")

est <- 0

} else if(type == "Binomial"){
form <- paste0("logit(", outcome, ")", " ~ ", "1 + ", comb_form[i], "+ ", 
	           "(1 + ", comb_form[i], "| id)")

est <- 1
}

poly.fit[[i]] <- runMLwiN(Formula = as.formula(form), 
                          estoptions = list(resi.store = TRUE, maxiter = 100,
                          	                EstM = est),
                          data = data, 
                          D = type)

if(poly.fit[[i]]@Converged == FALSE){

poly.fit[[i]] <- NULL
print("Model did not converge, coefficients not stored")
converged[i] <- FALSE

} else if(poly.fit[[i]]@Converged == TRUE){

poly.fit[[i]] <- poly.fit[[i]]
converged[i] <- TRUE

} 

}

## Display how many models removed
removed <- comb_form[which(converged == FALSE)]

if(length(removed) > 0){
cat("The following model(s) were removed due to non-convergence: ", 
	removed, sep = "\n")
} else if(length(removed) == 0){
print("All models converged succesfully")
}

## Remove non-converged models
poly.fit <- poly.fit[which(converged == TRUE)]


## Create table with fit statistics 
fit.tab <- data.frame(matrix(NA, nrow = length(poly.fit), ncol = 4))

colnames(fit.tab) <- c("Polynomial 1", "Polynomial 2", "Deviance", 
	                   "Log likelihood")

fit.tab[, 1:2] <- comb[which(converged == TRUE), ]
fit.tab[, 3] <- sapply(poly.fit, deviance)
fit.tab[, 4] <- sapply(poly.fit, logLik)

## Create a variable ranking the fit 
fit.tab %<>%
mutate(dev_rank = dense_rank(Deviance))

out <- list(poly.fit, fit.tab, data, outcome)

return(out)
}


################################################################################
# 7. ggPolyTraj  
################################################################################

## Function to create list of trajectory plots for each polynomial model
ggPolyTraj <- function(polycomb, xlimit, ylimit, ylabel, 
	                   sub = length(polycomb[[1]]), convert_age = FALSE, 
	                   lab_x, lab_y, data_type = "continuous"){

poly.fit <- polycomb[[1]]
fit.tab <- polycomb[[2]]
data <- polycomb[[3]]
outcome <- polycomb[[4]]

## Sort with best model coming first
poly_order.fit <- poly.fit[order(fit.tab$dev_rank)]

## Subset list to display top n models (based on "sub"). Default is all.
poly_sub.fit <- poly_order.fit[1 : sub]

## Create a table with the mean SDQ scores per measurement occasion 
means <- occAv(data, outcome, type = data_type)

## Create a table with the predicted probabilities for plotting 
preds <- list()

for(i in 1 : length(poly_sub.fit)){

preds[[i]] <- mlmPredTraj(poly_sub.fit[[i]], data, mat_ed = FALSE)

}

if(convert_age == TRUE){

preds <- lapply(preds, transform, age = age / 12)
means$occasion_num <- means$occasion_num / 12

} else 
preds <- preds
means <- means


## Create a table with the polynomial labels from the model along with the 
## correct polynomial names
polys <- c("FP_age", "FP_age_m_2", "FP_age_m_1", "FP_age_m_0_5", "FP_age_log", 
	       "FP_age_0_5", "FP_age_2", "FP_age_3")

correct_names <- data.frame(matrix(NA, nrow = length(polys), ncol = 2))
correct_names[, 1] <- polys
correct_names[, 2] <- c("age", "age^-2", "age^-1", "age^-0.5", "log(age)", 
	                    "age^0.5", "age^2", "age^3")

colnames(correct_names) <- c("name", "label")

## Create a list of plots. We use "sample_frac" to reduce number of points
## plotted. We also lookup the correct polynomial names for the title.
plots <- list()

for(i in 1 : length(poly_sub.fit)){

t1_correct <- matchLab(correct_names$label, correct_names$name,
	                   names(poly_sub.fit[[i]]@FP[2]))
t2_correct <- matchLab(correct_names$label, correct_names$name,
	                   names(poly_sub.fit[[i]]@FP[3]))

title <- paste0(t1_correct, " + ", t2_correct)
dev_lab <- paste0("Deviance = ", round(deviance(poly_sub.fit[[i]]), 2))

plots[[i]] <- ggplot() + 
geom_line(data = sample_frac(preds[[i]], 0.01), 
	      aes(x = age, y = predicted), size=0.4) +
geom_point(data = means, 
	       aes(x = occasion_num, y = means[, 2]), size = 1.2) +
theme_traj +
labs(title = title, x = "Age", y = ylabel) +
scale_x_continuous(limit = xlimit, expand = c(0, 0), 
	               breaks = seq(xlimit[1], xlimit[2], by = 1)) + 
scale_y_continuous(limit = ylimit, expand = c(0, 0)) +
annotate("text", x = lab_x, y = lab_y, label = dev_lab)

}

return(plots)
}


################################################################################
# 8. ggMultiPdf  
################################################################################

## Function to save list of plots as pdf
ggMultiPdf <- function(plot_list, filename){

plots_gr <- marrangeGrob(plot_list, nrow = 2, ncol = 1)

ggsave(filename = filename,
       plot = plots_gr,
       dpi = 60,
       device = cairo_pdf,
       onefile = TRUE)

}


################################################################################
# 9. mlmCompSpline  
################################################################################

## Function to run different spline models (companion to mlmPolyComb)
mlmCompSpline <- function(data, outcome, knots_1, knots_2 = NA, knots_3 = NA, 
	                      knots_4 = NA, maxit = 100, type = "Normal"){

## Create all possible combinations of knot vectors
knots <- list(knots_1, knots_2, knots_3, knots_4)

knot_comb <- expand.grid(knots)

knot_valid <- knot_comb %>% 
select_if(function(x) any(!is.na(x)))

colnames(knot_valid) <- paste0("knot_", seq(1:ncol(knot_valid)))

if(ncol(knot_valid == 1)){

comb_knots <- knot_valid$knot_1

} else if(ncol(knot_valid > 1)){

comb_knots <- apply(knot_valid, 1, paste, collapse = ",")

}

## Start a for loop for running models - we will create the splines each time
## within the loop
splines.fit <- list()
splines_converged <- vector()

spline_stats.tab <- data.frame(matrix(NA, nrow = nrow(knot_valid), ncol = 3))

colnames(spline_stats.tab) <- c("Splines", "Deviance", "Log likelihood")

for(i in 1 : nrow(knot_valid)){

## Vector of splines. Should vary in length depending on number of spline
## arguments given above.
spline_vec <- as.numeric(as.vector(knot_valid[i, ]))

## Create splines
splines <- data.frame(lspline(x = data$age, knots = spline_vec))

## Rename
colnames(splines) <- sapply(1 : ncol(splines), function (x) paste0("s", x))
newdata <- cbind(data, splines)

## Paste together spline names for formula
comb_form <- paste(colnames(splines), collapse = " + ")

if(type == "Normal"){
form <- paste0(outcome, " ~ ", "1 + ", comb_form, "+ ", 
	           "(1 + ", comb_form, "| id)")

est <- 0

} else if(type == "Binomial"){
form <- paste0("logit(", outcome, ")", " ~ ", "1 + ", comb_form, "+ ", 
	           "(1 + ", comb_form, "| id)")

newdata <- arrange(newdata, id)
est <- 1
}


## Run models
splines.fit[[i]] <- runMLwiN(Formula = as.formula(form), 
                             estoptions = list(resi.store = TRUE, 
                             	               maxiter = maxit,
                             	               EstM = est),
                             data = newdata, 
                             D = type)

if(splines.fit[[i]]@Converged == FALSE){

splines.fit[[i]] <- NULL
print("Model did not converge, coefficients not stored")
splines_converged[i] <- FALSE

spline_stats.tab[i, 1:3] <- NA

} else if(splines.fit[[i]]@Converged == TRUE){

splines.fit[[i]] <- splines.fit[[i]]
splines_converged[i] <- TRUE

spline_stats.tab[i, 1] <- paste(spline_vec, collapse = ",")
spline_stats.tab[i, 2] <- deviance(splines.fit[[i]])
spline_stats.tab[i, 3] <- logLik(splines.fit[[i]])

} 

}

## Display how many models removed
splines_removed <- comb_knots[which(splines_converged == FALSE)]
splines_kept <- comb_knots[which(splines_converged == TRUE)]

if(length(splines_removed) > 0){
cat("The following model(s) succesfully converged: ", 
	splines_kept, sep = "\n")

cat("The following model(s) were removed due to non-convergence: ", 
	splines_removed, sep = "\n")

} else if(length(splines_removed) == 0){
print("All models converged succesfully")
}

## Remove non-converged models
splines.fit <- splines.fit[which(splines_converged == TRUE)]
spline_stats.tab <- spline_stats.tab[which(splines_converged == TRUE), ]

## Create a variable ranking the fit 
spline_stats.tab %<>%
mutate(dev_rank = dense_rank(Deviance))

out <- list(splines.fit, spline_stats.tab)

return(out)

}


################################################################################
# 10. ggSplinesTraj 
################################################################################

## Function to create list of trajectory plots for each spline model
ggSplinesTraj <- function(splinescomb, polycomb, xlimit, ylimit, ylabel, 
	                      convert_age = FALSE, lab_x, lab_y, 
	                      data_type = "continuous"){

## ---- Get required objects from input lists ----------------------------------
poly.fit <- polycomb[[1]]
poly_stats.tab <- polycomb[[2]]
splines.fit <- splinescomb[[1]]
splines_stats.tab <- splinescomb[[2]]
orig_data <- polycomb[[3]]
outcome <- polycomb[[4]]


## ---- Mean scores per measurement occasion -----------------------------------
means <- occAv(orig_data, outcome, type = data_type)


## ---- Polynomial data --------------------------------------------------------

## Take the best fitting polynomial model
poly_top.fit <- poly.fit[order(poly_stats.tab$dev_rank)][[1]]

## Create a table with the predicted probabilities
preds_poly <- mlmPredTraj(poly_top.fit, orig_data, mat_ed = FALSE)

## Create a table with the polynomial labels from the model along with the 
## correct polynomial names
polys <- c("FP_age", "FP_age_m_2", "FP_age_m_1", "FP_age_m_0_5", "FP_age_log", 
	       "FP_age_0_5", "FP_age_2", "FP_age_3")

correct_names <- data.frame(matrix(NA, nrow = length(polys), ncol = 2))
correct_names[, 1] <- polys
correct_names[, 2] <- c("age", "age^-2", "age^-1", "age^-0.5", "log(age)", 
	                    "age^0.5", "age^2", "age^3")

colnames(correct_names) <- c("name", "label")


## ---- Spline data ------------------------------------------------------------

## Sort with best model coming first
splines_order.fit <- splines.fit[order(splines_stats.tab$Deviance)]
splines_stats_order.tab <- splines_stats.tab[order(splines_stats.tab$Deviance),]

## Create a table with the predicted probabilities from the spline models.
preds_spline <- list()

for(i in 1 : length(splines_order.fit)){

preds_spline[[i]] <- mlmPredTraj(splines_order.fit[[i]], orig_data, mat_ed = FALSE)

}


## ---- Option for converting months to years ----------------------------------
if(convert_age == TRUE){

preds_spline <- lapply(preds_spline, transform, age = age / 12)
preds_poly$age <- preds_poly$age / 12
means$occasion_num <- means$occasion_num / 12

} else 
preds_spline <- preds_spline
preds_poly <- preds_poly
means <- means

## ---- Plots ------------------------------------------------------------------

# Create a list of plots. There are three main layers: (i) mean score per
# measurement occasion, (ii) best fitting polynomial model, (iii) spline models.

plots <- list()
for(i in 1 : length(splines_order.fit)){

## Get info for title
t1_poly <- matchLab(correct_names$label, correct_names$name, 
	                names(poly_top.fit@FP[2]))
t2_poly <- matchLab(correct_names$label, correct_names$name,
	                names(poly_top.fit@FP[3]))

t_splines <- splines_stats_order.tab$Splines[i]

title <- paste0("Polynomial(s): ", t1_poly, " + ", t2_poly, "\n",
	            "Knot(s): ", t_splines)

dev_lab <- paste0("Polynomial deviance = ", round(deviance(poly_top.fit), 2),
	              "\n",
	              "Spline deviance = ", round(deviance(splines_order.fit[[i]]), 2))

plots[[i]] <- ggplot() + 
geom_point(data = means, 
	       aes(x = occasion_num, y = means[, 2]), size = 1.2) +
geom_line(data = sample_frac(preds_poly, 0.10), 
	      aes(x = age, y = predicted), size = 1.5, colour = "#197d78", 
	          alpha = 0.4) +
geom_line(data = sample_frac(preds_spline[[i]], 0.10), 
	      aes(x = age, y = predicted), size = 0.5) +
theme_traj +
labs(title = title, x = "Age", y = ylabel) +
scale_x_continuous(limit = xlimit, expand = c(0, 0), 
	               breaks = seq(xlimit[1], xlimit[2], by = 1)) + 
scale_y_continuous(limit = ylimit, expand = c(0, 0)) +
annotate("text", x = lab_x, y = lab_y, label = dev_lab)

}

return(plots)
}


################################################################################
# Linear binary model vs proportions at measurement occasion
################################################################################
ggSplineLin <- function(data, fit, outcome, title, ylabel, xlimits, 
	                    ylimits){

pred_lin <- mlmPredTraj(
	fit = fit,
	data = arrange(data, id))

prop_lin <- occAv(
	data = data, 
    outcome = outcome, 
    type = type)


pred_lin[, 1] <- exp(pred_lin[, 1])

plot_out <- ggplot() + 
geom_line(data = pred_lin, 
	      aes(x = age, y = predicted), size=0.4) +
geom_point(data = prop_lin, 
	       aes(x = occasion_num, y = proportion), size = 1.2) +
theme_traj +
labs(title = title, x = "Age", y = ylabel) +
scale_x_continuous(limit = xlimits, expand = c(0, 0)) +	              
scale_y_continuous(limit = ylimits, expand = c(0, 0))

return(plot_out)

}


################################################################################
# Make splines and bind to dataframe  
################################################################################
mkSpline <- function(data, knots, age_var){

splines <- data.frame(lspline(x = dplyr::pull(data, age_var), knots = knots))

cols <- vector()

for(i in 1 : (length(knots) + 1)){

cols[i] <- paste0("s", i)

}

colnames(splines) <- cols

out <- cbind(data, splines)

}


################################################################################
# Manuscript trajectory plot  
################################################################################

ggFigA <- function(data, fit, pred, outcome, title, ylabel, xlimits, 
	               ylimits, centerVal){

palette <- c("#197d78", "#d79632", "#d74632", "#827d78", "#1e8c32")

plot_out <- ggplot() + 
geom_line(data = pred, 
	      aes(x = ((age + centerVal) / 12), 
	      	  y = predicted, 
	      	  color = mat_ed_rank), size=0.4) +
theme_traj +
labs(title = title, x = "", y = ylabel, color = "Maternal education rank") +
scale_x_continuous(limit = xlimits, expand = c(0, 0)) +	              
scale_y_continuous(limit = ylimits, expand = c(0, 0)) +
scale_color_manual(values = palette) + 
theme(legend.position = "none")

return(plot_out)

}


ggFigC <- function(fit, pred, outcome, title, ylabel, xlimits, 
	               ylimits, centerVal){

palette <- c("#197d78", "#d79632", "#d74632", "#827d78", "#1e8c32")

plot_out <- 
ggplot(data = pred, aes(x = ((age_cen + centerVal) / 12), y = predicted, 
	   color = mat_ed_rank)) + 
geom_line(size=0.4) +
theme_traj +
labs(title = title, x = "", y = ylabel, color = "Maternal education rank") +
scale_x_continuous(limit = xlimits, expand = c(0, 0)) +	              
scale_y_continuous(limit = ylimits, expand = c(0, 0)) +
scale_color_manual(values = palette) + 
theme(legend.position = "none")

return(plot_out)

}

################################################################################
# Calculate SII  
################################################################################

mlmStats <- function(data_tmp, fit, outcome, type, centred, knots = vector()){

av_meas <- occAv(data, outcome, type, centred)

pred <- mlmPredTraj(
    data = data,
	fit = fit,
	mat_ed = TRUE,
	centred = centred)

coefs <- fit@FP[grep("mat_ed_rank", names(fit@FP))]

if(length(knots) > 0){
sii.tab <- data.frame(matrix(NA, nrow = length(knots) + 2, ncol = 2))

sii.tab[1, 1] <- head(av_meas$occasion_num, 1)
sii.tab[2 : (length(knots) + 1), 1] <- knots
sii.tab[nrow(sii.tab), 1] <- tail(av_meas$occasion_num, 1)
sii.tab[1, 2] <- coefs[1]

for(i in 2 : nrow(sii.tab)){

sii.tab[i, 2] <- (sii.tab[i, 1] - sii.tab[i - 1, 1]) * coefs[i]

}


} else if(length(knots) == 0){

sii.tab <- data.frame(matrix(NA, nrow = 2, ncol = 2))

sii.tab[1, 1] <- head(av_meas$occasion_num, 1)
sii.tab[2, 1] <- tail(av_meas$occasion_num, 1)
sii.tab[1, 2] <- coefs[1]
sii.tab[2, 2] <- (sii.tab[2, 1] - sii.tab[1, 1]) * coefs[2]

}

colnames(sii.tab) <- c("age", "sii")

out <- list(predicted = pred, sii = sii.tab, av_age = av_meas)

return(out)

}

################################################################################
# Manuscript SII plot  
################################################################################

ggFigB <- function(sii_tab, ylabel, xlimits, ylimits, centerVal){

sii_plot <- ggplot(data = sii_tab, aes(x = ((age + centerVal) / 12), y = sii)) + 
  geom_point(size = 1.4) +
  geom_line(size = 0.7) +
  theme_traj +
  labs(title="", x = "Age (years)", y="SII") +
  scale_x_continuous(limit = xlimits, expand = c(0, 0)) +	              
  scale_y_continuous(limit = ylimits, expand = c(0, 0)) + 
  theme(legend.position = "none")
 
}


