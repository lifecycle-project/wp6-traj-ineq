################################################################################
## Project: Spline plotting
## Script purpose: Getting predicted values and plotting with ggplot
## Date: 8th July 2020
## Author: Tim Cadman
## Email: t.cadman@bristol.ac.uk
################################################################################

library(ggplot2)
library(dplyr)
library(Cairo)
library(magrittr)

setwd("C:/Users/Tim Cadman/OneDrive - University of Bristol/repos")
load("lmermodel.RData")

################################################################################
# 1. Get predicted values and add variables for age and splines  
################################################################################

## So you need to add the argument "re.form = NA": this ignores the random
## effects and just plots the fixed effects components of the model
predicted_vals <- tibble(
	sdq = m7@frame$sdq,
	predicted = predict(m7, re.form = NA),
	s7 = m7@frame$s7, 
	s8 = m7@frame$s8, 
	age = s7 + s8)

################################################################################
# 2. Create a theme
################################################################################
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
    legend.position="top", #Legend position
    legend.direction="vertical", #Legend stacking
    legend.justification = "left", #Left justify legend
    legend.key.width = unit(3, "line"), #Make amount of line displayed in legend longer
    legend.margin=margin(t=0.2, r=0, b=0.2, l=0, unit="cm"), #Margin around legend
    plot.margin = unit(c(0.5, 0.5, 0.2, 0.5),"cm"))


################################################################################
# 3. Use Cairo as your default graphics device  
################################################################################

## This renders plots much better than the default R device
dev.off()
options(device="CairoWin")


################################################################################
# 4. Make plots  
################################################################################

## ---- A basic plot -----------------------------------------------------------

# We use "sample_frac" to only plot a random 10% of values to make graphs render
# more easily
plot_1 <- ggplot(data = sample_frac(predicted_vals, 0.1), 
	aes(x = age, y = predicted)) + 
geom_line(size=0.4) +
labs(title = "SDQ Spline plot", x = "Age (years)", y = "SDQ score") +
theme_traj +
scale_x_continuous(limit = c(0, 27), expand = c(0, 0)) +	              
scale_y_continuous(limit = c(0, 4), expand = c(0, 0))

plot_1


## ---- Add a point to show where the knot point is ----------------------------
knot <- predicted_vals %>%
filter(age == 14) %>%
head(1)

plot_2 <- ggplot(data = sample_frac(predicted_vals, 0.1), 
	aes(x = age, y = predicted)) + 
geom_line(size=0.4) +
labs(title = "SDQ Spline plot", x = "Age (years)", y = "SDQ score") +
theme_traj +
scale_x_continuous(limit = c(0, 27), expand = c(0, 0)) +	              
scale_y_continuous(limit = c(0, 4), expand = c(0, 0)) + 
geom_point(data = knot, (aes(x = age, y = predicted)))

plot_2


## ---- Overlay SDQ scores with splines ----------------------------------------
plot_3 <- ggplot(data = sample_frac(predicted_vals, 0.1), 
	aes(x = age, y = predicted)) + 
geom_line(size=0.4) +
geom_point(data = sample_frac(predicted_vals, 0.05), 
	aes(x = age, y = sdq), alpha = 0.05) +
labs(title = "SDQ Spline plot", x = "Age (years)", y = "SDQ score") +
theme_traj +
scale_x_continuous(limit = c(0, 27), expand = c(0, 0)) +	              
scale_y_continuous(limit = c(0, 4), expand = c(0, 0)) + 
geom_point(data = knot, (aes(x = age, y = predicted)))

plot_3


## ---- Maybe we want to make our line coloured --------------------------------
plot_4 <- ggplot(data = sample_frac(predicted_vals, 0.1), 
	aes(x = age, y = predicted)) + 
geom_line(size=0.4, colour = "#197d78") +
geom_point(data = sample_frac(predicted_vals, 0.05), 
	aes(x = age, y = sdq), alpha = 0.05) +
labs(title = "SDQ Spline plot", x = "Age (years)", y = "SDQ score") +
theme_traj +
scale_x_continuous(limit = c(0, 27), expand = c(0, 0)) +	              
scale_y_continuous(limit = c(0, 4), expand = c(0, 0)) + 
geom_point(data = knot, (aes(x = age, y = predicted)))

plot_4


## ---- Or perhaps use different colours for each spline -----------------------
predicted_vals %<>%
mutate(spline = factor(ifelse(age < 14, "spline1", "spline2")))

plot_5 <- ggplot() + 
geom_line(
	data = sample_frac(predicted_vals, 0.1) %>% filter(spline == "spline1"),
	aes(x = age, y = predicted), size=0.4, colour = "#197d78") +
geom_line(
	data = sample_frac(predicted_vals, 0.1) %>% filter(spline == "spline2"),
	aes(x = age, y = predicted), size=0.4, colour = "#d79632") +
geom_point(
	data = sample_frac(predicted_vals, 0.05), 
	aes(x = age, y = sdq), alpha = 0.05) +
labs(title = "SDQ Spline plot", x = "Age (years)", y = "SDQ score") +
theme_traj +
scale_x_continuous(limit = c(0, 27), expand = c(0, 0)) +	              
scale_y_continuous(limit = c(0, 4), expand = c(0, 0)) + 
geom_point(data = knot, (aes(x = age, y = predicted)))

plot_5


################################################################################
# 5. Save plots  
################################################################################

## We can also used ggsave and cairo to save better quality versions of the 
## plots

ggsave(
	filename = "plot_5.png", 
	plot = plot_5, 
	dpi = 300, 
	type="cairo-png", 
	h = 12, 
    w = 15.92)