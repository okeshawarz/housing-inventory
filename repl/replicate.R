options(scipen = 9999)
library(tidyverse)
library(nlme)
setwd("../")

# Data Cleaning
sf <- read.csv("combined.csv", blank.lines.skip = TRUE)
sf_subset <- sf %>% 
    filter(year > 1974) %>%
    filter(year < 2015)

# Adjust median rent and total wages to 1984,
# calculate cpi-adjusted per capita wages
sf_subset <- sf_subset %>%
    mutate(real_medrent = median_rent / CPI,
            real_totalwage = total_wages / CPI,
            real_annwage = real_totalwage / employment)

# Replicate the "simple" model, which is rent as
# a function of per capita wages and employment:

rentmodel_simple <- lm(log10(real_medrent) ~ log10(real_annwage) +
    log10(employment), data =
    sf_subset)
summary(rentmodel_simple)

# The simple model (rent as a function of annual per capita wage
# and the employment rate) estimates a significant and positive effect
# of both variables.

simple_regplot1 <- ggplot(data = sf_subset, aes(log(real_annwage),
    log(real_medrent))) +
    geom_point() +
    geom_smooth(method = 'lm')

simple_regplot2 <- ggplot(data = sf_subset, aes(log(employment),
    log(real_medrent))) +
    geom_point() +
    geom_smooth(method = 'lm')

## Replicate the 'housing inventory' model
rm_inv <- lm(log(real_medrent) ~ log(employment) +
    log(real_annwage) + log(housing_units), data = sf_subset)
summary(rm_inv)

# This clearly shows a less significant but large effect of 
# housing supply on rent

invplot <- ggplot(data = sf_subset, aes(log(housing_units),
    log(real_medrent))) +
    geom_point() +
    geom_smooth(method = 'lm') 

# Estimate the 'full' model (rent = employment + wage + housing) via REML
# (restricted maximum likelihood)

rentmodel_full <- gls(log(real_medrent) ~ log(employment) + log(real_annwage) +
                          log(housing_units) + year, data = sf_subset)

# Visually test for autocorrelation

E <- residuals(rentmodel_full, type = "normalized")
acf(E, na.action = na.pass, main = "Auto-correlation plot for residuals")

# Note that there is a cyclical pattern in the ACF, and some of the lags
# exceed the 'generally accepted" (p > 0.05???) significance threshold. 
# This is evidence of auto-correlation, and violates the independence
# assumption. This makes sense, bc today's rent has got to be a function 
# of yesterday's rent. 

## Add time auto-correlation to the model

rentmodel_ar1 <- gls(log(real_medrent) ~
                         log(employment) + log(real_annwage) +
                         log(housing_units) + year, data = sf_subset,
                         method = "REML",
                    correlation = corAR1(form =~ year))
anova(rentmodel_full, rentmodel_ar1)

# The AR1 model significantly improves the AIC and and increases the fit (?)

## Estimating the Optimal Model

rentmodel_ar1_full <- gls(log(real_medrent) ~ log(employment) +
                              log(real_annwage) + log(housing_units) +
                              year, data = sf_subset, method = "REML")

summary(rentmodel_ar1_full)

diffplot.df <- as.data.frame(cbind(sf_subset$year, diff(log(sf_subset$employment)), 
                     diff(log(sf_subset$housing_units)), 
                     diff(log(sf_subset$real_medrent)),
                     diff(log(sf_subset$real_annwage))))
names(diffplot.df) <- c("year", "emp", "hu", "medrent", "wage")

# plots
ggplot(data = diffplot.df) +
    geom_line(aes(year, emp)) +
    geom_line(aes(year, hu), col = "red") +
    geom_line(aes(year, medrent), col = "blue")

              