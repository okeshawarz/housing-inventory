options(scipen = 9999)
library(tidyverse)

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