# two way ANOVA data
library(tidyverse)
library(ggpubr)
library(car)

# herbicide treatments
n = 5
y_bar = 75
y_sd = 5



control <- expand_grid(biomass = rnorm(n, y_bar, y_sd),
                       Chemical = "none",
                       Manual = "none")

chem <- expand_grid(biomass = rnorm(n, y_bar,
                              y_sd),
                    Chemical = "herbicide",
                    Manual = "none")

man <- expand_grid(biomass = rnorm(n, y_bar,
                             y_sd),
                   Chemical = "none",
                   Manual = "chainsaw")

chem_man <- expand_grid(biomass = rnorm(n,
                                  y_bar,
                                  y_sd),
                        Chemical = "herbicide",
                        Manual = "chainsaw")

d1 <- bind_rows(control, chem, man, chem_man)

ggline(d1, 
       x = "Manual",
       y = "biomass",
       color = "Chemical", 
       add = "mean_se",
       size = 1,
       ylim = c(50, 80),
       palette = c("#00AFBB",
                   "#E7B800"))

a1 <- summary(aov(biomass ~ Manual * Chemical, data = d1))



control <- expand_grid(biomass = rnorm(n, y_bar, y_sd),
                       Chemical = "none",
                       Manual = "none")

chem <- expand_grid(biomass = rnorm(n, y_bar-15,
                                    y_sd),
                    Chemical = "herbicide",
                    Manual = "none")

man <- expand_grid(biomass = rnorm(n, y_bar,
                                   y_sd),
                   Chemical = "none",
                   Manual = "chainsaw")

chem_man <- expand_grid(biomass = rnorm(n,
                                        y_bar-15,
                                        y_sd),
                        Chemical = "herbicide",
                        Manual = "chainsaw")

d2 <- bind_rows(control, chem, man, chem_man)

ggline(d2, 
       x = "Manual",
       y = "biomass",
       color = "Chemical", 
       add = "mean_se",
       size = 1,
       ylim = c(50, 80),
       palette = c("#00AFBB",
                   "#E7B800"))

a2 <- summary(aov(biomass ~ Manual * Chemical, data = d2))

control <- expand_grid(biomass = rnorm(n, y_bar, y_sd),
                       Chemical = "none",
                       Manual = "none")

chem <- expand_grid(biomass = rnorm(n, y_bar-15,
                                    y_sd),
                    Chemical = "herbicide",
                    Manual = "none")

man <- expand_grid(biomass = rnorm(n, y_bar - 10,
                                   y_sd),
                   Chemical = "none",
                   Manual = "chainsaw")

chem_man <- expand_grid(biomass = rnorm(n,
                                        y_bar-25,
                                        y_sd),
                        Chemical = "herbicide",
                        Manual = "chainsaw")

d3 <- bind_rows(control, chem, man, chem_man)

ggline(d3, 
       x = "Manual",
       y = "biomass",
       color = "Chemical", 
       add = "mean_se",
       size = 1,
       ylim = c(50, 80),
       palette = c("#00AFBB",
                   "#E7B800"))

a3 <- summary(aov(biomass ~ Manual * Chemical, data = d3))


control <- expand_grid(biomass = rnorm(n, y_bar, y_sd),
                       Chemical = "none",
                       Manual = "none")

chem <- expand_grid(biomass = rnorm(n, y_bar-6,
                                    y_sd),
                    Chemical = "herbicide",
                    Manual = "none")

man <- expand_grid(biomass = rnorm(n, y_bar - 5,
                                   y_sd),
                   Chemical = "none",
                   Manual = "chainsaw")

chem_man <- expand_grid(biomass = rnorm(n,
                                        y_bar-25,
                                        y_sd),
                        Chemical = "herbicide",
                        Manual = "chainsaw")

d4 <- bind_rows(control, chem, man, chem_man)

ggline(d4, 
       x = "Manual",
       y = "biomass",
       color = "Chemical", 
       add = "mean_se",
       size = 1,
       ylim = c(45, 80),
       palette = c("#00AFBB",
                   "#E7B800"))

a4 <- summary(aov(biomass ~ Manual * Chemical, data = d4))

anova(lm(biomass ~ Manual * Chemical, data = d4))
