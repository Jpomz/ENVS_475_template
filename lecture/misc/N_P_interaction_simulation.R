# N:P 2-way interaction data

library(dplyr)
library(ggplot2)

no_n_mean <- 10.8
plus_n_mean <- 15.3
plus_p_mean <- 16
plus_both_mean <- 24.3

no_n_sd <- 1.5
plus_n_sd <- 1.15
plus_p_sd <- 1.26
plus_both_sd <- 1

data.frame(no_)


means <- c(8.8, 13.3, 16, 29.3)
sds <- c(2.5, 2.15, 2.26, 2)

trts <- expand_grid(
  N = c("no_N", "plus_N"),
  P = c("no_P", "plus_P"))

trts$means <- means
trts$sds <- sds

set.seed(884)
(dat_out <- trts %>%
  group_by(N, P) %>%
  mutate(yield = list(round(rnorm(10, mean = means, sd = sds), 3))) %>%
  unnest(cols = yield) %>%
  ungroup() %>%
  select(-means, -sds))

ggplot(dat_out, 
       aes(x = N, 
           y = yield, 
           group = P,
           color = P)) +
  geom_point() +
  stat_summary(fun = mean,                         geom = "line") +
  theme_bw()

with(data = dat_out, 
     interaction.plot(N, P, yield))
