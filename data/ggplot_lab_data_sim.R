# simulate ggplot Spring 2024 lab data

set.seed(136)
dat <- data.frame(month = rep(c(3, 7, 11), 10),
                  taxa = rep(c("beetle", "spider"), each = 15))

count <- c(
   rpois(10, 15),
   rpois(10, 35),
   rpois(10, 20))
 
temp <- c(
  rnorm(10, 10),
  rnorm(10, 25),
  rnorm(10, 20))

body_length <- rnorm(30, 25, 5)
body_mass_mg <- body_length^0.79
body_mass_mg <- body_mass_mg + rnorm(30, 0, 5)
plot(body_length_mm, body_mass_mg)

(dat <- dplyr::bind_cols(dat,
                        #count = count,
                        #temp = temp,
                        body_length_mm = body_length_mm,
                        body_mass_g = body_mass_mg))

readr::write_csv(dat, "data/ggplot_lab_invert_survey.csv")

library(ggplot2)

ggplot(dat, 
       aes(x = body_length))+
  geom_density()

ggplot(dat, 
       aes(x = body_length))+
  geom_histogram()

ggplot(dat, 
       aes(x = body_length))+
  geom_boxplot()


ggplot(dat, 
       aes(x = body_length,
           fill = taxa))+
  geom_density(alpha = 0.5)

ggplot(dat, 
       aes(x = body_length,
           fill = taxa))+
  geom_histogram(position = "identity",
                 alpha = 0.5)

ggplot(dat, 
       aes(x = body_length,
           fill = taxa))+
  geom_boxplot()

ggplot(dat, 
       aes(y = body_length,
           x = as.factor(month),
           fill = taxa))+
  geom_boxplot()

ggplot(dat, 
       aes(x = body_length,
           y = body_mass_mg))+
  geom_point()+
  geom_smooth(method = "lm")

ggplot(dat, 
       aes(x = body_length,
           y = body_mass_mg,
           color = taxa))+
  geom_point()

ggplot(dat, 
       aes(x = body_length,
           y = body_mass_mg,
           color = taxa))+
  geom_point() +
  geom_smooth(method = "lm")

ggplot(dat, 
       aes(x = body_length,
           y = body_mass_mg,
           color = taxa))+
  geom_point() +
  facet_wrap(~month)

ggplot(dat, 
       aes(x = body_length,
           y = body_mass_mg,
           color = month))+
  geom_point()

ggplot(dat, 
       aes(x = body_length,
           y = body_mass_mg,
           color = as.factor(month)))+
  geom_point()

ggplot(dat, 
       aes(x = body_length,
           y = body_mass_mg,
           color = as.factor(month)))+
  geom_point() +
  facet_wrap(~taxa)
