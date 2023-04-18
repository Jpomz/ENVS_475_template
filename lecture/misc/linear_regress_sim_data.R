# air quality data for regression lecture
library(dplyr)
library(ggplot2)

airquality

dat <- airquality %>% 
  select(Ozone, Temp) %>%
  filter(Ozone < 100) %>%
  na.omit() 

air_lm <- lm(Ozone ~ Temp, data = dat)
anova(air_lm)
summary(air_lm)

plot(dat)
plot(air_lm)

ggplot(dat, 
       aes(y = Ozone, 
           x = Temp)) +
  geom_point() +
  theme_bw()


set.seed(675)
n = 50
x <- runif(n, min = 20, max = 100)
error <- rnorm(n = n, mean = 0, sd = 0.25)
y = 1.541 + 0.014 * x + error

plot(y ~ x)
r_max = y
light = x

fit <- lm(r_max ~ light)
anova(fit)
summary(fit)

df <- data.frame(light = x, r_max = y)

ggplot(df,
       aes(x = light, y = r_max)) +
  geom_point() +
  theme_bw()

ggplot(df,
       aes(x = light, y = r_max)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_bw()
