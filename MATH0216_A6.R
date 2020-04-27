## ----include=FALSE---------------------------------------------------------------------------------------------------
library(tidyverse)


## --------------------------------------------------------------------------------------------------------------------
taboo <-read.csv("https://raw.githubusercontent.com/ebmwhite/MATH0216/master/assignments/taboo.csv")


## --------------------------------------------------------------------------------------------------------------------
model <- lm(Happiness ~ Salary, data = taboo)

taboo %>% 
  ggplot(aes(x = Salary, y = Happiness)) +
  geom_point() +
  geom_abline(slope = coef(model)[2],
              intercept = coef(model)[1],
              color = "red")

summary(model)


## --------------------------------------------------------------------------------------------------------------------
model2 <- lm(Happiness ~ Salary + factor(Gender), data = taboo)

summary(model2)

summary(model)$r.squared
summary(model2)$r.squared


## --------------------------------------------------------------------------------------------------------------------
taboo %>% 
  ggplot(aes(x = Sex, y = Happiness, color = Gender)) +
  geom_point()


## --------------------------------------------------------------------------------------------------------------------
model3 <- lm(data = taboo, Happiness ~ Sex + factor(Gender))

summary(model3)


## --------------------------------------------------------------------------------------------------------------------
full_model <- lm(data = taboo, Cigarette ~ Happiness + Sex + Gender + Alcohol + Salary)

step(full_model)

