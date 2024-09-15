library(ggplot2)

load("data/work.RData")

names(work)

work$tra_hou_c <- work$tra_hou - mean(work$tra_hou)

fit_qua <- lm(data = work,
              formula = job_per ~ 1 + tra_hou_c + I(tra_hou_c**2) + I(tra_hou_c**3))
fit_lin <- lm(data = work,
              formula = job_per ~ 1 + tra_hou_c)

anova(fit_qua, fit_lin)
summary(fit_qua)

ggplot(data = work, mapping = aes(x = tra_hou_c, y = job_per)) + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3))

fit_qua_uc <- lm(data = work,
              formula = job_per ~ 1 + tra_hou + I(tra_hou**2) + I(tra_hou**3))
summary(fit_qua)
summary(fit_qua_uc)

fit_lin <- lm(data = work,
              formula = job_per ~ 1 + tra_hou)