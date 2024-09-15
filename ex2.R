# Exercise 2 --------------------------------------------------------------
library(emmeans)
library(ggplot2)
library(GWmodel)
library(sp)

# Load dataset ------------------------------------------------------------
load("data/health.RData")
names(health)

health$per_ris_c <- health$per_ris - mean(health$per_ris)

# Exercise 2.1 ------------------------------------------------------------
fit_base <- lm(data = health,
               formula = hea_beh ~ 1 + ins_enr)
summary(fit_base)

fit_lin <- lm(data = health,
              formula = hea_beh ~ 1 + per_ris_c + 
                ins_enr + ins_enr : per_ris_c)
summary(fit_lin)

fit_qua <- lm(data = health,
              formula = hea_beh ~ 1 + per_ris_c + I(per_ris_c^2) + 
                ins_enr + ins_enr : per_ris_c + ins_enr : I(per_ris_c^2))
summary(fit_qua)

anova(fit_lin, fit_qua)

focal_points <- seq(min(health$per_ris_c), max(health$per_ris_c), length.out = 1000)
trend <- emtrends(fit_qua, specs = ~ per_ris_c | ins_enr, var = "ins_enr", at = list(per_ris_c = focal_points)) 

ggplot(data = as.data.frame(trend), mapping = aes(x = per_ris_c, y = ins_enr.trend)) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL), alpha = .3) +
  geom_line() +
  labs(x = "Centered Sleep Quality", y = "Regression Slope")

# Ex 2.2

# Only 1 moderator, so one dimension is set to 0
coordinates <- cbind(
  health$per_ris_c,
  rep(0, nrow(health))
)
head(coordinates)

sp_hea <- SpatialPointsDataFrame(
  coords = coordinates,
  data = health
)

gwr_model <- gwr.basic(hea_beh ~ 1 + ins_enr,
                       data = sp_hea, bw = 1.1, kernel = "gaussian",
                       F123.test = T, parallel.method = "cluster"
)
gwr_model


gwr_linSlo <- gwr.mixed(
  hea_beh ~ ins_enr + ins_enr:per_ris_c,
  data = sp_hea,
  fixed.vars = c("ins_enr", "ins_enr:per_ris_c"),
  intercept.fixed = FALSE,
  bw = 1.1,
  kernel = "gaussian"
)

gwr_quaSlo <- gwr.mixed(
  hea_beh ~ ins_enr + ins_enr:per_ris_c + ins_enr:I(per_ris_c^2),
  data = sp_hea,
  fixed.vars = c("ins_enr", "ins_enr:per_ris_c", "ins_enr:I(per_ris_c^2)"),
  intercept.fixed = FALSE,
  bw = 1.1,
  kernel = "gaussian"
)

# Linear model comparison
n <- 500
RSS0 <- gwr_linSlo$r.ss
RSS1 <- gwr_model$GW.diagnostic$RSS.gw
DF0 <- gwr_linSlo$df.used
DF1 <- gwr_model$GW.diagnostic$edf
F_value <- ((RSS0 - RSS1) / (DF1 - DF0)) / (RSS1 / (n - DF1))
(F.p <- pf(F_value, DF1 - DF0, n - DF1, lower.tail = TRUE))

# Extract coefficients and standard errors from the GWR model
coefficients <- gwr_model$SDF@data$ins_enr
se <- gwr_model$SDF@data$ins_enr_SE

# Create Confidence Intervals (CI)
upper_bound <- coefficients + 1.96 * se
lower_bound <- coefficients - 1.96 * se

# Create a dataframe for plotting
plot_data <- data.frame(
  per_ris_c = health$per_ris_c,
  coefficients = coefficients,
  upper_bound = upper_bound,
  lower_bound = lower_bound
)

library(ggplot2)
ggplot(
  data = plot_data,
  mapping = aes(x = per_ris_c, y = coefficients)
) +
  geom_line(
    color = "blue",
    linewidth = 1
  ) +
  geom_ribbon(
    aes(ymin = lower_bound, ymax = upper_bound),
    alpha = 0.2,
    fill = "blue"
  ) +
  labs(
    x = "Perceived Risk",
    y = "Slope of Health Insurance Enrollment"
  )

