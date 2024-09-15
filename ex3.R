library(lavaan)

load("data/media.RData")

head(media)

model_uvi <- "
  # Measurement Model
  CFraming =~ NA * x1 + x2 + x3
  MTrust =~ NA * y1 + y2 + y3 + y4 + y5
  
  # Structural Model  
  MTrust ~ CFraming
  
  MTrust ~~ 1 * MTrust 
  CFraming ~~ 1 * CFraming 
"

fit_uvi <- sem(model_uvi, media)
summary(fit_uvi, standardized = TRUE, fit.measures = TRUE)

model_uli <- "
  # Measurement Model
  CFraming =~ x1 + x2 + x3
  MTrust =~ y1 + y2 + y3 + y4 + y5
  
  # Structural Model  
  MTrust ~ CFraming
"

fit_uli <- sem(model_uli, media)
summary(fit_uli, standardized = TRUE, fit.measures = TRUE)
