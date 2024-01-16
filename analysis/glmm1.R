# GLMMS - FUNCTIONAL DIVERSITY METRICS

# 1. SETUP ----
library(dplyr)
library(ggplot2)
library(MASS)
library(MuMIn)
library(nlme)

ses <- readRDS('results/ses_results.RDS')
env <- readRDS('data/env_log.RDS')
source('scripts/figures/custom_theme_functional_thesis.R')

data <- left_join(ses, env, by = 'sample') %>% 
  relocate(c(site, date), .after = sample) %>% 
  relocate(c(fric, feve, fdis, fric.ses, feve.ses, fdis.ses), .after = no3_no2)



# 2. FRIC MODEL ----
mod.fric <- glmmPQL(fric.ses ~ 
                      disc + 
                      #cond + 
                      do,
                      #tss +
                      #bod + 
                      #po4 + 
                      #nh4, 
                      #no3_no2,
                      random = ~ 1|date,
                    correlation = corSpatial(form = ~ lat + lon), 
                    family = 'gaussian', data = data)

## model summary ----
summary(mod.fric)
car::vif(mod.fric) # all good
r.squaredGLMM(mod.fric)
intervals(mod.fric, which="fixed") 

## residuals ----
# calculating residuals
res <- resid(mod.fric, type = "pearson")

# generating palette
color <- res
color[res >= 0 ] <- 1  #black
color[res < 0 ]  <- 2  #red

# plotting spatial correlations
lattice::xyplot(lat ~ lon,
       cex =  4 * sqrt(abs(res) / max(abs(res))),  
       pch = 1,
       col = color,
       data = data,
       main = "Spatial plot of residuals")

# plotting residuals vs predicted
ggplot(data, aes(x = predict(mod.fric), y = resid(mod.fric, type = "pearson"))) +
    geom_point(size = 4, pch = 1) +
    geom_hline(yintercept = 0, lty = 2, size = 1) +
    labs(y = 'Residuals', x = 'Predicted') +
    custom_theme


# 3. FEVE MODEL ----
mod.feve <- glmmPQL(feve.ses ~ 
                      #disc + 
                      #cond + 
                      #do + 
                      #tss + 
                      bod, 
                      #po4 + 
                      #nh4, 
                      #no3_no2, 
                      random = ~ 1|date,
                    correlation = corSpatial(form = ~ lat + lon), 
                    family = 'gaussian', data = data)

## model summary ----
summary(mod.feve)
car::vif(mod.feve) # all good
r.squaredGLMM(mod.feve)
intervals(mod.feve, which="fixed") 


## residuals ----
# calculating residuals
res <- resid(mod.feve, type = "pearson")

# generating palette
color <- res
color[res >= 0 ] <- 1  #black
color[res < 0 ]  <- 2  #red

# plotting spatial correlations
lattice::xyplot(lat ~ lon,
                cex =  4 * sqrt(abs(res) / max(abs(res))),  
                pch = 1,
                col = color,
                data = data,
                main = "Spatial plot of residuals")

# plotting residuals vs predicted
ggplot(data, aes(x = predict(mod.feve), y = resid(mod.feve, type = "pearson"))) +
  geom_point(size = 4, pch = 1) +
  geom_hline(yintercept = 0, lty = 2, size = 1) +
  labs(y = 'Residuals', x = 'Predicted') +
  custom_theme


# 4. FDIS MODEL ----
mod.fdis <- glmmPQL(fdis.ses ~ 
                      #disc, 
                      #cond + 
                      #do + 
                      #tss + 
                      #bod + 
                      #po4 + 
                      #nh4, 
                      #no3_no2,
                    random = ~ 1|date,
                    correlation = corSpatial(form = ~ lat + lon), 
                    family = 'gaussian', data = data)

## model summary ----
summary(mod.fdis)
car::vif(mod.fdis) # all good
r.squaredGLMM(mod.fdis)
intervals(mod.fdis, which="fixed") 


## residuals ----
# calculating residuals
res <- resid(mod.fdis, type = "pearson")

# generating palette
color <- res
color[res >= 0 ] <- 1  #black
color[res < 0 ]  <- 2  #red

# plotting spatial correlations
lattice::xyplot(lat ~ lon,
                cex =  4 * sqrt(abs(res) / max(abs(res))),  
                pch = 1,
                col = color,
                data = data,
                main = "Spatial plot of residuals")

# plotting residuals vs predicted
ggplot(data, aes(x = predict(mod.fdis), y = resid(mod.fdis, type = "pearson"))) +
  geom_point(size = 4, pch = 1) +
  geom_hline(yintercept = 0, lty = 2, size = 1) +
  labs(y = 'Residuals', x = 'Predicted') +
  custom_theme
