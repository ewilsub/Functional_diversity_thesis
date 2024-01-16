# GLMMS - TRAIT MODALITIES

# 1. SETUP ----
library(dplyr)
library(ggplot2)
library(MASS)
library(MuMIn)
library(nlme)

traits <- readRDS('data/trait_composition.RDS')
env <- readRDS('data/env_log.RDS')
source('scripts/figures/custom_theme_functional_thesis.R')

data <- left_join(traits, env, by = c('sample', 'date')) %>% 
  dplyr::select(-site, bod, cond, do, nh4, no3_no2, po4, tss)



# 2. RESP1 (Gills) model ----
mod.resp1 <- glmmPQL(RESP1 ~ 
                      #disc + 
                      cond + 
                      #do + 
                      #tss + 
                      bod, 
                      #po4, 
                      #nh4, 
                      #no3_no2,
                    random = ~ 1|date,
                    correlation = corSpatial(form = ~ lat + lon), 
                    family = 'binomial', data = data)

## model summary ----
summary(mod.resp1)
car::vif(mod.resp1) # all good
r.squaredGLMM(mod.resp1)
intervals(mod.resp1, which="fixed")


## residuals ----
# calculating residuals
res <- resid(mod.resp1, type = "pearson")

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
ggplot(data, aes(x = predict(mod.resp1), y = resid(mod.resp1, type = "pearson"))) +
  geom_point(size = 4, pch = 1) +
  geom_hline(yintercept = 0, lty = 2, size = 1) +
  labs(y = 'Residuals', x = 'Predicted') +
  custom_theme


# 3. RESP4 (Tegument) model ----
mod.resp4 <- glmmPQL(RESP4 ~
                       cond + 
                       #do + 
                       #tss + 
                       bod, 
                       #po4,
                       #nh4, 
                       #no3_no2,
                       random = ~ 1|date,
                     correlation = corSpatial(form = ~ lat + lon), 
                     family = 'binomial', data = data)

## model summary ----
summary(mod.resp4)
car::vif(mod.resp4) # all good
r.squaredGLMM(mod.resp4)
intervals(mod.resp4, which="fixed")


## residuals ----
# calculating residuals
res <- resid(mod.resp4, type = "pearson")

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
ggplot(data, aes(x = predict(mod.resp4), y = resid(mod.resp4, type = "pearson"))) +
  geom_point(size = 4, pch = 1) +
  geom_hline(yintercept = 0, lty = 2, size = 1) +
  labs(y = 'Residuals', x = 'Predicted') +
  custom_theme


# 4. FFG3 (Gatherer) model ----
mod.ffg3 <- glmmPQL(FFG3 ~  
                       cond + 
                       #do + 
                       #tss + 
                       bod + 
                       #po4 + 
                       #nh4 +
                       no3_no2, 
                     random = ~ 1|date,
                     correlation = corSpatial(form = ~ lat + lon), 
                     family = 'binomial', data = data)

## model summary ----
summary(mod.ffg3)
car::vif(mod.ffg3) # all good
r.squaredGLMM(mod.ffg3)
intervals(mod.ffg3, which="fixed")


## residuals ----
# calculating residuals
res <- resid(mod.ffg3, type = "pearson")

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
ggplot(data, aes(x = predict(mod.ffg3), y = resid(mod.ffg3, type = "pearson"))) +
  geom_point(size = 4, pch = 1) +
  geom_hline(yintercept = 0, lty = 2, size = 1) +
  labs(y = 'Residuals', x = 'Predicted') +
  custom_theme



