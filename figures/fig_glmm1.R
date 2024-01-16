# GLMMS PLOTS - FUNCTIONAL METRICS

# 1. SETUP ----
library(camcorder)
library(dplyr)
library(ggplot2)
library(ggtext)
library(patchwork)

ses_results <- readRDS('ses_results.RDS')

env <- readRDS('data/env_log.RDS') %>% 
  select(sample, disc, cond, do, bod)

data <- ses_results %>% 
  select(sample, fric.ses, feve.ses, fdis.ses) %>% 
  left_join(env, by = c('sample'))

source('figures/custom_theme_functional_thesis.R')



# 2. FRIC ----
gg_record(device = 'pdf', dpi = 320, width = 15, height = 16, units = 'cm')

(
  fric.disc <- ggplot(data = data, aes(x = disc, y = fric.ses)) +
    geom_hline(aes(yintercept = -1.96), lty = 'longdash') +
    geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
    geom_smooth(method = 'glm', size = 2, color = '#307837', fill = '#307837') +
    labs(x = 'log Discharge', y = 'Functional richness (SES)',) +
    scale_y_continuous(limits = c(-3.5, 3.5), expand = expansion(add = c(0.05, 0.05))) +
    scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$disc)), 0.05*diff(range(data$disc))))) +
    coord_cartesian(clip = "off") +
    custom_theme %+replace%
    theme(aspect.ratio = 1,
          axis.title.x = element_markdown(size = 15))
)


(
  fric.do <- ggplot(data = data, aes(x = do, y = fric.ses)) +
    geom_hline(aes(yintercept = -1.96), lty = 'longdash') +
    geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
    geom_smooth(method = 'glm', size = 2, color = '#307837', fill = '#307837') +
    labs(x = 'log Dissolved oxygen', y = '',) +
    scale_y_continuous(limits = c(-3.5, 3.5), expand = expansion(add = c(0.05, 0.05))) +
    scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$do)), 0.05*diff(range(data$do))))) +
    coord_cartesian(clip = "off") +
    custom_theme %+replace%
    theme(aspect.ratio = 1,
         axis.title.x = element_markdown(size = 15))
)



# 3. FEVE ----
(
  feve.bod <- ggplot(data = data, aes(x = bod, y = feve.ses)) +
    geom_hline(aes(yintercept = -1.96), lty = 'longdash') +
    geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
    geom_smooth(method = 'glm', size = 2, color = '#353078', fill = '#353078') +
    labs(x = 'log BOD<sub>5</sub>', y = 'Functional evenness (SES)',) +
    scale_y_continuous(limits = c(-4, 3.5), expand = expansion(add = c(0.05, 0.05)), breaks = c(-2, 0, 2)) +
    scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$bod)), 0.05*diff(range(data$bod))))) +
    coord_cartesian(clip = "off") +
    custom_theme %+replace%
    theme(aspect.ratio = 1,
          axis.title.x = element_markdown(size = 15))
)



# 4. FDIS ----
(
  fdis.disc <- ggplot(data = data, aes(x = disc, y = fdis.ses)) +
    geom_hline(aes(yintercept = -1.96), lty = 'longdash') +
    geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
    geom_smooth(method = 'glm', size = 2, color = '#78304f', fill = '#78304f') +
      labs(y = '', x = 'log Discharge', y = 'Functional dispersion (SES)') +
    scale_y_continuous(limits = c(-3.5, 3.5), expand = expansion(add = c(0.05, 0.05))) +
    scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$disc)), 0.05*diff(range(data$disc))))) +
    coord_cartesian(clip = "off") +
    custom_theme %+replace%
    theme(aspect.ratio = 1,
          axis.title.x = element_markdown(size = 15))
)


(
  fdis.bod <- ggplot(data = data, aes(x = bod, y = fdis.ses)) +
    geom_hline(aes(yintercept = -1.96), lty = 'longdash') +
    geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
    geom_smooth(method = 'glm', size = 2, color = '#78304f', fill = '#78304f') +
    labs(x = 'log BOD<sub>5</sub>', y = 'Functional dispersion (SES)',) +
    scale_y_continuous(limits = c(-4, 3.5), expand = expansion(add = c(0.05, 0.05)), breaks = c(-2, 0, 2)) +
    scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$bod)), 0.05*diff(range(data$bod))))) +
    coord_cartesian(clip = "off") +
    custom_theme %+replace%
    theme(aspect.ratio = 1,
          axis.title.x = element_markdown(size = 15))
)





gg_record(device = 'pdf', dpi = 320, width = 30, height = 25, units = 'cm')


design <- "
  12
  3#
"


fric.disc + fric.do + feve.bod + plot_layout(design = design)


gg_stop_recording()
ggsave(file = "glmms1.png", dpi = 320, width = 30, height = 25, units = 'cm')













