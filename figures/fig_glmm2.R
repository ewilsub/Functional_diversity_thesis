# GLMMS PLOTS - TRAIT MODALITIES

# 1. SETUP ----

library(camcorder)
library(dplyr)
library(ggplot2)
library(ggtext)
library(patchwork)

traits <- readRDS('data/trait_composition.RDS')  %>% 
  select(sample, date, BOD1, RESP1, RESP4, FFG3, FFG5)

env <- readRDS('data/env_log.RDS') %>% 
  select(sample, site, cond, bod, no3_no2)

data <- env %>% 
  left_join(traits, by = 'sample')
  
source('scripts/figures/custom_theme_functional_thesis.R')


# 2. RESP1 ----
(resp1.cond <- ggplot(data = data, aes(x = cond, y = RESP1)) +
    geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
    geom_smooth(method = 'glm', size = 2, color = '#9e2a2b', fill = '#9e2a2b') +
    labs(y = 'Respiration: Gills\n(relative abundance)', x = 'log conductivity') +
    scale_y_continuous(limits = c(-0.075, 0.75), breaks = c(0, 0.25, 0.5, 0.75), expand = expansion(add = c(0.05, 0.05))) +
    scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$cond)), 0.05*diff(range(data$cond))))) +
    coord_cartesian(clip = "off") +
    custom_theme %+replace%
    theme(aspect.ratio = 1,
         axis.title.x = element_markdown(size = 15),
         axis.text.x = element_text(size = 15, color = 'black'),
         axis.text.y = element_text(size = 15, color = 'black')))

(resp1.bod <- ggplot(data = data, aes(x = bod, y = RESP1)) +
    geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
    geom_smooth(method = 'glm', size = 2, color = '#9e2a2b', fill = '#9e2a2b') +
    labs(y = '', x = 'log BOD<sub>5</sub>') +
    scale_y_continuous(limits = c(-0.35, 0.75), breaks = c(0, 0.25, 0.5, 0.75), expand = expansion(add = c(0.05, 0.05))) +
    scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$bod)), 0.05*diff(range(data$bod))))) +
    coord_cartesian(clip = "off") +
    custom_theme %+replace%
    theme(aspect.ratio = 1,
          axis.title.x = element_markdown(size = 15),
          axis.text.x = element_text(size = 15, color = 'black'),
          axis.text.y = element_text(size = 15, color = 'black')))

# 3. RESP4 ----
(resp4.cond <- ggplot(data = data, aes(x = cond, y = RESP4)) +
   geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
   geom_smooth(method = 'glm', size = 2, color = '#9e2a2b', fill = '#9e2a2b') +
   labs(y = 'Respiration: Tegument\n(relative abundance)', x = 'log Conductivity') +
   scale_y_continuous(limits = c(0, 1.15), breaks = c(0.00, 0.50, 1.00), expand = expansion(add = c(0.05, 0.05))) +
   scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$cond)), 0.05*diff(range(data$cond))))) +
   coord_cartesian(clip = "off") +
   custom_theme %+replace%
   theme(aspect.ratio = 1,
         axis.title.x = element_markdown(size = 15),
         axis.text.x = element_text(size = 15, color = 'black'),
         axis.text.y = element_text(size = 15, color = 'black')))

(resp4.bod <- ggplot(data = data, aes(x = bod, y = RESP4)) +
    geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
    geom_smooth(method = 'glm', size = 2, color = '#9e2a2b', fill = '#9e2a2b') +
    labs(y = '', x = 'log BOD<sub>5</sub>') +
    scale_y_continuous(limits = c(0, 1.3), breaks = c(0.00, 0.50, 1.00), expand = expansion(add = c(0.05, 0.05))) +
    scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$bod)), 0.05*diff(range(data$bod))))) +
    coord_cartesian(clip = "off") +
    custom_theme %+replace%
    theme(aspect.ratio = 1,
          axis.title.x = element_markdown(size = 15),
          axis.text.x = element_text(size = 15, color = 'black'),
          axis.text.y = element_text(size = 15, color = 'black')))



# 4. FFG3 ----
(ffg3.cond <- ggplot(data = data, aes(x = cond, y = FFG3)) +
    geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
    geom_smooth(method = 'glm', size = 2, color = '#003566', fill = '#003566') +
    labs(y = 'FFG: Gatherer\n(relative abundance)', x = 'log Conductivity') +
    scale_y_continuous(limits = c(-0.25, 1.15), breaks = c(0.00, 0.50, 1.00), expand = expansion(add = c(0.05, 0.05))) +
    scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$cond)), 0.05*diff(range(data$cond))))) +
    coord_cartesian(clip = "off") +
    custom_theme %+replace%
   theme(aspect.ratio = 1,
         axis.title.x = element_markdown(size = 15),
         axis.text.x = element_text(size = 15, color = 'black'),
         axis.text.y = element_text(size = 15, color = 'black')))

(ffg3.bod <- ggplot(data = data, aes(x = bod, y = FFG3)) +
    geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
    geom_smooth(method = 'glm', size = 2, color = '#003566', fill = '#003566') +
    labs(y = '', x = 'log BOD<sub>5</sub>') +
    scale_y_continuous(limits = c(0, 1.4), breaks = c(0.00, 0.50, 1.00), expand = expansion(add = c(0.05, 0.05))) +
    scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$bod)), 0.05*diff(range(data$bod))))) +
    coord_cartesian(clip = "off") +
    custom_theme %+replace%
    theme(aspect.ratio = 1,
          axis.title.x = element_markdown(size = 15),
          axis.text.x = element_text(size = 15, color = 'black'),
          axis.text.y = element_text(size = 15, color = 'black')))

(ffg3.no3_no2 <- ggplot(data = data, aes(x = no3_no2, y = FFG3)) +
    geom_point(pch = 21, size = 3.5, fill = 'grey60', color = 'grey60') +
    geom_smooth(method = 'glm', size = 2, color = '#003566', fill = '#003566') +
    labs(y = '', x = 'log NO<sub>3</sub><sup>-</sup> + NO<sub>2</sub><sup>-</sup>') +
    scale_y_continuous(limits = c(0, 1), breaks = c(0.00, 0.50, 1.00), expand = expansion(add = c(0.05, 0.05))) +
    scale_x_continuous(expand = expansion(add = c(0.05*diff(range(data$no3_no2)), 0.05*diff(range(data$no3_no2))))) +
    coord_cartesian(clip = "off") +
    custom_theme %+replace%
    theme(aspect.ratio = 1,
          axis.title.x = element_markdown(size = 15),
          axis.text.x = element_text(size = 15, color = 'black'),
          axis.text.y = element_text(size = 15, color = 'black')))









gg_record(device = 'pdf', dpi = 320, width = 30, height = 35, units = 'cm')

design <- "
  AB#
  CD#
  EFG
  "


(glmm2_final <- resp1.cond + resp1.bod + resp4.cond + resp4.bod + ffg3.cond + ffg3.bod + ffg3.no3_no2 + plot_layout(design = design))


gg_stop_recording()
ggsave(glmm2_final,file = 'figures/glmms2.png', dpi = 320, width = 30, height = 35, units = 'cm')












