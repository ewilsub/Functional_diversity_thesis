# ENVIRONMENTAL VARIABLES


# 1. SETUP ----
library(camcorder)
library(dplyr)
library(ggplot2)
library(ggtext)
library(patchwork)

env <- readxl::read_excel('data/env.xlsx')
env$site <- factor(env$site, levels = c(1:13))
env <- env %>% filter(!sample == 'v.chico.2')
source('scripts/figures/custom_theme_functional_thesis.R')

env_sum <- env %>% group_by(site) %>% 
  summarise_at(c('disc', 'cond', 'do', 'sat', 'tss', 'chlo', 'bod', 'e.coli', 
                 'po4', 'nh4', 'no3_no2', 'no2', 'no3', 'tp', 'tn', 'qbr', 'ha'),
               list(mean = mean, sd = sd))
env_sum$site <- factor(env_sum$site, levels = c(1:13))


# 2. PLOTS ----
#gg_record(device = 'pdf', dpi = 320, width = 25, height = 15, units = 'cm')

# Conductivity ----
(
  cond <- ggplot(env_sum,   aes(x = site, y = cond_mean)) +
    geom_errorbar(aes(x= site, ymin = cond_mean-cond_sd, ymax = cond_mean+cond_sd), 
                  width = 0.4, color = 'grey75', size = 1.3) +
    geom_point(pch = 19, size = 8, stroke = 1.3, color = 'grey75') +
    geom_point(data = env, aes(x = site, y = cond),
               stroke = NA, fill = '#1d3482', color = '#1d3482', size = 5, alpha = 0.5, pch = 21) +
    scale_y_continuous(limits = c(0, 750), breaks = seq(from = 0, to = 750, by = 150)) +
    labs(y = 'Conductivity (&mu;S.cm<sup>-1</sup>)', x = '') +
    custom_theme %+replace%
    theme(plot.margin = unit(c(0.15,1,0.15,0.35), "cm"),
      axis.title.y = element_markdown(angle = 90, color = 'black', size = 21, margin = margin(r = 8, t = 8)))
)

# Dissolved oxygen ----
(
  dox <- ggplot(env_sum,   aes(x = site, y = do_mean)) +
    geom_errorbar(aes(x= site, ymin = do_mean-do_sd, ymax = do_mean+do_sd), 
                  width = 0.4, color = 'grey75', size = 1.3) +
    geom_point(pch = 19, size = 8, stroke = 1.3, color = 'grey75') +
    geom_point(data = env, aes(x = site, y = do),
               stroke = NA, fill = '#1d3482', color = '#1d3482', size = 5, alpha = 0.5, pch = 21) +
    geom_hline(aes(yintercept = 5.5), color = 'red', size = 1, lty = 2) +
    scale_y_continuous(limits = c(0, 18), breaks = seq(from = 0, to = 18, by = 6)) +
    labs(y = 'Dissolved oxygen (mg.l<sup>-1</sup>)', x = '') +
    custom_theme %+replace%
    theme(plot.margin = unit(c(0.15,1,0.15,0.35), "cm"),
          axis.title.y = element_markdown(angle = 90, color = 'black', size = 21, margin = margin(r = 8, t = 8)))
)

# TSS ----
(
  tss <- ggplot(env_sum,   aes(x = site, y = tss_mean)) +
    geom_errorbar(aes(x= site, ymin = tss_mean-tss_sd, ymax = tss_mean+tss_sd), 
                  width = 0.4, color = 'grey75', size = 1.3) +
    geom_point(pch = 19, size = 8, stroke = 1.3, color = 'grey75') +
    geom_point(data = env, aes(x = site, y = tss),
               stroke = NA, fill = '#1d3482', color = '#1d3482', size = 5, alpha = 0.5, pch = 21) +
    scale_y_continuous(limits = c(-5, 60), breaks = seq(from = 0, to = 60, by = 15)) +
    labs(y = 'TSS (mg.l<sup>-1</sup>)', x = 'Site') +
    custom_theme %+replace%
    theme(plot.margin = unit(c(0.15,1,0.15,0.35), "cm"),
          axis.title.y = element_markdown(angle = 90, color = 'black', size = 21, margin = margin(r = 8)))
)


# NH4 ----

(
  nh4 <- ggplot(env_sum,   aes(x = site, y = nh4_mean)) +
    geom_errorbar(aes(x= site, ymin = nh4_mean-nh4_sd, ymax = nh4_mean+nh4_sd), 
                  width = 0.4, color = 'grey75', size = 1.3) +
    geom_point(pch = 19, size = 8, stroke = 1.3, color = 'grey75') +
    geom_point(data = env, aes(x = site, y = nh4), 
               stroke = NA, fill = '#1d3482', color = '#1d3482', size = 5, alpha = 0.5, pch = 21) +
    geom_hline(aes(yintercept = 1370), color = 'red', size = 1, lty = 2) +
    scale_y_continuous(limits = c(-1500, 12000), breaks = seq(from = 0, to = 12000, by = 3000)) +
    labs(y = 'NH4<sup>+</sup> (&mu;g.l<sup>-1</sup>)', x = '') +
    custom_theme %+replace%
    theme(plot.margin = unit(c(0.15,1,0.15,0.35), "cm"),
          axis.title.y = element_markdown(angle = 90, color = 'black', size = 21, margin = margin(r = 8)))
)

# NO3 + NO2 ----
(
  no3_no2 <- ggplot(env_sum,   aes(x = site, y = no3_no2_mean)) +
    geom_errorbar(aes(x= site, ymin = no3_no2_mean-no3_no2_sd, ymax = no3_no2_mean+no3_no2_sd), 
                  width = 0.4, color = 'grey75', size = 1.3) +
    geom_point(pch = 19, size = 8, stroke = 1.3, color = 'grey75') +
    geom_point(data = env, aes(x = site, y = no3_no2), 
               stroke = NA, fill = '#1d3482', color = '#1d3482', size = 5, alpha = 0.5, pch = 21) +
    scale_y_continuous(limits = c(-100, 2500), breaks = seq(from = 0, to = 2500, by = 500)) +
    labs(y = 'NO<sub>3</sub><sup>-</sup> + NO<sub>2</sub><sup>-</sup> (&mu;g.l<sup>-1</sup>)', x = '') +
    custom_theme %+replace%
    theme(plot.margin = unit(c(0.15,1,0.15,0.35), "cm"),
          axis.title.y = element_markdown(angle = 90, color = 'black', size = 21, margin = margin(r = 8)))
)


# PO4 ----
(
  po4 <- ggplot(env_sum,   aes(x = site, y = po4_mean)) +
    geom_errorbar(aes(x= site, ymin = po4_mean-po4_sd, ymax = po4_mean+po4_sd), 
                  width = 0.4, color = 'grey75', size = 1.3) +
    geom_point(pch = 19, size = 8, stroke = 1.3, color = 'grey75') +
    geom_point(data = env, aes(x = site, y = po4), fill = '#1d3482', color = '#1d3482', size = 5, alpha = 0.5, pch = 21) +
    geom_hline(aes(yintercept = 100), color = 'red', size = 1, lty = 2) +
    scale_y_continuous(limits = c(-100, 1550), breaks = seq(from = 0, to = 1550, by = 310)) +
    labs(y = 'PO<sub>4</sub><sup>3-</sup> (&mu;g.l<sup>-1</sup>)', x = 'Site') +
    custom_theme %+replace%
    theme(plot.margin = unit(c(0.15,1,0.15,0.35), "cm"),
          axis.title.y = element_markdown(angle = 90, color = 'black', size = 21, margin = margin(r = 8)))
)


# BOD ----
(
  bod <- ggplot(env_sum,   aes(x = site, y = bod_mean)) +
    geom_errorbar(aes(x= site, ymin =bod_mean-bod_sd, ymax = bod_mean+bod_sd), 
                  width = 0.4, color = 'grey75', size = 1.3) +
    geom_point(pch = 19, size = 8, stroke = 1.3, color = 'grey75') +
    geom_point(data = env, aes(x = site, y = bod), fill = '#1d3482', color = '#1d3482', size = 5, alpha = 0.5, pch = 21) +
    scale_y_continuous(limits = c(-10, 250), breaks = seq(from = 0, to = 250, by = 50)) +
    labs(y = 'BOD<sub>5</sub> (mg.l<sup>-1</sup>)', x = 'Site') +
    custom_theme %+replace%
    theme(plot.margin = unit(c(0.15,1,0.15,0.35), "cm"),
          axis.title.y = element_markdown(angle = 90, color = 'black', size = 21, margin = margin(r = 8)))
)


# Discharge ----

(
  disc <- ggplot(env_sum,   aes(x = site, y = disc_mean)) +
    geom_errorbar(aes(x= site, ymin = disc_mean-disc_sd, ymax = disc_mean+disc_sd), 
                  width = 0.4, color = 'grey75', size = 1.3) +
    geom_point(pch = 19, size = 8, stroke = 1.3, color = 'grey75') +
    geom_point(data = env, aes(x = site, y = disc), fill = '#1d3482', color = '#1d3482', size = 5, alpha = 0.5, pch = 21) +
    scale_y_continuous(limits = c(0, 15), breaks = seq(from = 0, to = 15, by = 3)) +
    labs(y = 'Discharge (m<sup>3</sup>.s<sup>-1</sup>)', x = '') +
    custom_theme %+replace%
    theme(plot.margin = unit(c(0.15,1,0.15,0.35), "cm"),
          axis.title.y = element_markdown(angle = 90, color = 'black', size = 21, margin = margin(r = 8, t = 8)))
)



gg_record(device = 'pdf', dpi = 320, width = 40, height = 30, units = 'cm')
design <- 'ABC
           DEF
           GH#'
(final <- disc + cond + dox + nh4 + no3_no2 + po4 + bod + tss + plot_layout(design = design))

gg_stop_recording()
ggsave(final, file = 'figures/env_variables.png', dpi = 320, width = 40, height = 30, units = 'cm')









