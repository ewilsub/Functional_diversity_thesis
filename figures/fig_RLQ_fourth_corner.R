# RLQ AND FOURTH CORNER PLOTS


# 1. SETUP ----
library(camcorder)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(patchwork)

rlq <- readRDS('results/rlq.RDS')
source('scripts/figures/custom_theme_functional_thesis.R')




# 2. FOURTH CORNER PLOTS ----
# creating fourth corner tables
# traits table
corner_traits <- data.frame(modality = rownames(rlq$traits),
                            # 1 = negative; 2 = positive
                            AxcR1 = c(0,0,0,0,0,0,2,0,0,1,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0),
                            AxcR2 = rep(0, 26))

corner_traits <- corner_traits %>% 
  mutate(modality = case_when(modality == 'BOD1' ~ '<2.5', 
                              modality == 'BOD2' ~ '2.5 - 5', 
                              modality == 'BOD3' ~ '5 - 10',
                              modality == 'BOD4' ~ '10 - 15',
                              modality == 'BOD5' ~ '15 - 20',
                              modality == 'BOD6' ~ '>20',
                              modality == 'RESP1' ~ 'Gills',
                              modality == 'RESP2' ~ 'Spiracle',
                              modality == 'RESP3' ~ 'Plastron',
                              modality == 'RESP4' ~ 'Tegument',
                              modality == 'RESP5' ~ 'Lung',
                              modality == 'FFG1' ~ 'Predator',
                              modality == 'FFG2' ~ 'Filterer',
                              modality == 'FFG3' ~ 'Gatherer',
                              modality == 'FFG4' ~ 'Scraper',
                              modality == 'FFG5' ~ 'Shredder',
                              modality == 'FFG6' ~ 'Miner / herbivore',
                              modality == 'RS1' ~ 'Aquatic', 
                              modality == 'RS2' ~ 'Terrestrial',
                              modality == 'LOC1' ~ 'Swimmer',
                              modality == 'LOC2' ~ 'Crawler',
                              modality == 'LOC3' ~ 'Burrower',
                              modality == 'LOC4' ~ 'Attached',
                              modality == 'VOLT1' ~ 'Semivoltine',
                              modality == 'VOLT2' ~ 'Univoltine',
                              modality == 'VOLT3' ~ 'Plurivoltine'))

corner_traits$modality <- factor(corner_traits$modality,
                                 levels = c('<2.5', 
                                            '2.5 - 5', 
                                            '5 - 10',
                                            '10 - 15',
                                            '15 - 20',
                                            '>20',
                                            'Gills',
                                            'Spiracle',
                                            'Plastron',
                                            'Tegument',
                                            'Lung',
                                            'Predator',
                                            'Filterer',
                                            'Gatherer',
                                            'Scraper',
                                            'Shredder',
                                            'Miner / herbivore',
                                            'Aquatic', 
                                            'Terrestrial',
                                            'Swimmer',
                                            'Crawler',
                                            'Burrower',
                                            'Attached',
                                            'Semivoltine',
                                            'Univoltine',
                                            'Plurivoltine'))

corner_traits <- corner_traits %>% tidyr::pivot_longer(cols = c('AxcR1', 'AxcR2'), 
                                                       names_to = 'axis',
                                                       values_to = 'correlation')
corner_traits$correlation <- as.factor(corner_traits$correlation)


# variables table
corner_env <- data.frame(variable = rownames(rlq$env),
                         # 1 = negative; 2 = positive
                         AxcQ1 = c(0,1,2,1,1,1,1,1),
                         AxcQ2 = c(2,0,2,0,0,0,0,0))
corner_env <- corner_env %>% tidyr::pivot_longer(cols = c('AxcQ1', 'AxcQ2'), 
                                                 names_to = 'axis',
                                                 values_to = 'correlation')
corner_env <- corner_env %>% 
  mutate(variable = case_when(variable == 'disc' ~ 'Discharge',
                              variable == 'cond' ~ 'Conductivity',
                              variable == 'do' ~ 'Dissolved oxygen',
                              variable == 'tss' ~ 'TSS',
                              variable == 'bod' ~ 'BOD<sub>5</sub>',
                              variable == 'po4' ~ 'PO<sub>4</sub><sup>3-</sup>',
                              variable == 'nh4' ~ 'NH<sub>4</sub><sup>-</sup>',
                              variable == 'no3_no2' ~ 'NO<sub>3</sub><sup>-</sup> + NO<sub>2</sub><sup>-</sup>',
                               ))
corner_env$correlation <- as.factor(corner_env$correlation)


gg_record(device = 'pdf', dpi = 320, width = 13, height = 26, units = 'cm')

# traits ====
(
  corner_traits_plot <- ggplot(corner_traits, aes(x = axis, y = modality)) +
    geom_tile(aes(width=1, height = 1, fill = correlation), color = 'grey20', size = 0.4) +
    scale_y_discrete(limits=rev) +
    scale_fill_manual(labels = c('Non-Significant\nCorrelation', 'Negative\nCorrelation', 'Positive\nCorrelation'),
                      values = c('white', '#f75c5f', '#6fc5e8')) +
    annotate(geom = 'segment', y = 3.5, yend = 3.5, x = 0.5, xend = 2.5, size = 1) +
    annotate(geom = 'segment', y = 7.5, yend = 7.5, x = 0.5, xend = 2.5, size = 1) +
    annotate(geom = 'segment', y = 9.5, yend = 9.5, x = 0.5, xend = 2.5, size = 1) +
    annotate(geom = 'segment', y = 15.5, yend = 15.5, x = 0.5, xend = 2.5, size = 1) +
    annotate(geom = 'segment', y = 20.5, yend = 20.5, x = 0.5, xend = 2.5, size = 1) +
    annotate(geom = 'text', y = 23.5, x = 2.58, label = 'Body size', angle = -90, size = 5) +
    annotate(geom = 'text', y = 18, x = 2.58, label = 'Respiration', angle = -90, size = 5) +
    annotate(geom = 'text', y = 12.5, x = 2.58, label = 'FFG', angle = -90, size = 5) +
    annotate(geom = 'text', y = 8.5, x = 2.58, label = 'RS', angle = -90, size = 5) +
    annotate(geom = 'text', y = 5.5, x = 2.58, label = 'Locomotion', angle = -90, size = 5) +
    annotate(geom = 'text', y = 2, x = 2.58, label = 'Voltinism', angle = -90, size = 5) +
    labs(y = '', x = '') +
    coord_cartesian(clip = "off") +
    custom_theme %+replace% 
    theme(
    legend.position = 'top',
    legend.title = element_blank(),
    legend.text = element_text(hjust = 0.5),
    axis.line = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_text(color = 'black', size = 13, hjust = 1, margin = margin(r = -15)),
    axis.text.x = element_text(color = 'black', size = 18, margin = margin(t = 0)),
    plot.margin = margin(r = 14))
)
#ggsave(corner_traits_plot, file = 'figures/corner_traits.png', dpi = 320, width = 13, height = 26, units = 'cm')



# variables ====
gg_record(device = 'pdf', dpi = 320, width = 13, height = 15, units = 'cm')
(
  corner_env_plot <- ggplot(corner_env, aes(x = axis, y = variable)) +
    geom_tile(aes(width=1, height = 1, fill = correlation), color = 'grey20', size = 0.4) +
    scale_y_discrete(limits=rev) +
    scale_fill_manual(labels = c('Non-Significant\nCorrelation', 'Negative\nCorrelation', 'Positive\nCorrelation'),
                      values = c('white', '#f75c5f', '#6fc5e8')) +
    labs(y = '', x = '') +
    custom_theme %+replace% 
    theme(
      legend.position = 'top',
      legend.title = element_blank(),
      legend.text = element_text(hjust = 0.5),
      axis.line = element_blank(),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text.y = element_markdown(color = 'black', size = 13, hjust = 1, margin = margin(r = -11)),
      axis.text.x = element_text(color = 'black', size = 18, margin = margin(t = 0)),
      plot.margin = margin(r = 22))
)

ggsave(corner_env_plot, file = 'figures/corner_env.png', dpi = 320, width = 13, height = 15, units = 'cm')


# 3. RLQ PLOT ----
# preparing tables
# variables ====
env <- rlq$env %>% rename(axis1 = RS1, axis2 = RS2) %>% 
  mutate(var = rownames(rlq$env)) %>% 
  mutate(var = case_when(var == 'disc' ~ 'Discharge',
                         var == 'cond' ~ 'Conductivity',
                         var == 'do' ~ 'Dissolved<br>oxygen',
                         var == 'tss' ~ 'TSS',
                         var == 'bod' ~ 'BOD<sub>5</sub>',
                         var == 'po4' ~ 'PO<sub>4</sub><sup>3-</sup>',
                         var == 'nh4' ~ 'NH<sub>4</sub><sup>-</sup>',
                         var == 'no3_no2' ~ 'NO<sub>3</sub><sup>-</sup> + NO<sub>2</sub><sup>-</sup>',
                         ))

# traits ====
traits <- rlq$traits %>% rename(axis1 = CS1, axis2 = CS2) %>% 
  mutate(modality = rownames(rlq$traits)) %>% 
  mutate(text_color = case_when(modality %in% c('RESP1', 'RESP4') ~ '#9e2a2b',
                                modality %in% c('FFG3') ~ '#003566',
                                .default = 'grey57'))
           
           
 
  
# taxa ====
taxa <- rlq$spe %>% rename(axis1 = AxcQ1, axis2 = AxcQ2)
  

# sites ====
env_log <- readRDS('data/env_log.RDS')
sites <- rlq$samples %>% 
  rename(axis1 = AxcR1, axis2 = AxcR2) %>% 
  mutate(site = env_log$site, date = env_log$date) %>% 
  mutate(date = case_when(date == 'December' ~ 'D',
                          date == 'March' ~ 'M',
                          date == 'July' ~ 'J',
                          date == 'October' ~ 'O')) %>% 
  mutate(sample = paste(site, date, sep = '.'), .before = everything())
sites$site <- factor(sites$site, levels = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13'))
 

# multiple RLQ plots ====
## sites ----
rlq_plot_sites <- ggplot(sites, aes(x = axis1, y = axis2)) +
    geom_hline(yintercept = 0, lty = 'longdash', size = 0.5, alpha = 0.35) +
    geom_vline(xintercept = 0, lty = 'longdash', size = 0.5, alpha = 0.35) +
    
    geom_point(aes(fill = site, color = site), pch = 21, size = 8, alpha = 0.9) +
    scale_fill_viridis_d() +
    scale_colour_viridis_d() +
    geom_text(aes(label = site), size = 5, fontface = 'bold') +
    
    labs(title = 'A. Samples', x = '', y = 'RLQ axis 2 (8%)') +
    
  custom_theme %+replace% 
    theme(plot.title = element_text(size = 14, hjust = 0.02, vjust = 1, face = 'bold'))

## variables ----
rlq_plot_env <- ggplot(data = env, aes(x = axis1, y = axis2)) +
  geom_hline(yintercept = 0, lty = 'longdash', size = 0.5, alpha = 0.35) +
  geom_vline(xintercept = 0, lty = 'longdash', size = 0.5, alpha = 0.35) +
  
  geom_segment(data = env %>% filter(var != 'Discharge'), 
               aes(x = 0, y = 0, xend = axis1*0.9, yend = axis2*0.9),
               arrow = arrow(length = unit(4, "mm"))) +
  
  geom_segment(data = env %>% filter(var == 'Discharge'), 
               aes(x = 0, y = 0, xend = axis1*0.9, yend = axis2*0.9),
               color = 'grey57',
               arrow = arrow(length = unit(4, "mm"))) +
  
  geom_richtext(data = env %>% filter(var != 'Discharge'), 
                aes(x = axis1, y = axis2, label = var),
                  fontface = 'bold', size = 4, fill = NA, label.color = NA) +
  geom_richtext(data = env %>% filter(var == 'Discharge'), 
                aes(x = axis1, y = axis2, label = var),
                fontface = 'bold', size = 4, color = 'grey57', fill = NA, label.color = NA) +
  
  labs(title = 'B. Environment', x = '', y = '') +
  
  scale_y_continuous(expand = c(0.05, 0.05)) +
  scale_x_continuous(expand = c(0.09, 0.09)) +
  
  custom_theme %+replace% 
  theme(plot.title = element_text(size = 14, hjust = 0.02, vjust = 1, face = 'bold'))

## taxa ----
rlq_plot_taxa <- ggplot(taxa, aes(x = axis1, y = axis2)) +
  geom_hline(yintercept = 0, lty = 'longdash', size = 0.5, alpha = 0.35) +
  geom_vline(xintercept = 0, lty = 'longdash', size = 0.5, alpha = 0.35) +
  
  geom_text(aes(label = code), size = 5, color = 'grey20') +
  
  labs(title = 'C. Taxa', x = 'RLQ axis 1 (90%)', y = 'RLQ axis 2 (8%)') +
  
  custom_theme %+replace% 
  theme(plot.title = element_text(size = 12, hjust = 0.02, vjust = 1, face = 'bold'))


## traits ----
rlq_plot_traits <- ggplot(data = traits, aes(x = axis1, y = axis2)) +
  geom_hline(yintercept = 0, lty = 'longdash', size = 0.5, alpha = 0.35) +
  geom_vline(xintercept = 0, lty = 'longdash', size = 0.5, alpha = 0.35) +
  
  geom_segment(data = traits %>% filter(!modality %in% c('RESP1', 'RESP4', 'FFG3')), 
               aes(x = 0, y = 0, xend = axis1*0.9, yend = axis2*0.9),
               color = 'grey57', arrow = arrow(length = unit(4, "mm"))) +
  geom_segment(data = traits %>% filter(modality %in% c('RESP1', 'RESP4', 'FFG3')), 
               aes(x = 0, y = 0, xend = axis1*0.9, yend = axis2*0.9),
               arrow = arrow(length = unit(4, "mm"))) +
   
  geom_richtext(data = traits %>% filter(!modality %in% c('RESP1', 'RESP4', 'FFG3')), 
                aes(x = axis1, y = axis2, label = modality),
                fontface = 'bold', size = 5, fill = NA, label.color = NA, text.colour = 'grey57') +
  geom_richtext(data = traits %>% filter(modality %in% c('RESP1', 'RESP4', 'FFG3')), 
                aes(x = axis1, y = axis2, label = modality, text.colour = text_color),
                fontface = 'bold', size = 6, fill = 'white', label.color = 'black') +
  
  labs(title = 'D. Traits', x = 'RLQ axis 1 (90%)', y = '') +
  
  scale_y_continuous(expand = c(0.05, 0.05)) +
  scale_x_continuous(expand = c(0.09, 0.09)) +
  
  custom_theme %+replace% 
  theme(plot.title = element_text(size = 14, hjust = 0.02, vjust = 1, face = 'bold'))



gg_record(device = 'pdf', dpi = 320, width = 25, height = 25, units = 'cm')
(rlq_4_plots <- rlq_plot_sites + rlq_plot_env + rlq_plot_taxa + rlq_plot_traits)

gg_stop_recording()
ggsave(rlq_4_plots, file = 'figures/rlq.png', dpi = 320, width = 25, height = 25, units = 'cm')

















                        