# RLQ AND FOURTH CORNER PLOTS


# 1. SETUP ----
library(camcorder)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggtext)
library(patchwork)

rlq <- readRDS('rlq.RDS')
source('figures/custom_theme_functional_thesis.R')



# 2. RLQ PLOT ----
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
ggsave(rlq_4_plots, file = 'rlq.png', dpi = 320, width = 25, height = 25, units = 'cm')

















                        
