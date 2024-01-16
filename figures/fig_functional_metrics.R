# FUNCTIONAL METRIC PLOTS


# 1. SETUP ----
library(camcorder)
library(dplyr)
library(ggplot2)
library(patchwork)

ses_results <- readRDS('ses_results.RDS')
source('figures/custom_theme_functional_thesis.R')

ses_results <- ses_results %>%
  mutate(site = case_when(
    startsWith(sample, "hoya") ~ 1,
    startsWith(sample, "vialidad") ~ 2,
    startsWith(sample, "toma.de.agua") ~ 3,
    startsWith(sample, "piletones") ~ 4,
    startsWith(sample, "canalización") ~ 5,
    startsWith(sample, "v.chico") ~ 6,
    startsWith(sample, "esquel.v. chico") ~ 7,
    startsWith(sample, "matadero") ~ 8,
    startsWith(sample, "planta") ~ 9,
    startsWith(sample, "campo.roberts") ~ 10,
    startsWith(sample, "teddy") ~ 11,
    startsWith(sample, "puente.kansas") ~ 12,
    startsWith(sample, "cinco.esquinas") ~ 13),
    .after = sample)
ses_results <- ses_results %>% mutate(site = as.factor(ses_results$site))


# 2. INDIVIDUAL PLOTS ----
gg_record(device = 'pdf', dpi = 320, width = 25, height = 15, units = 'cm')

(
plot1 <- ses_results %>% 
  ggplot(aes(x = site, y = fric.ses)) +
  annotate(geom = 'rect', ymin = -1.96, ymax = 1.96, xmin = -Inf, xmax = Inf, 
             fill = 'grey80', color = NA, alpha = 0.4) +
  geom_hline(yintercept = 0, lty = 'longdash') +
  geom_point(fill = '#1d3482', color = '#1d3482', size = 7, alpha = 0.5, pch = 21) +
  scale_y_continuous(limits = c(-4, 4)) +
  labs(y = 'Functional richness (SES)', x = '') +
  custom_theme
)


(
  plot2 <- ses_results %>% 
  ggplot(aes(x = site, y = feve.ses)) +
  annotate(geom = 'rect', ymin = -1.96, ymax = 1.96, xmin = -Inf, xmax = Inf, 
             fill = 'grey80', color = NA, alpha = 0.4) +
  geom_hline(yintercept = 0, lty = 'longdash') +
  geom_point(fill = '#1d3482', color = '#1d3482', size = 7, alpha = 0.5, pch = 21) +
  scale_y_continuous(limits = c(-4, 4)) +
  labs(y = 'Functional evenness (SES)', x = '') +
  custom_theme
)


(
  plot3 <-ses_results %>% 
  ggplot(aes(x = site, y = fdis.ses)) +
  annotate(geom = 'rect', ymin = -1.96, ymax = 1.96, xmin = -Inf, xmax = Inf, 
             fill = 'grey80', color = NA, alpha = 0.4) +
  geom_hline(yintercept = 0, lty = 'longdash') +
  geom_point(fill = '#1d3482', color = '#1d3482', size = 7, alpha = 0.5, pch = 21) +
  scale_y_continuous(limits = c(-4, 4)) +
  labs(y = 'Functional dispersion (SES)', x = 'Site') +
  custom_theme
)

# 3. FINAL PLOT ----
gg_record(device = 'pdf', dpi = 320, width = 25, height = 40, units = 'cm')

(finalplot <- plot1 + plot2 + plot3 +
    plot_layout(nrow = 3, byrow = FALSE))

gg_stop_recording()

ggsave(finalplot, filename = 'functional_metrics.png',
       dpi = 320, width = 25, height = 40, units = 'cm')





























