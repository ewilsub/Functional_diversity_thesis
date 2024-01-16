# CUSTOM THEME

custom_theme <- theme_classic() + 
  theme(
    legend.position = 'none',
  axis.title.x = element_text(color = 'black', size = 16, margin = margin(t = 8)),
  axis.title.y = element_text(color = 'black', size = 16, margin = margin(r = 8)),
  axis.text.x = element_text(color = 'black', size = 13, margin = margin(t = 6)),
  axis.text.y = element_text(color = 'black', size = 13, margin = margin(r = 6)),
  axis.ticks = element_line(size = 1, color = 'black'),
  axis.ticks.length = unit(.25, "cm"),
  axis.line = element_blank(),
  panel.border = element_rect(size = 1, color = 'black', fill = NA)
  )
