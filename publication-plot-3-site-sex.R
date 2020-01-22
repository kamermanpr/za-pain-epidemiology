############################################################
#                                                          #
#                       Site and sex                       #
#                                                          #
############################################################

# Load packages
library(tidyverse)
library(magrittr)

# Palettes
pal_za <- c('#9ecae1', '#3182bd')
scales::show_col(pal_za)

# Load data
data <- read_csv('data/site-sex.csv')

# Glimpse
glimpse(data)

# Plot
pp <- data %>%
    mutate(site = factor(site),
           site = fct_reorder(site,
                              desc(estimate_chronic))) %>%
    ggplot(data = .) +
    aes(x = site,
        y = estimate_chronic,
        ymin = ci.low_chronic,
        ymax = ci.high_chronic,
        fill = sex) +
    geom_errorbar(position = position_dodge(width = 0.6),
                  size = 1,
                  width = 0.5,
                  colour = '#000000') +
    geom_point(position = position_dodge(width = 0.6),
               size = 8,
               shape = 21,
               stroke = 1,
               colour = '#000000') +
    labs(x = 'Body region',
         y = 'Prevalence of chronic pain (%)') +
    scale_y_continuous(limits = c(0, 50)) +
    scale_fill_manual(values = pal_za,
                      labels = c('Women', 'Men')) +
    theme_bw(base_size = 20) +
    theme(legend.position = c(0.85, 0.86),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.key.size = unit(40, "pt"),
          panel.grid = element_blank(),
          axis.title.y = element_text(margin = margin(r = 1,
                                                      unit = 'lines')),
          axis.title.x = element_text(margin = margin(t = 1,
                                                      unit = 'lines')),
          axis.text.y = element_text(colour = '#000000',
                                     size = 20),
          axis.text.x = element_text(colour = '#000000',
                                     size = 20,
                                     angle = 30,
                                     hjust = 1))

# Save
ggsave(filename = 'figures/figure-3-site-sex.png',
       plot = pp,
       height = 8.5,
       width = 9.5)

