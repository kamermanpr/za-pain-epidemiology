############################################################
#                                                          #
#                        Provinces                         #
#                                                          #
############################################################

# Load packages
library(tidyverse)
library(magrittr)

# Palettes
pal_za <- c('#9ecae1', '#3182bd')
scales::show_col(pal_za)

# Load data
data <- read_csv('data/province.csv')

# Glimpse
glimpse(data)

# Process data
data %<>%
    mutate(province = factor(province),
           province = fct_reorder(province,
                                  estimate_chronic))

# Plot
pp <- ggplot(data = data) +
    aes(x = province,
        y = estimate_chronic,
        ymin = ci.low_chronic,
        ymax = ci.high_chronic) +
    geom_errorbar(size = 1,
                  width = 0.25,
                  colour = '#000000') +
    geom_point(size = 8,
               shape = 21,
               stroke = 1,
               fill = pal_za[[2]],
               colour = '#000000') +
    geom_text(aes(y = 40,
                  label = paste0('(', unweighted_n, ')')),
              size = 6) +
    labs(title = 'E: Prevalence by province',
         subtitle = 'Numbers in parentheses show the unweighted sample sizes',
         x = 'Provinces',
         y = 'Prevalence of chronic pain (%)') +
    scale_y_continuous(limits = c(0, 40)) +
    theme_bw(base_size = 20) +
    theme(panel.grid = element_blank(),
          plot.subtitle = element_text(size = 14),
          axis.title.y = element_text(margin = margin(r = 1,
                                                      unit = 'lines')),
          axis.title.x = element_text(margin = margin(t = 1,
                                                      unit = 'lines')),
          axis.text.y = element_text(colour = '#000000'),
          axis.text.x = element_text(colour = '#000000',
                                     angle = 30,
                                     hjust = 1))

# Save
ggsave(filename = 'figures/supplement-1-E-provinces.png',
       plot = pp,
       height = 8,
       width = 8)
