# Load packages
library(tidyverse)
library(magrittr)

# Palettes
pal_za <- c('#9ecae1', '#3182bd')
scales::show_col(pal_za)

# Load data
data <- read_csv('data/wealth.csv')

# Glimpse
glimpse(data)

# Process data
data %<>%
    mutate(wealth = factor(wealth,
                           levels = c('Poorest', 'Poorer', 'Middle',
                                      'Richer', 'Richest'),
                           ordered = TRUE))

# Plot
pp <- ggplot(data = data) +
    aes(x = wealth,
        y = estimate_chronic,
        ymin = ci.low_chronic,
        ymax = ci.high_chronic) +
    geom_errorbar(size = 1,
                  width = 0.5,
                  colour = '#000000') +
    geom_point(size = 8,
               shape = 21,
               stroke = 1,
               fill = pal_za[[2]],
               colour = '#000000') +
    geom_text(aes(y = 30,
                  label = paste0('(', unweighted_n, ')')),
              size = 6) +
    labs(title = 'G: Prevalence by wealth index',
         subtitle = 'Numbers in parentheses show the unweighted sample sizes',
         x = 'Wealth index',
         y = 'Prevalence of chronic pain (%)') +
    scale_y_continuous(limits = c(0, 30)) +
    scale_x_discrete(labels = c('Poorest\n(quintile 1)',
                                'Poorer\n(quintile 2)',
                                'Middle\n(quintile 3)',
                                'Richer\n(quintile 4)',
                                'Richest\n(quintile 5)')) +
    theme_bw(base_size = 20) +
    theme(panel.grid = element_blank(),
          plot.subtitle = element_text(size = 14),
          axis.title.y = element_text(margin = margin(r = 1,
                                                      unit = 'lines')),
          axis.title.x = element_text(margin = margin(t = 1,
                                                      unit = 'lines')),
          axis.text.y = element_text(colour = '#000000'),
          axis.text.x = element_text(colour = '#000000'))

# Save
ggsave(filename = 'figures/supplement-1-G-wealth.png',
       plot = pp,
       height = 8,
       width = 8)
