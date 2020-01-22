############################################################
#                                                          #
#         Chronic pain vs self-rated health status         #
#                                                          #
############################################################

# Load packages
library(tidyverse)
library(magrittr)
library(RColorBrewer)

# Palettes
pal_za <- rev(brewer.pal(n = 4, name = 'Blues'))
scales::show_col(pal_za)

# Load data
data <- read_csv('data/pain-health-status.csv')

# Glimpse
glimpse(data)

# Process data
data %<>%
    mutate(health_status = factor(health_status,
                                  levels = c('excellent', 'good', 'average', 'poor'),
                                  ordered = TRUE)) %>%
    mutate(percent = round(count, 1),
           ci.low = round(ci.low, 1),
           ci.high = round(ci.high, 1)) %>%
    mutate(ci_range = str_glue('[{ci.low}, {ci.high}]')) %>%
    group_by(Pain) %>%
    mutate(y_position = cumsum(count))

# Plot - filled barplot
pp <- ggplot(data = data) +
    aes(x = Pain,
        y = count,
        fill = health_status) +
    geom_col(colour = '#000000') +
    geom_text(aes(label = str_glue('{percent}%'),
                  y = y_position - 2),
              size = 6) +
    geom_text(aes(label = ci_range,
                  y = y_position - 5),
              size = 5) +
    labs(x = 'Chronic pain',
         y = 'Percent (%)') +
    scale_x_discrete(labels = c('No', 'Yes')) +
    scale_fill_manual(name = 'Health status',
                      labels = c('Excellent', 'Good', 'Average', 'Poor'),
                      values = pal_za) +
    theme_bw(base_size = 20) +
    theme(legend.text = element_text(size = 18),
          legend.key.size = unit(40, "pt"),
          panel.grid = element_blank(),
          axis.title.y = element_text(margin = margin(r = 1,
                                                      unit = 'lines')),
          axis.title.x = element_text(margin = margin(t = 1,
                                                      unit = 'lines')),
          axis.text.y = element_text(colour = '#000000',
                                     size = 20),
          axis.text.x = element_text(colour = '#000000',
                                     size = 20))

# Save
ggsave(filename = 'figures/figure-4-health-status.png',
       plot = pp,
       height = 8.5,
       width = 9.5)

