############################################################
#                                                          #
#                   Create chroropleths                    #
#                                                          #
############################################################

#-- Load packages --#
library(tidyverse)
library(magrittr)
library(sf)
library(ggthemes)

#-- Read shapefile --#
za_map <- st_read('data-shp/zaf_admin1.shp')

#-- Take a look and clean --#
# Quick look
za_map
glimpse(za_map)

# Check object size
object.size(za_map)

# Simplyfy
za_map %<>%
    ## Select columns
    select(ADM1_EN) %>%
    ## Reduce size
    st_simplify(preserveTopology = TRUE,
                dTolerance = 0.03)

# Check object size and glimpse
object.size(za_map)
glimpse(za_map)

# Clean
za_map %<>%
    ## Convert factors to character
    mutate(ADM1_EN = as.character(ADM1_EN)) %>%
    ## Correct Northern Cape error
    mutate(ADM1_EN = ifelse(ADM1_EN == 'Nothern Cape',
                            yes = 'Northern Cape',
                            no = ADM1_EN))

# Exploratory plot
plot(st_geometry(za_map))

#-- Rounding up from 0.5 --#
round2 <- function(x, n) {
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5
    z = trunc(z)
    z = z/10^n
    z * posneg
}

#-- Provincial chronic pain --#
cpain_province <- read_csv('data/province.csv') %>%
    rename(ADM1_EN = province)


#-- Join cpain_province with za_map
cpain_map <- za_map %>%
    left_join(cpain_province)

#-- Plot --#
unadjusted <- ggplot(data = cpain_map) +
    geom_sf(aes(fill = ADM1_EN),
            colour = '#000000') +
    geom_sf_label(aes(label = paste0(round2(estimate_chronic, 0), '%')),
                  size = 6) +
    coord_sf(datum = NA) +
    scale_fill_tableau(palette = 'Classic Color Blind',
                       name = 'Province') +
    theme_minimal(base_size = 16) +
    theme(plot.background = element_rect(colour = '#FFFFFF',
                                          fill = '#FFFFFF'),
          axis.title = element_blank())

unadjusted

ggsave(filename = 'figures/figure-2-province-unadjusted.png',
       plot = unadjusted,
       width = 9,
       height = 8)

adjusted <- ggplot(data = cpain_map) +
    geom_sf(aes(fill = ADM1_EN),
            colour = '#000000') +
    geom_sf_label(aes(label = paste0(round2(adj.estimate_chronic, 0), '%')),
                  size = 6) +
    coord_sf(datum = NA) +
    scale_fill_tableau(palette = 'Classic Color Blind',
                       name = 'Province') +
    theme_minimal(base_size = 16) +
    theme(plot.background = element_rect(colour = '#FFFFFF',
                                         fill = '#FFFFFF'),
          axis.title = element_blank())

adjusted

ggsave(filename = 'figures/figure-2-province-adjusted.png',
       plot = adjusted,
       width = 9,
       height = 8)
