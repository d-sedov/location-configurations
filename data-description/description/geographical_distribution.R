################################################################################
################################################################################
#
# FILE: geographical_distribution.R
#
# BY: Dmitry Sedov 
#
# CREATED: Thu Feb 13 2020
#
# DESC: This file creates a map-plot with the distribution of restaurants and 
#       establishments over the US territory (urban establishments only).
#
################################################################################
################################################################################

################################ Libraries #####################################

library(readr)
library(dplyr)
library(xtable)
library(kableExtra)
library(jsonlite)
library(tidyr)
library(ggplot2)
library(scales)
library(usmap) # US population data
library(noncensus)
library(stringr)

################################################################################

################################# Options ######################################

# Set the tables properties
# Footnote font size
options(xtable.size = 'footnotesize')
# Page top placement
options(xtable.table.placement = 't')

# Input folder
input_folder_path = '/Users/muser/dfolder/Research/urban/data/output/descriptive'

# Output folders
tables_folder_path = '/Users/muser/dfolder/Research/urban/output/tables/descriptive'
plots_folder_path = '/Users/muser/dfolder/Research/urban/output/plots/descriptive'

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 6),
                  legend.title = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.text = element_blank(),
                  axis.title = element_blank(),
)
mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

################################################################################

############################## Prepare data ####################################

# Import data
restaurants <- read_csv(file.path(input_folder_path, 'data_restaurants.csv'))
establishments <- read_csv(file.path(input_folder_path, 'data_establishments.csv'))

# County-level data, zip code to county mapping
data('zip_codes')
data('counties')

# Get zip-county mapping
zip_codes <- zip_codes %>% 
    select(zip, fips) %>%
    mutate(zip = as.integer(zip))
counties <- counties %>%
    mutate(fips = paste0(state_fips, county_fips)) %>%
    transmute(fips = as.integer(fips),
              population = population)

# Aggregate by county establishments and restaurants
urban_establishments_by_zip <- establishments %>% 
    filter(!is.na(cbsa)) %>%
    group_by(zip_code) %>%
    summarise(n = n()) %>%
    mutate(zip_code = as.integer(zip_code))

urban_establishments_by_county <- urban_establishments_by_zip %>%
    left_join(zip_codes,
              by = c('zip_code' = 'zip')) %>% 
    left_join(counties) %>%
    mutate(fips = str_pad(fips, 5, pad = "0")) %>% 
    group_by(fips) %>%
    summarise(s = sum(n),
              logn = log10(sum(n))) %>%
    filter(logn >= 1)

restaurants_by_county <- restaurants %>%
    filter(!is.na(cbsa)) %>% 
    mutate(zip_code = as.integer(zip_code)) %>%
    left_join(zip_codes,
              by = c('zip_code' = 'zip')) %>% 
    left_join(counties) %>%
    mutate(fips = str_pad(fips, 5, pad = "0")) %>% 
    group_by(fips) %>%
    summarise(lon = mean(longitude, na.rm = T),
              lat = mean(latitude, na.rm = T),
              total = n(),
              logn = log10(n())) %>%
    select(lon, lat, total, logn) %>%
    filter(logn >= 1) %>%
    usmap_transform()

################################################################################

################################### Plot #######################################
# Plot the distribution
p1 <- plot_usmap(regions = 'counties', 
                 data = urban_establishments_by_county,
                 size = 0.025,
                 linetype = 0,
                 color = 'white',
                 values = 's') + 
    scale_fill_continuous(name = 'Establishments', 
                          trans = 'log10',
                          breaks = c(10, 100, 1000, 10000, 100000),
                          label = comma,
                          guide = guide_colourbar(order = 0),
                          low = '#BADAF1',
                          high = '#00207F',
                          na.value = '#E5E5E5') + 
    geom_point(data = restaurants_by_county %>% filter(lon.1 < quantile(lon.1, 0.99, na.rm = T) 
                                       & lon.1 > quantile(lon.1, 0.01, na.rm = T)),
               aes(x = lon.1, y = lat.1, size = total),
               color = '#e70300', alpha = 0.5) +
    scale_size_continuous(name = 'Restaurants', 
                          range = c(0.1, 2),
                          label = comma, 
                          breaks = c(1000, 5000, 10000, 20000),
                          guide = guide_legend(order = 1, reverse=T)) + 
    theme(panel.background = element_rect(color = 'black',
                                          fill = 'white',
                                          linetype = 0),
          text = element_text(family="Times")
          ) + 
    theme(legend.position = c(0.85, 0.025), 
          legend.box = 'vertical',
          legend.background = element_rect(colour = "black", size = 0.25),
          legend.key.size = unit(0.075, 'inches'),
          plot.margin = margin(0, 0, 0, 0, 'pt')) +
    my_theme  

ggsave(file.path(plots_folder_path, 'geographical_distribution.pdf'), 
       plot = p1, 
       device = cairo_pdf, 
       width = 4.85, 
       height = 3)
embed_fonts(file.path(plots_folder_path, 'geographical_distribution.pdf'))


# Plot without establishment count
urban_establishments_by_county <- urban_establishments_by_county %>% 
    mutate(s = 1)
p2 <- plot_usmap(regions = 'counties', 
                 data = urban_establishments_by_county,
                 size = 0.025,
                 linetype = 0,
                 color = 'white',
                 values = 's') + 
    scale_fill_continuous(high = mycolorscheme3[2], low = mycolorscheme3[2], 
                      guide = FALSE, na.value = '#E5E5E5') + 
    geom_point(data = restaurants_by_county %>% filter(lon.1 < quantile(lon.1, 0.99, na.rm = T) 
                                                       & lon.1 > quantile(lon.1, 0.01, na.rm = T)),
               aes(x = lon.1, y = lat.1, size = total),
               color = mycolorscheme3[1], alpha = 0.75) +
    scale_size_continuous(name = 'Restaurants', 
                          range = c(0.1, 2),
                          label = comma, 
                          breaks = c(1000, 5000, 10000, 20000),
                          guide = guide_legend(order = 1, reverse=T)) + 
    theme(panel.background = element_rect(color = 'black',
                                          fill = 'white',
                                          linetype = 0),
          text = element_text(family="Times")
    ) + 
    theme(legend.position = c(0.85, 0.025), 
          legend.box = 'vertical',
          legend.background = element_rect(colour = "black", size = 0.25),
          legend.key.size = unit(0.075, 'inches'),
          plot.margin = margin(0, 0, 0, 0, 'pt')) +
    my_theme  

ggsave(file.path(plots_folder_path, 'geographical_distribution_restaurants_only.pdf'), 
       plot = p2, 
       device = cairo_pdf, 
       width = 4.85, 
       height = 3)
embed_fonts(file.path(plots_folder_path, 'geographical_distribution_restaurants_only.pdf'))

################################################################################