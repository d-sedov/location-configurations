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
my_theme <- theme(legend.text = element_text(size = 14),
                  legend.title = element_text(size= 16),
                  plot.title = element_text(hjust = 0.5, size = 18)
)
mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#ffe200', '#722ab5')

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
    summarise(logn = log10(sum(n))) %>%
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
                 values = 'logn') + 
    scale_fill_continuous(name = 'Log establishments', 
                          guide = guide_colourbar(order = 0),
                          low = '#BADAF1',
                          high = '#00207F',
                          na.value = '#E5E5E5') + 
    geom_point(data = restaurants_by_county %>% filter(lon.1 < quantile(lon.1, 0.99, na.rm = T) 
                                       & lon.1 > quantile(lon.1, 0.01, na.rm = T)),
               aes(x = lon.1, y = lat.1, size = total),
               color = '#e70300', alpha = 0.5) +
    scale_size_continuous(name = 'Restaurants', guide = guide_legend(order = 1)) + 
    theme(panel.background = element_rect(color = 'black',
                                          fill = 'white',
                                          linetype = 0),
          text = element_text(family="Times")
          ) + 
    theme(legend.position = c(0.85, 0.025), 
          legend.box = 'vertical',
          legend.background = element_rect(colour = "black", size = 0.25)) +
    my_theme  

ggsave(file.path(plots_folder_path, 'geographical_distribution.pdf'), 
       plot = p1, 
       device = cairo_pdf, 
       width = 12, 
       height = 8)

################################################################################