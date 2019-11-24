################################################################################
#
# FILE: visits-county-distribution.R
#
# BY: Dmitry Sedov 
#
# CREATED: Thu No1 20 2019 
#
# DESC: This file creates a histogram of visits per person distribution.
#
################################################################################

# Import libraries
library(readr) # Importing from csv and other sources
library(dplyr) # Chained sql-style table operations
library(usmap) # US population data
library(statebins) # Statebin maps
library(extrafont)
library(ggplot2)
library(stringr)
library(noncensus)

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 10),
                  legend.title = element_text(size= 16),
                  plot.title = element_text(hjust = 0.5, size = 24)
)

# Set the data path
data_path <- '/Users/muser/dfolder/Research/urban/data/'
# Set the plots path
plots_path <- '/Users/muser/dfolder/Research/urban/output/plots/'

# Set the paths
visits_by_state_path <- file.path(data_path, 'visits_by_state.csv')
visits_by_zip_path <- file.path(data_path, 'visits_by_zip.csv')

# Import data
visits_by_zip <- read_csv(visits_by_zip_path, 
                          col_types = cols(zip_code = col_integer(),
                                           visits = col_integer()
                          )
)

# County-level data, zip code to county mapping
data('zip_codes')
data('counties')

zip_codes <- zip_codes %>% 
    select(zip, fips) %>%
    mutate(zip = as.integer(zip))

counties <- counties %>%
    mutate(fips = paste0(state_fips, county_fips)) %>%
    transmute(fips = as.integer(fips),
              population = population)

visits_by_county <- visits_by_zip %>% 
    left_join(zip_codes,
              by = c('zip_code' = 'zip')) %>% 
    left_join(counties) %>% 
    mutate(visits_per_person = visits / population,
           log_visits_per_person = log10(visits_per_person),
           fips = str_pad(fips, 5, pad = "0")) 

p <- ggplot(visits_by_county, aes(x = log_visits_per_person)) +
    geom_histogram(aes(fill = ..count..)) +
    theme(panel.background = element_rect(color = "black", 
                                          fill = "white", 
                                          linetype = 0),
          axis.title.x = element_text(size = 18, color = 'black'),
          axis.title.y = element_blank(),
          legend.title = element_text(size = 14),
          axis.text = element_text(size = 12, color = 'grey30'),
          legend.text = element_text(size = 12, color = 'grey30'),
          text = element_text(size = 18,  family="CMU Sans Serif")
    ) +
    labs(x = 'Log10 visits per person', fill = 'N(Counties)')

ggsave(filename = file.path(plots_path, 'counties_visits_hist.pdf'),
       device = cairo_pdf,
       plot = p, 
       width = 7,
       height = 4.5)


