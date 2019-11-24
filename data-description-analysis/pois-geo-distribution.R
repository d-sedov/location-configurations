################################################################################
#
# FILE: pois-geo-distribution.R
#
# BY: Dmitry Sedov 
#
# CREATED: Wed Nov 20 2019 
#
# DESC: This file creates a countybinplot of POIs.
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
pois_by_state_path <- file.path(data_path, 'pois_by_state.csv')
pois_by_zip_path <- file.path(data_path, 'pois_by_zip.csv')
visits_by_state_path <- file.path(data_path, 'visits_by_state.csv')
visits_by_zip_path <- file.path(data_path, 'visits_by_zip.csv')

# Import data
pois_by_state <- read_csv(pois_by_state_path,
                          col_types = cols(state = col_character(),
                                           pois = col_integer()
                                           )
                          )
pois_by_zip <- read_csv(pois_by_zip_path, 
                        col_types = cols(zip_code = col_integer(),
                                         pois = col_integer()
                                         )
                        )
visits_by_state <- read_csv(visits_by_state_path, 
                            col_types = cols(state = col_character(),
                                             visits = col_integer()
                                             )
                            )
visits_by_zip <- read_csv(visits_by_zip_path, 
                          col_types = cols(zip_code = col_integer(),
                                           visits = col_integer()
                                           )
                          )

# Import the state population data
data("statepop")

pois_by_state <- pois_by_state %>% 
    mutate(state = toupper(state)) %>%
    inner_join(statepop, by = c('state' = 'abbr')) %>%
    mutate(pois_per_person = pois / pop_2015)

p <- statebins(pois_by_state,
               value_col = 'pois_per_person',
               legend_title = 'POIs per person',
               legend_position = 'bottom') + 
    labs(title = 'Distribution of POIs across states') + 
    theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = file.path(plots_path, 'statebins_pois.pdf'), plot = p)

visits_by_state <- visits_by_state %>% 
    mutate(state = toupper(state)) %>% 
    group_by(state) %>% 
    summarise(visits = sum(visits)) %>%
    inner_join(statepop, by = c('state' = 'abbr')) %>%
    mutate(visits_per_person = visits / pop_2015)

p <- statebins_continuous(visits_by_state,
               value_col = 'visits_per_person',
               legend_title = 'Visits per person',
               legend_position = 'bottom') + 
    labs(title = 'Distribution of visits across states') + 
    theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = file.path(plots_path, 'statebins_visits.pdf'), plot = p)

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

pois_by_county <- pois_by_zip %>% 
    left_join(zip_codes,
              by = c('zip_code' = 'zip')) %>% 
    left_join(counties) %>% 
    mutate(pois_per_person = pois / population,
           log_pois_per_person = log10(pois_per_person),
           fips = str_pad(fips, 5, pad = "0")) 

p <- plot_usmap(regions = "counties", 
                data = pois_by_county,
                size = 0.01,
                linetype = 0,
                color = 'white',
                values = 'log_pois_per_person') + 
    theme(panel.background = element_rect(color = "white", 
                                          fill = "white", 
                                          linetype = 0),
          text = element_text(family="CMU Sans Serif"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10, color = 'grey30')
          ) + 
    scale_fill_continuous(name = "Log (POIs\nper person)") + 
    theme(legend.position = 'right')  +
    my_theme

ggsave(filename = file.path(plots_path, 'counties_pois.pdf'),
       device = cairo_pdf,
       plot = p, 
       width = 7,
       height = 3.5)

visits_by_county <- visits_by_zip %>% 
    left_join(zip_codes,
              by = c('zip_code' = 'zip')) %>% 
    left_join(counties) %>% 
    mutate(visits_per_person = visits / population,
           log_visits_per_person = log10(visits_per_person),
           fips = str_pad(fips, 5, pad = "0")) 

p <- plot_usmap(regions = "counties", 
                size = 0.00, 
                linetype = 0,
                data = visits_by_county, 
                values = 'log_visits_per_person') + 
    theme(panel.background = element_rect(color = "black", 
                                          fill = "white", 
                                          linetype = 0),
          text = element_text(size = 16,  family="CMU Sans Serif")
          ) + 
    scale_fill_continuous(name = "Log visits\nper person") + 
    theme(legend.position = 'right') + 
    my_theme

ggsave(filename = file.path(plots_path, 'counties_visits.pdf'),
       device = cairo_pdf,
       plot = p, 
       width = 7,
       height = 4.5)