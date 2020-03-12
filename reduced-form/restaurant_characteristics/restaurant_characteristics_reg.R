################################################################################
################################################################################
#
# FILE: restaurants_characterstics_reg.R
#
# BY: Dmitry Sedov 
#
# CREATED: Tue Feb 25 2020
#
# DESC: This file creates the dataset for cross-section regressions to run in Stata.
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
library(lfe)
library(stargazer)
library(haven)

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
data_folder_path = '/Users/muser/dfolder/Research/urban/data/output/reduced-form'

# Visits data vintage
year = '2018'
month = 'October'

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 6),
                  legend.title = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.text = element_text(size = 6),
                  axis.title = element_text(size = 8)
)
mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#ffe200', '#722ab5')

################################################################################

################################ Data import ###################################

# Import the cbg data
cbgs <- read_csv(file.path(input_folder_path, 'data_cbg.csv'))

# Import demographics info
census_cbg_extract <- read_csv(file.path(input_folder_path, 'census_cbg_extract.csv'),
                               col_types = cols_only(cbg_id = col_guess(),
                                                     median_hh_income = col_guess(), 
                                                     somecoll_over25y_pop = col_guess()
                               )
)

# Import restaurants data
restaurants <- read_csv(file.path(input_folder_path, 'data_restaurants.csv'))

################################################################################

############################## Prepare data ####################################

# Join the cbg data
cbgs <- cbgs %>% left_join(census_cbg_extract, by = c('cbg' = 'cbg_id'))

# Set price to missing if it is -1
restaurants <- restaurants %>% mutate(price = ifelse(price == -1, NA, price))

# Replace missing (null) categories with NAs
restaurants <- restaurants %>% mutate(categories = ifelse(categories == 'null', 
                                                          NA, 
                                                          categories))

# Transform the total minutes open variable
restaurants <- restaurants %>%
    mutate(perc_time_open = 100 * total_minutes_open / (24 * 60 * 31)) %>%
    mutate(perc_time_open = replace(perc_time_open,  
                                    which(perc_time_open <= 0.1 | perc_time_open >= 100.1), 
                                    NA))

# Encode 'urban' restaurants
restaurants <- restaurants %>%
    mutate(urban = !is.na(cbsa)) 

# Count restaurant's categories
restaurants <- restaurants %>%
    mutate(n_categories = sapply(categories, 
                                 function(x) {
                                     ifelse(is.na(x), 
                                            NA,
                                            ifelse(x == "[]",
                                                   0,
                                                   nrow(fromJSON(x))
                                            )
                                     )
                                 }
    )
    )

# Extract restaurant's first category
restaurants <- restaurants %>%
    mutate(category1 = sapply(categories, 
                              function(x) {
                                  ifelse(is.na(x) || x == "[]",
                                         NA,
                                         fromJSON(x)[1, 'title']
                                  )
                              }
                              )
           )

rest_for_reg <- left_join(restaurants, cbgs, by = c('cbg' = 'cbg')) %>% 
    replace_na(list(brands = 'None', 
                    category1 = 'None')) %>% 
    mutate(ct = factor(ct), 
           brands = factor(brands), 
           category1 = factor(category1)) %>%
    mutate(price_bin = case_when(price == 1 ~ 'low',
                                 price == 2 ~ 'medium',
                                 price %in% c(2,3,4) ~ 'high',
                                 TRUE ~ 'na'), 
           rating_bin = case_when(rating %in% c(0, 1.0, 1.5, 2.0) ~ 'low',
                                  rating %in% c(2.5, 3.0, 3.5) ~ 'medium',
                                  rating %in% c(4.0, 4.5, 5.0) ~ 'high',
                                  TRUE ~ 'na')) %>% 
    mutate(price_bin = factor(price_bin, levels = c('low', 'medium', 'high', 'na')), 
           rating_bin = factor(rating_bin, levels = c('low', 'medium', 'high', 'na')
           )
    )

################################################################################

############################ Fit the models ####################################

fit1 <- felm(data = rest_for_reg, 
             raw_visit_counts ~ est_number + 
                 number_devices_residing +
                 price_bin * rating_bin +
                 factor(naics_code) + 
                 area_m2.x +
                 phone + median_hh_income | brands + category1 | 0 | ct)

fit2 <- felm(data = rest_for_reg, 
             raw_visit_counts ~ est_number + 
                 number_devices_residing +
                 price_bin * rating_bin +
                 factor(naics_code) + 
                 area_m2.x +
                 phone + median_hh_income| ct + brands + category1 | 0 | ct)

fit3 <- felm(data = rest_for_reg, 
             raw_visit_counts ~ est_number + 
                 number_devices_residing +
                 price_bin * rating_bin +
                 factor(naics_code) + 
                 area_m2.x +
                 phone + 
                 rest_number + median_hh_income| brands + category1 | 0 | ct)

fit4 <- felm(data = rest_for_reg, 
             raw_visit_counts ~ est_number + 
                 number_devices_residing +
                 price_bin * rating_bin +
                 factor(naics_code) + 
                 area_m2.x +
                 phone + 
                 rest_number + median_hh_income | ct + brands + category1 | 0 | ct)

stargazer(fit1, fit2, fit3, fit4, type = 'text')

################################################################################

##################### Save data for Stata regressions ##########################

rest_for_reg <- rest_for_reg %>% rename(area_m2 = area_m2.x, area_cbg = area_m2.y)
rest_for_reg <- rest_for_reg %>% rename(cbsa = cbsa.x, cbsa_cbg = cbsa.y)

write_dta(data = rest_for_reg %>% select(-categories),
          path = file.path(data_folder_path,
                           'restaurants_cross_section.dta'))

################################################################################



