###############################################################################
#
# FILE: estimate_logit.R
#
# BY: Dmitry Sedov 
#
# DATE: Fri May 8 2020
#
# DESC: This code contains the code for the estimation of logit models.
#
# IN:
#     0. CBG-level choices
#     1. Restaurant-level features and instuments.
#
###############################################################################


################################ Libraries ####################################

library(readr)
library(dplyr)
library(lfe)
library(ggplot2)
library(gsubfn)
library(data.table)
library(stargazer)

###############################################################################


################################ Constants ####################################

days <- 31
input_folder <- '/home/quser/project_dir/urban/data/output/spatial-demand/main_demand'
output_folder <- '/home/quser/project_dir/urban/data/output/spatial-demand/main_demand'
summary_file_path <- file.path(input_folder, 'minimization_summary_optimized.csv')

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 6),
                  legend.title = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.text = element_text(size = 6),
                  axis.title = element_text(size = 8)
)
mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

###############################################################################


################################# Functions ###################################

restaurants_file_path <- function(x) {
  folder_name <- paste0('cbsa', x)
  file_name <- paste0('restaurants', x, '.csv')
  file_path <- file.path(input_folder, folder_name, file_name)
  return(file_path)
}

get_choices_distances <- function(x) {
  folder_name <- paste0('cbsa', x)
  choices_file_name <- paste0('choices', x, '.csv')
  pairs_file_name <- paste0('pairs', x, '.csv')
  choices_file_path <- file.path(input_folder, folder_name, choices_file_name)
  pairs_file_path <- file.path(input_folder, folder_name, pairs_file_name)
  choices <- read_csv(choices_file_path, col_types = cols(home_cbg = col_character()))
  pairs <- read_csv(pairs_file_path, col_types = cols(cbg = col_character())) %>%
    rename(home_cbg = cbg)
  choices <- left_join(choices, pairs) %>% select(sname_place_id,
                                                  home_cbg, 
                                                  distance,
                                                  raw_visit_counts, 
                                                  raw_visitor_counts, 
                                                  visitors_from_home_cbg)
  return(choices)
}

###############################################################################


################################# Main code ###################################

# Read the minimization summary file
minimization_summary_optimized <- read_csv(summary_file_path, col_names = FALSE)

# Get choices
#choices_list <- lapply(rev(minimization_summary_optimized$X1, get_choices_distances))
#choices <- bind_rows(choices_list)
#write_csv(choices, path = file.path(output_folder, 'choices_all.csv'), na = '')
choices <- read_csv(file.path(output_folder, 'choices_all.csv'), 
                    col_types = cols(home_cbg = col_character()))

choices <- choices %>% mutate(distance_km = distance / 1000) %>%
  mutate(distance_km_2 = distance_km ^ 2)
choices <- choices %>% 
  mutate(visits_from_home_cbg = raw_visit_counts * (visitors_from_home_cbg / raw_visitor_counts))
choices <- choices %>% mutate(lsj = log(visits_from_home_cbg))

# Get restaurant features
all_rest_paths <- sapply(minimization_summary_optimized$X1, restaurants_file_path)
rest_list <- lapply(all_rest_paths, read_csv, 
                    col_types = cols(zip_code = col_character(), 
                                     r_cbg = col_character(), 
                                     r_cbsa = col_character()))
restaurants <- bind_rows(rest_list)

# Get instruments
restaurants_neighbors_features <- read_csv(file.path(input_folder, 
                                                     'restaurants_neighbors_features.csv')
)

# Clean variables
restaurants <- restaurants %>% mutate(brands = replace(brands, is.na(brands), 'none'))
restaurants <- restaurants %>% mutate(category1 = replace(category1, is.na(category1), 'none'))

sample <- restaurants %>% select(-raw_visit_counts)

sample <- sample %>% 
  mutate(price = replace(price, price %in% c(3, 4), 2))

sample <- sample %>% 
  mutate(price = as.character(price)) %>%
  mutate(price = factor(price)) %>% 
  mutate(price = relevel(price, ref = '1')) 

# Group small brands an categories into one
sample <- sample %>% group_by(brands) %>% 
  mutate(n_brands = n()) %>% 
  ungroup() %>%
  mutate(brands = replace(brands, n_brands <= 100, 'none')) %>%
  select(-n_brands)

sample <- sample %>% group_by(category1) %>% 
  mutate(n_category1 = n()) %>% 
  ungroup() %>%
  mutate(category1 = replace(category1, n_category1 <= 100, 'none')) %>%
  select(-n_category1)

sample <- sample %>% mutate(rating_2 = rating ^ 2)
sample <- sample %>% mutate(area_m2_2 = area_m2 ^ 2)

# Create the missing indicators for time and rating
sample <- sample %>% 
  mutate(time_not_avail = is.na(total_minutes_open)) %>% 
  mutate(total_minutes_open = replace(total_minutes_open, time_not_avail, 0))
sample <- sample %>% 
  mutate(rating_not_avail = is.na(rating)) %>% 
  mutate(rating = replace(rating, rating_not_avail, 0), 
         rating_2 = replace(rating_2, rating_not_avail, 0))

choices <- choices %>% left_join(sample)

choices <- choices %>% 
  group_by(home_cbg, category1) %>%
  mutate(group_visits = sum(visits_from_home_cbg, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(within_group = log(visits_from_home_cbg / group_visits))
choices <- choices %>% 
  group_by(home_cbg, category1) %>% 
  mutate(group_distance = sum(distance_km, na.rm = T), 
         n_home_category = n()) %>% 
  ungroup() %>% 
  mutate(group_distance = (group_distance - distance_km) / (n_home_category - 1))

distance_threshold <- quantile(choices$distance_km, probs = c(0.99), na.rm = T)
choices <- choices %>% filter(distance_km <= distance_threshold)

fit_logit <- felm(data = choices, lsj ~ distance_km + factor(price) +  factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby + area_m2 | home_cbg + brands + category1 | 0 | home_cbg + sname_place_id)
fit_nested_logit_ols <- felm(data = choices, lsj ~ distance_km + factor(price) +  factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby + area_m2 + within_group | home_cbg + brands + category1 | 0 | home_cbg + sname_place_id)
fit_nested_logit_iv <- felm(data = choices, lsj ~ distance_km + factor(price) +  factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby + area_m2 | home_cbg + brands + category1 | (within_group ~ group_distance) | home_cbg + sname_place_id)
fit_fe <- felm(data = choices, lsj ~ distance_km | home_cbg + sname_place_id | 0 | home_cbg + sname_place_id)

stargazer(fit_logit, 
          fit_fe, 
          fit_nested_logit_ols, 
          fit_nested_logit_iv,
          type = 'latex',
          omit = c(2, 4, 8, 9, 10, 11, 12),
          omit.stat = c('rsq', 'adj.rsq', 'ser'),
          add.lines = list(`Home CBG FE` = c('Home CBG FE', 'Yes', 'Yes', 'Yes', 'Yes'), 
                           `Rest FE` = c('Rest FE', 'No', 'Yes', 'No', 'No'),
                           `Brand FE` = c('Brand FE', 'Yes', 'No', 'Yes', 'Yes'), 
                           `Category FE` = c('Category FE', 'Yes', 'No', 'Yes', 'Yes'), 
                           `Open` = c('Time controls', 'Yes', 'No', 'Yes', 'Yes'), 
                           `Nearby` = c('Location controls', 'Yes', 'No', 'Yes', 'Yes'), 
                           `SE` = c('SE', 'cl(Home, Rest)', 'cl(Home, Rest)', 'cl(Home, Rest)', 'cl(Home, Rest)'), 
                           `F` = c('F-stat', '', '', '', '222.1')
          ),
          digits = 3, 
          digits.extra = 0,
          align = TRUE)

###############################################################################