###############################################################################
#
# FILE: visits_area.R
#
# BY: Dmitry Sedov 
#
# DATE: Sun May 10 2020
#
# DESC: This code contains the code to estimate the extra visits from an 
#       an increase in area.
#
# IN:
#     0. CBSA - the identifier of the urban area.
#     1. Rho - the disutility of distance.
#     2. c_a - the coefficient on the area.
#
###############################################################################


################################ Libraries ####################################

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gsubfn))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(stargazer))

###############################################################################


################################ Constants ####################################

days = 31
input_folder = '/home/quser/project_dir/urban/data/output/spatial-demand/main_demand'
population_folder = '/home/quser/project_dir/urban/data/output/descriptive'
output_folder = '/home/quser/project_dir/urban/data/output/entry/markups'

###############################################################################

################################# Functions ###################################

PrepareData <- function(cbsa) {
  # Function to import and prepare single-CBSA dataset for estimation of extra 
  # visits from extra area.
  #
  # IN:
  #     cbsa - string with the cbsa number
  # OUT:
  #     pairs, deltas
  
  # Set the cbsa folder name
  cbsa_folder_name <- paste0('cbsa', cbsa)
  
  # Import the cbg-restaurant pairs
  pairs_file_name <- paste0('pairs', cbsa, '.csv')
  pairs_file_path <- file.path(input_folder, cbsa_folder_name, pairs_file_name)
  pairs <- read_csv(pairs_file_path, col_types = cols(cbg = col_character()))
  
  # Import the deltas
  deltas_file_name <- paste0('deltas_optimized', cbsa, '.csv')
  deltas_file_path <- file.path(input_folder, cbsa_folder_name, deltas_file_name)
  suppressMessages(deltas <- read_csv(deltas_file_path))
  
  # Import the restaurants
  restaurants_file_name <- paste0('restaurants', cbsa, '.csv')
  restaurants_file_path <- file.path(input_folder, cbsa_folder_name, restaurants_file_name)
  suppressMessages(restaurants <- read_csv(restaurants_file_path))
  
  # Import the total population count
  population_file_name <- 'cbg_population.csv'
  population_file_path <- file.path(population_folder, population_file_name)
  suppressMessages(population <- read_csv(population_file_path, 
                                          col_types = cols(home_cbg = col_character())))
  
  # Clean the pairs data
  pairs <- pairs %>%
    rename(home_cbg = cbg) %>% 
    select(-r_cbsa) 
  if ('cbg_cbsa_x' %in% colnames(pairs)) {
    pairs <- pairs %>% 
      rename(cbg_cbsa = cbg_cbsa_x) %>%
      select(-cbg_cbsa_y)
  }
  pairs <- pairs %>% mutate(distance_km = distance / 1000) %>%
    mutate(distance_km_2 = distance_km ^ 2)
  pairs <- pairs %>% filter(!is.na(number_devices_residing))
  pairs <- pairs %>% left_join(population)
  
  # Conversion to data tables
  pairs <- as.data.table(pairs)
  deltas <- as.data.table(deltas)
  restaurants <- as.data.table(restaurants)
  setkey(pairs, sname_place_id)
  setkey(deltas, sname_place_id)
  setkey(restaurants, sname_place_id)
  
  # Conversion to data tables and set the variables in the outer scope
  value <- list(pairs = pairs, 
                deltas = deltas,
                restaurants = restaurants)
  return(value)
}


visits_if_area_increased <- function(sg_id, area_coef) {
  # Function to compute the number of visits for a given restaurant with
  # an increase area.
  # IN:
  #   1. sg_id - restaurant identifier.
  #   2. area_coef - the coefficient on the area.
  # OUT:
  #   1. vector: c(sg_id, counterfactual_visits)
  #
  
  # Copy the pairs - deltas joined dataframe 
  pairs_deltas <- copy(pairs_deltas_iteration)
  
  pairs_deltas[.(sg_id), delta := delta + area_coef]
  
  pairs_deltas[,
               `:=`(
                 choice_utility = exp(delta + rho1 * distance_km + rho2 * distance_km_2)
               )
               ]
  pairs_deltas[,
               `:=`(
                 total_utility = 1 + sum(choice_utility)),
               by = .(home_cbg)
               ]
  pairs_deltas[,
               choices_made := total_pop * days * (choice_utility / total_utility)
               ]
  pairs_deltas <- pairs_deltas[,
                               .(predicted_choices = sum(choices_made)),
                               by = .(sname_place_id)]
  value <- pairs_deltas[sname_place_id == sg_id, predicted_choices]
  
  no_change <- visits[sname_place_id == sg_id, visits]
  
  marginal_visits <- value - no_change
  # Save summary of the results
  one_line <- paste(sg_id, as.character(no_change), 
                    as.character(value), 
                    as.character(marginal_visits),
                    sep = ',')
  write(one_line, file = online_results_file_path, append = TRUE)
  return(list(sname_place_id = sg_id, altered_visits = value))
}


visits_if_log_area_increased <- function(sg_id, area_coef, added_unit) {
  # Function to compute the number of visits for a given restaurant with
  # an increased area.
  # IN:
  #   1. sg_id - restaurant identifier.
  #   2. area_coef - the coefficient on the LOG of area.
  # OUT:
  #   1. vector: c(sg_id, counterfactual_visits)
  #
  
  # Copy the pairs - deltas joined dataframe 
  pairs_deltas <- copy(pairs_deltas_iteration)
  
  pairs_deltas[.(sg_id), delta := delta + area_coef * (log(area_m2 + added_unit) - log(area_m2))]
  
  pairs_deltas[,
               `:=`(
                 choice_utility = exp(delta + rho1 * distance_km + rho2 * distance_km_2)
               )
               ]
  pairs_deltas[,
               `:=`(
                 total_utility = 1 + sum(choice_utility)),
               by = .(home_cbg)
               ]
  pairs_deltas[,
               choices_made := total_pop * days * (choice_utility / total_utility)
               ]
  pairs_deltas <- pairs_deltas[,
                               .(predicted_choices = sum(choices_made)),
                               by = .(sname_place_id)]
  value <- pairs_deltas[sname_place_id == sg_id, predicted_choices]
  
  no_change <- visits[sname_place_id == sg_id, visits]
  
  marginal_visits <- value - no_change
  
  cat(sg_id, marginal_visits, '\n')
  return(list(sname_place_id = sg_id, marginal_visits = marginal_visits))
}

###############################################################################


################################## Main code ##################################

args <- commandArgs(trailingOnly = TRUE)
cbsa <- as.character(args[1])
rho1 <- as.numeric(args[2])
rho2 <- 0
c_a <- as.numeric(args[3])
cbsa_folder_name <- paste0('cbsa', cbsa)
cat('cbsa: ', cbsa, '\n')
# Create directory if it doesn't exist yet
dir.create(file.path(output_folder, cbsa_folder_name), showWarnings = FALSE)
# Set 'online-results' path
online_results_file_name <- paste0('online_visits_altered', cbsa, '.csv')
online_results_file_path <- file.path(output_folder, cbsa_folder_name, online_results_file_name)

# Prepare data
list[pairs, deltas] <- PrepareData(cbsa)

# Compute the predicted number of visits
pairs_deltas_main <- merge(pairs,
                           deltas,
                           by = 'sname_place_id',
                           all.x = T)
pairs_deltas_main[,
                  `:=`(
                    choice_utility =  exp(delta + rho1 * distance_km + rho2 * distance_km_2)
                  )
                  ]
pairs_deltas_main[,
                  `:=`(
                    total_utility = 1 + sum(choice_utility)),
                  by = .(home_cbg)
                  ]
pairs_deltas_main[,
                  choices_made := total_pop * days * (choice_utility / total_utility)
                  ]
visits <- pairs_deltas_main[,
                            .(visits = sum(choices_made)),
                            by = .(sname_place_id)]

# Join pairs and deltas for iteration over sname id, changing area
pairs_deltas_iteration <- merge(pairs, 
                                deltas,
                                by = 'sname_place_id', 
                                all.x = T)

# Compute visits when extra area unit is added / subtracted
altered_visits_p1 <- lapply(deltas$sname_place_id, 
                            function(x) {visits_if_area_increased(x, c_a)})
# altered_visits_m1 <- lapply(deltas$sname_place_id, 
#                            function(x) {visits_if_area_increased(x, -c_a)})

# Convert the list to dataframe
altered_visits_p1 <- rbindlist(lapply(altered_visits_p1, as.data.frame, stringsAsFactors = FALSE))
# altered_visits_m1 <- rbindlist(lapply(altered_visits_m1, as.data.frame, stringsAsFactors = FALSE))

# Rename columns
altered_visits_p1 <- altered_visits_p1 %>% rename(visits_p1 = altered_visits)
# altered_visits_m1 <- altered_visits_m1 %>% rename(visits_m1 = altered_visits)

visits <- visits %>% left_join(altered_visits_p1) # %>% left_join(altered_visits_m1) 
visits <- visits %>% mutate(diff_p1 = visits_p1 - visits) # %>% mutate(diff_m1 = visits - visits_m1)

# Export the altered visits
visits_altered_file_name <- paste0('all_visits_altered', cbsa, '.csv')
visits_altered_file_path <- file.path(output_folder, cbsa_folder_name, visits_altered_file_name)
write_csv(visits, visits_altered_file_path)

###############################################################################