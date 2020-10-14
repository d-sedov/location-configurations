###############################################################################
#
# FILE: excessive_entry.R
#
# BY: Dmitry Sedov 
#
# DATE: Wed Sep 9 2020
#
# DESC: This code contains the code to determine the percentage of firms that 
#       can be removed without harming consumer welfare or total industry 
#       profits.
#
#
###############################################################################


################################ Libraries ####################################

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gsubfn))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(stargazer))
library(kableExtra)
library(ggridges)
library(extrafont)
library(lfe)

###############################################################################


################################ Constants ####################################

max_time <- 3.75 * 3600

days <- 31

# Output file path
output_file_path <- file.path('/home/quser/project_dir/urban/data/output', 
                              'counterfactuals', 
                              'excessive_entry', 
                              'deleted_firms.csv')

# Variuos input paths
input_folder <- '/home/quser/project_dir/urban/data/output/spatial-demand/main_demand'
markup_folder <- '/home/quser/project_dir/urban/data/output/entry/markups'
population_folder <- '/home/quser/project_dir/urban/data/output/descriptive'
rent_folder_path <- '/home/quser/project_dir/urban/data/output/rent'

# The rho-minimization summary path
summary_file_path = file.path(input_folder, 'minimization_summary_optimized.csv')
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

###############################################################################


################################ Functions ####################################

PrepareData <- function(cbsa) {
  # Function to import and prepare single-CBSA dataset for estimation
  #
  # IN:
  #     cbsa - string with the cbsa number
  # OUT:
  #     pairs, restaurants, locations data tables
  #     rho1 set in the global environment
  
  # Read in the distance cost parameter for the current CBSA
  minimization_summary_optimized <- read_csv(summary_file_path, col_names = FALSE)
  rho1 <<- minimization_summary_optimized$X2[minimization_summary_optimized$X1 == cbsa]
  rho2 <<- 0
  
  # The folder conatining CBSA-related files
  cbsa_folder_name <- paste0('cbsa', cbsa)
  
  # Read the info on restaurants, deltas and markups
  restaurants <- read_csv(file.path(input_folder, 
                                    cbsa_folder_name,
                                    paste0('restaurants',
                                           cbsa,
                                           '.csv')),
                          col_types = cols(zip_code = col_character(),
                                           r_cbg = col_character())
  )
  deltas <- read_csv(file.path(input_folder, 
                               cbsa_folder_name,
                               paste0('deltas_optimized', 
                                      cbsa, 
                                      '.csv')
  )
  )
  markups <- read_csv(file.path(markup_folder, 
                                'restaurants_estimated_markups.csv'),
                      col_types = cols_only(sname_place_id = col_character(),
                                            estimated_markup = col_double())
  )
  
  # Remove outliers
  #markups <- markups %>%  
  #  mutate_at(vars(estimated_markup), 
  #            funs(remove_outliers))
  
  # Import rent data
  loopnet_data <- read_csv(file.path(rent_folder_path, 
                                     'loopnet_listings.csv'))
  crexi_data <- read_csv(file.path(rent_folder_path, 
                                   'crexi_listings.csv')) %>% 
    select(-X1) %>% mutate(source = 'Crexi')
  commercialexchange_data <- read_csv(file.path(rent_folder_path, 
                                                'commercialexchange_listings.csv')) %>% 
    select(-X1) %>% mutate(source = 'CommercialExchange')
  commercialcafe_data <- read_csv(file.path(rent_folder_path, 
                                            'commercialcafe_listings.csv')) %>% 
    select(-X1) %>% mutate(source = 'CommercialCafe')
  
  all_rent_data <- loopnet_data %>% 
    select(-subtype) %>% 
    bind_rows(crexi_data) %>%
    bind_rows(commercialexchange_data) %>%
    mutate(source = 'All w/o CommercialCafe')
  
  rent_by_zip <- all_rent_data %>% 
    group_by(zip_code) %>%
    summarise(mean_rate = mean(rate, na.rm = TRUE))
  
  # Get the rent on the restaurant zip-code level
  restaurants <- left_join(restaurants, rent_by_zip)
  
  # Create the dataset of locations and rents in those locations
  locations <- restaurants %>% select(sname_place_id, mean_rate)
  locations <- locations %>% rename(rent = mean_rate)
  
  # Replace missing rental rates with median
  locations <- locations %>% 
    mutate(rent = ifelse(is.na(rent), 
                         median(rent, na.rm=TRUE), 
                         rent))
  # Create location ids
  locations <- locations %>% 
    mutate(location_id = row_number()) %>%
    select(location_id, everything())
  
  # Get restaurant quality and markup, generate rest_id, join with locations
  restaurants <- restaurants %>% 
    left_join(deltas) %>% 
    left_join(markups) %>%
    rename(markup = estimated_markup) %>% 
    arrange(sname_place_id) %>%
    mutate(rest_id = row_number()) %>% 
    left_join(locations)
  
  # Import the cbg-location pairs
  pairs <- read_csv(file.path(input_folder,
                              cbsa_folder_name,
                              paste0('pairs', cbsa, '.csv')), 
                    col_types = cols(cbg = col_character())) %>%
    rename(home_cbg = cbg) 
  pairs <- pairs %>%
    select(home_cbg, sname_place_id, distance)
  pairs <- pairs %>%
    left_join(locations, by = 'sname_place_id') %>%
    select(home_cbg, location_id, distance) %>%
    mutate(distance_km = distance / 1000) %>%
    select(-distance)
  
  # Import the total population count
  population <- read_csv(file.path(population_folder, 'cbg_population.csv'), 
                         col_types = cols(home_cbg = col_character()))
  
  population <- population %>% filter(home_cbg %in% pairs$home_cbg)
  
  # Match pairs to population count
  pairs <- pairs %>% left_join(population, by = 'home_cbg')
  
  # Compute weighted distance to consumers for each location id
  weighted_distance <- pairs %>% 
    mutate(total_distance = total_pop * distance_km) %>% 
    group_by(location_id) %>%
    summarise(weighted_distance = sum(total_distance) / sum(total_pop))
  
  # Convert to data tables
  pairs <- as.data.table(pairs)
  
  # Save restaurant features
  restaurant_features <- restaurants %>% 
    select(rest_id, delta, markup, price, rating) %>%
    mutate(price = replace(price, price %in% c(-1, 0), NA)) %>%
    mutate(price = replace(price, price %in% c(3, 4), 2))
  
  # Save location features
  location_features <- restaurants %>%
    select(location_id, rest_id, r_cbg) %>% 
    left_join(population, by = c('r_cbg' = 'home_cbg')) %>%
    group_by(r_cbg) %>%
    mutate(restaurant_count = n()) %>%
    ungroup() %>%
    select(location_id, r_cbg, total_pop, restaurant_count) %>%
    left_join(weighted_distance)
  
  # Initial allocation of restaurants over the space
  initial_restaurants <- restaurants %>%
    left_join(weighted_distance) %>% 
    select(location_id, rest_id, delta, area_m2, markup, weighted_distance) 
  
  # Replace missing markups with median
  initial_restaurants <- initial_restaurants %>% 
    mutate(markup = ifelse(is.na(markup), median(markup, na.rm=TRUE), markup))
  initial_restaurants <- initial_restaurants %>% 
    mutate(delta = ifelse(is.na(delta), median(delta, na.rm=TRUE), delta))
  initial_restaurants <- as.data.table(initial_restaurants)
  setkey(initial_restaurants, rest_id)
  
  # Prepare datasets for output
  value <- list(pairs = pairs, 
                initial_restaurants = initial_restaurants, 
                locations = locations, 
                restaurant_features = restaurant_features,
                location_features = location_features)
  return(value)
}


compute_maximum_expected_utility <- function(reordered_restaurants, rho1) {
  # IN:
  #    1. pairs - (home_cbg, location_id)
  #    2. reordered_restaurants - (rest_id, location_id, delta, area, markup)
  #    3. population - (home_cbg, total_pop)
  # OUT: 
  #    1. max_exp_u - maximum expected utility averaged across consumers
  # 
  
  rho2 <- 0
  
  # Match pairs and deltas
  matched_pairs_deltas <- merge(reordered_restaurants,
                                pairs,
                                all.x = TRUE,
                                all.y = FALSE,
                                by = 'location_id', 
                                allow.cartesian = TRUE
  )
  
  # Compute product values on the home cbg level
  matched_pairs_deltas[, 
                       `:=`(
                         choice_utility =  exp(delta + rho1 * distance_km)
                       )
                       ]
  
  # Compute within-home_location max expected utility
  matched_pairs_deltas <- matched_pairs_deltas[,
                                               .(max_exp_u = log(1 + sum(choice_utility)), 
                                                 total_pop = first(total_pop)),
                                               by = .(home_cbg)
                                               ]
  
  matched_pairs_deltas <- as.data.table(matched_pairs_deltas)
  # Average across home location to get the return value
  value <- matched_pairs_deltas[, (sum(max_exp_u * total_pop) / sum(total_pop))]
  return(value)
  
}

compute_profits <- function(reordered_restaurants, rho1) {
  # IN:
  #    1. pairs - (home_cbg, location_id)
  #    2. reordered_restaurants - (rest_id, location_id, delta, area, markup)
  #    3. population - (home_cbg, total_pop)
  # OUT: 
  #    1. total_profits - total profits for firms with markup estimates
  # 
  
  # Compute the predicted number of visits
  rho2 <- 0
  # Match pairs and deltas
  matched_pairs_deltas <- merge(reordered_restaurants,
                                pairs, 
                                all.x = TRUE,
                                all.y = FALSE,
                                by = 'location_id',             
                                allow.cartesian = TRUE
  )
  
  # Compute product values on the home cbg level
  matched_pairs_deltas[, 
                       `:=`(
                         choice_utility =  exp(delta + rho1 * distance_km)
                       )
                       ]
  matched_pairs_deltas[,
                       `:=`(
                         total_utility = 1 + sum(choice_utility)),
                       by = .(home_cbg)
                       ]
  matched_pairs_deltas[,
                       choices_made := total_pop * days * (choice_utility / total_utility)
                       ]
  visits <- matched_pairs_deltas[,
                                 .(visits = sum(choices_made)),
                                 by = .(rest_id)]
  
  # Match visits to data on markups, areas and rent at the location level
  visits <- left_join(visits, 
                      initial_restaurants %>% select(rest_id, area_m2, markup),
                      by = 'rest_id')
  visits <- left_join(visits, 
                      reordered_restaurants %>% select(rest_id, location_id), 
                      by = 'rest_id')
  visits <- left_join(visits, locations, by = 'location_id')
  
  visits <- visits %>% 
    mutate(profit = visits * markup - 10.7 * area_m2 * rent) %>%
    summarise(total_profits = sum(profit, na.rm = TRUE))
  # Average across home location to get the return value
  return(visits$total_profits)
  
}

find_rho <- function(max_exp_u) {
  # IN: 
  #     1. max_exp_u -- average maximum expected utility under counterfactual
  #     
  # OUT:
  #     1. rho -- welfare-equivalent rho under default distribution
  
  # Find the rho that leads to welfare closest to max_exp_u
  value <- distance_cost_variation %>% 
    mutate(abs_diff = abs(max_exp_u_default - max_exp_u)) %>%
    arrange(abs_diff) %>% 
    filter(row_number() == 1)
  
  # Return that rho
  value_low <- value$rho - 0.01
  value_high <- value$rho + 0.01
  
  f <- function(x) { compute_maximum_expected_utility(initial_restaurants, x) - max_exp_u }
  value <- uniroot(f, c(value_low, value_high), tol = 1e-7)
  
  return(value$root)
}


shift_firm <- function(rests, available_firms, available_locations) {
  # Function to move a single firm to a new location
  
  # Select firm to shift:
  firm_to_shift <- sample(x = available_firms, size = 1)
  
  # Change the location of the firm randomly:
  new_location <- sample(x = available_locations, size = 1)
  rests[.(firm_to_shift), location_id := new_location]
  
  # Compute total profits and welfare metrics:
  v1 <- compute_maximum_expected_utility(rests, rho1) 
  v2 <- compute_profits(rests, rho1) 
  
  value <- list(reordered_restaurants_mod = rests, 
                metrics = c(max_exp_u = v1, total_profit = v2))
  
  return(value)
}

switch_firms <- function(rests, available_firms) {
  # Function to switch two firm locations
  
  # Select 2 firms to switch:
  firms_to_shift <- sample(x = available_firms, size = 2)
  firm1_location <- rests[.(firms_to_shift[1]), location_id]
  firm2_location <- rests[.(firms_to_shift[2]), location_id]
  
  # Switch locations:
  rests[.(firms_to_shift[1]), location_id := firm2_location]
  rests[.(firms_to_shift[2]), location_id := firm1_location]
  
  # Compute total profits and welfare metrics:
  v1 <- compute_maximum_expected_utility(rests, rho1) 
  v2 <- compute_profits(rests, rho1) 
  
  value <- list(reordered_restaurants_mod = rests, 
                metrics = c(max_exp_u = v1, total_profit = v2))
  
  return(value)
}

delete_improve_allocation <- function(max_time) {
  # Function to delete a firm randomly, then improve the allocation in both 
  # so that both total profits and consumer welfare are at least as good as in the 
  # status-quo
  
  # Start time recording
  start.time <- Sys.time()
  stop <- FALSE
  
  # Current welfare and total profits
  current_cw <- d_max_exp_u
  current_profits <- d_profits
  
  # Start reordering restaurants
  reordered_restaurants <- copy(initial_restaurants)
  
  # Determine restaurants to be affected by re-configuration
  # Area threshold
  area_threshold <- quantile(initial_restaurants$area_m2, 0.8)
  # Markup threshold
  markup_threshold <- quantile(initial_restaurants$markup, 0.8)
  # Delta threshold
  delta_threshold <- quantile(initial_restaurants$delta, 0.8)
  
  # Select sample firms and alternative locations for these firms
  available_firms <- initial_restaurants %>% 
    filter(area_m2 <= area_threshold) %>%
    filter(markup <= markup_threshold) %>%
    filter(delta <= delta_threshold) %>%
    pull(rest_id)
  available_locations <- initial_restaurants %>% pull(location_id)
  
  # Save number of alterations by type:
  n_deletes <- 0
  
  n_outer <- 0
  while (TRUE) { 
    # Delete and reorder firm while time limit not reached
    
    n_outer <- n_outer + 1
    
    change <- 0
    n_inner <- 0
    
    # Delete firm randomly, record new metrics
    if ((current_cw >= d_max_exp_u) & (current_profits >= d_profits)) {
      firm_delete_id <- sample(available_firms, 1)
      reordered_restaurants <- reordered_restaurants[rest_id != firm_delete_id, ]
      available_firms <- available_firms[available_firms != firm_delete_id]
      current_cw <- compute_maximum_expected_utility(reordered_restaurants, rho1) 
      current_profits <- compute_profits(reordered_restaurants, rho1) 
      n_deletes <- n_deletes + 1
      print(paste('Deleted:', n_deletes))
    }

    # Improve metrics by shifting and switching
    while (!change) {
      # Reorder until change in both metrics
      reordered_restaurants_inner <- copy(reordered_restaurants)
      
      n_inner <- n_inner + 1
      
      if ((n_outer + n_inner) %% 2 == 0) {
        list[rest_mod, metrics] <- shift_firm(reordered_restaurants_inner, available_firms, available_locations)
      } else {
        list[rest_mod, metrics] <- switch_firms(reordered_restaurants_inner, available_firms)
      }
      
      # How did the welfare metrics change?
      m1 <- metrics[1] - current_cw
      m2 <- metrics[2] - current_profits
      
      change <- (m1 > 0) & (m2 > 0) 
      
      # Break if time limit reached
      now.time <- Sys.time()
      if (as.numeric(difftime(now.time, start.time, units = 'secs')) > max_time) {
        stop <- TRUE
        break
      }
      
    }
    
    # Update allocation and welfare metrics
    reordered_restaurants <- rest_mod
    current_cw <- metrics[1]
    current_profits <- metrics[2]

    # Break if time limit reached
    if (stop) {break}
    
  }
  value <- list(n_deletes = n_deletes, 
                reordered_restaurants = reordered_restaurants)
  return(value)
}

###############################################################################


################################## Main code ##################################

# Take the input parameters
args <- commandArgs(trailingOnly = TRUE)
cbsa <- as.character(args[1])

list[pairs, initial_restaurants, locations, restaurant_features, location_features] <- PrepareData(cbsa)

# Compute the status-quo outcomes
d_max_exp_u <- compute_maximum_expected_utility(initial_restaurants, rho1) 
d_profits <- compute_profits(initial_restaurants, rho1) 

# Delete firms, reallocate to save welfare
stats <- delete_improve_allocation(max_time)

# Determine, which restaurants were deleted
remaining_restaurants <- stats$reordered_restaurants %>% pull(rest_id)
deleted_restaurants <- restaurant_features %>%
  filter(!(rest_id %in% remaining_restaurants))

deleted_restaurants_by_price <- deleted_restaurants %>%
  group_by(price) %>% 
  summarize(n = n())

deleted_restaurants_by_price$cbsa <- cbsa

# Save summary of the results
if (file.exists(output_file_path)) {
  write_csv(deleted_restaurants_by_price, output_file_path, append = TRUE)
} else {
  write_csv(deleted_restaurants_by_price, output_file_path, append = FALSE)
}

###############################################################################
