###############################################################################
#
# FILE: number_counterfactual.R
#
# BY: Dmitry Sedov 
#
# DATE: Tue Jun 16 2020
#
# DESC: This code contains the code to construct welfare measures in case of 
#       varying the number of firms in the market.
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

days <- 31

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
  # locations <- locations %>% 
  #  mutate(rent = ifelse(is.na(rent), median(rent, na.rm=TRUE), rent))
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
    select(location_id, rest_id, delta, area_m2, markup) 
  
  # Replace missing markups with median
  #initial_restaurants <- initial_restaurants %>% 
  #  mutate(markup = ifelse(is.na(markup), median(markup, na.rm=TRUE), markup))
  initial_restaurants <- as.data.table(initial_restaurants)
  
  
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
  # NOTICE all.x = FALSE
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
  
  # Average across home location to get the return value
  value <- matched_pairs_deltas[, (sum(max_exp_u * total_pop) / sum(total_pop))]
  return(value)
  
}

delete <- function(delete_for_sure, n_try, quant_low, quant_high) {
  
  # Select which restaurants are allowed to be deleted
  quant_low <- quantile(initial_restaurants$delta, probs = quant_low)
  quant_high <- quantile(initial_restaurants$delta, probs = quant_high)
  
  rows_to_delete <- initial_restaurants %>% 
    filter(between(delta, quant_low, quant_high)) %>%
    filter(!(rest_id %in% delete_for_sure)) %>% 
    left_join(location_features) %>%
    group_by(r_cbg) %>%
    sample_n(1) %>% 
    ungroup() %>% 
    pull(rest_id)
  
  modified_restaurants <- initial_restaurants %>%
    filter(!(rest_id %in% delete_for_sure))
  
  rows_to_delete <- sample(rows_to_delete, n_try)
  
  current_max <- 0
  rest_found <- 0
  vs <- c()
  
  # Loop through restaurants deleting the least-welfare damaging one
  for (r in rows_to_delete) {
    modified_restaurants_inner <- modified_restaurants %>% 
      filter(rest_id != r) 
    modified_restaurants_inner <- as.data.table(modified_restaurants_inner)
    # Compute metrics
    v1 <- compute_maximum_expected_utility(modified_restaurants_inner,
                                           rho1) 
    if (v1 > current_max) {
      current_max <- v1
      rest_found <- r
    }
    vs <- c(v1, vs)
    # plot(x = rep(1, length(vs)), y = vs, type = 'p')
  }
  
  deleted <- c(rest_found, delete_for_sure)
  value <- list(max_exp_u = current_max, n = length(deleted), deleted = deleted)
  
  return(value)
}


add <- function(already_added, rest_id_next, n_try, quant_low, quant_high) {
  
  # Select thre restaurants that are allowed to be added
  rows_to_add <- initial_restaurants %>% 
    filter(between(delta, quant_low, quant_high)) 
    
  # Sample delta
  delta_new <- sample(rows_to_add$delta, 1)
  
  # Select the to-be added restaurants
  rows_to_add <- sample_n(rows_to_add, n_try)
  
  modified_restaurants <- initial_restaurants %>% bind_rows(already_added)
  
  current_max <- 0
  # Iterate through to-be-added restaurants and compute the updated expected utility
  for (r in 1 : n_try) {
    new_rest <- rows_to_add %>% filter(row_number() == r)
    rest_id_now <- new_rest$rest_id
    new_rest <- new_rest %>% mutate(rest_id = rest_id_next)

    # Add the new restaurants
    modified_restaurants_inner <- modified_restaurants %>% bind_rows(new_rest)
    # Compute metrics
    v1 <- compute_maximum_expected_utility(modified_restaurants_inner,
                                           rho1)
    if (v1 > current_max) {
      current_max <- v1
      rest_found <- new_rest
    }

  }
  
  value <- list(max_exp_u = current_max, 
                already_added = bind_rows(already_added, rest_found)
                )
  
  return(value)
}


add_average <- function(already_added, rest_id_next, n_try, average_delta) {
  
  sample_by_cbg <- location_features %>% 
    group_by(r_cbg) %>% 
    sample_n(1) %>% 
    ungroup() %>% 
    pull(location_id)
  
  # Select the to-be added restaurants options
  rows_to_add <- initial_restaurants %>% 
    filter(location_id %in% sample_by_cbg) %>%
    sample_n(n_try)
  
  # Append the already added restaurants
  modified_restaurants <- initial_restaurants %>% bind_rows(already_added)
  
  current_max <- 0
  # Iterate through to-be-added restaurants and compute the updated expected utility
  for (r in 1 : n_try) {
    new_rest <- rows_to_add %>% filter(row_number() == r)
    rest_id_now <- new_rest$rest_id
    new_rest <- new_rest %>% mutate(rest_id = rest_id_next, 
                                    delta = average_delta)
    
    # Add the new restaurants
    modified_restaurants_inner <- modified_restaurants %>% bind_rows(new_rest)
    # Compute metrics
    v1 <- compute_maximum_expected_utility(modified_restaurants_inner,
                                           rho1)
    if (v1 > current_max) {
      current_max <- v1
      rest_found <- new_rest
    }
    
  }
  
  value <- list(max_exp_u = current_max, 
                already_added = bind_rows(already_added, rest_found)
  )
  
  return(value)
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



# removed <- c()
# rhos <- c()
# 
# for (j in 1 : 250) {
#   updated <- delete(removed, 10, 0.1, 0.9)
#   removed <- updated$deleted
#   removed_rho_equiv <- find_rho(updated$max_exp_u)
#   rhos <- c(rhos, removed_rho_equiv)
#   pic <- qplot(seq(1, j), rhos, geom = 'line')
#   print(pic)
# }
# 
# added <- initial_restaurants %>% sample_n(0)
# rhos_add <- c()
# rest_id_next <- max(initial_restaurants$rest_id)
#   
# for (j in 1 : 250) {
#   rest_id_next <- rest_id_next + 1
#   updated <- add(added, rest_id_next, 10, 0.1, 0.9)
#   added <- updated$already_added
#   added_rho_equiv <- find_rho(updated$max_exp_u)
#   rhos_add <- c(rhos_add, added_rho_equiv)
#   pic <- qplot(seq(1, j), rhos_add, geom = 'line')
#   print(pic)
# }

###############################################################################
