###############################################################################
#
# FILE: reallocation_counterfactual.R
#
# BY: Dmitry Sedov 
#
# DATE: Tue Jun 16 2020
#
# DESC: This code contains the code to construct welfare / profits measure 
#       under counterfactual allocations of firms over the space.
#
# FUNC:
#     1. compute_maximum_expected_utility - compute the measure of consumer welfare.
#     2. compute_profits - compute firms profits under the new allocation.
#     3. find_rho - find the change in distance costs such that consumer welfare 
#        changes as much as under counterfactual allocation.
#     4. change_allocation - reorder firms across space.
#     5. explore_direction - change allocation to improve the consumer welfare, profits 
#        with weights given by direction.
#
# ALG: 
#     UNDER status-quo:
#         for rho in (-0.5, -0.01):
#             1. compute_maximum_expected_utility
#         collecte expected maximum utilities in a dataframe (max_ut_default)
#     REPEAT [until convergence / iterations thereshold is met]: 
#             1. Swap two firms
#             2. compute_maximum_expected_utility -> r_i
#             3. compute profits, Delta(profits) -> d_i
#             4. Update allocation across the space if both average utility and 
#                profits are improved
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
library(scales)

###############################################################################


################################ Constants ####################################

days <- 31
# Output file path
output_file_path <- file.path('/home/quser/project_dir/urban/data/output', 
                              'counterfactuals', 
                              'reallocation', 
                              'reallocate_output.csv')

# Variuos input paths
input_folder <- '/home/quser/project_dir/urban/data/output/spatial-demand/main_demand'
markup_folder <- '/home/quser/project_dir/urban/data/output/entry/markups'
population_folder <- '/home/quser/project_dir/urban/data/output/descriptive'
rent_folder_path <- '/home/quser/project_dir/urban/data/output/rent'

# The rho-minimization summary path
summary_file_path = file.path(input_folder, 'minimization_summary_optimized.csv')

# Theme for plots 
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
    left_join(weighted_distance) %>% 
    select(location_id, rest_id, delta, area_m2, markup, weighted_distance) 
  
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
  matched_pairs_deltas <- merge(pairs, 
                                reordered_restaurants,
                                all.x = TRUE,
                                by = 'location_id'
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
  matched_pairs_deltas <- merge(pairs, 
                                reordered_restaurants,
                                all.x = TRUE,
                                by = 'location_id'
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

change_allocation <- function(reordered_restaurants, n_move, quant_low, quant_high) {
  
  distance_high <- quantile(reordered_restaurants$weighted_distance, quant_high)
  # Select which restaurants are allowed to be shuffled
  quant_low <- quantile(reordered_restaurants$delta, probs = quant_low)
  quant_high <- quantile(reordered_restaurants$delta, probs = quant_high)
  rows_to_reshuffle <- which(between(reordered_restaurants$delta, quant_low, quant_high) & 
                               !is.na(reordered_restaurants$markup) &
                               reordered_restaurants$weighted_distance <= distance_high
                             )
  
  # Reshuffle restaurants
  rows_to_reshuffle <- sample(rows_to_reshuffle, n_move)
  new_order <- rows_to_reshuffle
  while (all(new_order == rows_to_reshuffle)) {
    new_order <- sample(rows_to_reshuffle)
  }
  reordered_restaurants$location_id[rows_to_reshuffle] <- reordered_restaurants$location_id[new_order]
  
  # Compute metrics
  v1 <- compute_maximum_expected_utility(reordered_restaurants, rho1) 
  v2 <- compute_profits(reordered_restaurants, rho1) 
  
  value <- list(reordered_restaurants_mod = reordered_restaurants, 
                metrics = c(max_exp_u = v1, total_profit = v2))
  
  return(value)
}

explore_direction <- function(x, y, n) {
  # Function to move allocation in the x - y direction in a stochastic greedy way:
  
  data_test <- data.frame(mu = 0, p = 0, col = 'highlight')
  data_test$col <- as.character(data_test$col)
  
  current_max1 <- 0
  current_max2 <- 0
  
  reordered_restaurants <- initial_restaurants
  
  for (i in 1 : n) {
    if (i %% 100 == 0) {
      cat(paste0('Step: ', i, '.\n'))  
    }
    change <- 0
    n_move <- 2 
    while (change <= 0) {
      list[rest_mod, metrics]  <- change_allocation(reordered_restaurants, 
                                                    n_move = n_move, 
                                                    quant_low = 0.1, 
                                                    quant_high = 0.9)
      now_max_exp_u <- metrics[1]
      metrics[1] <- (metrics[1] - d_max_exp_u) / d_max_exp_u
      metrics[2] <- (metrics[2] - d_profits) / abs(d_profits)
      m1 <- metrics[1] - current_max1
      m2 <- metrics[2] - current_max2
      change <- c(x, y) %*% c(m1, m2) # (m1 > 0) & (m2 > 0) #
      # print(m1)
      # print(m2)
      # cat('Change:', change, '.\n')
    }
    reordered_restaurants <- rest_mod
    current_max1 <- metrics[1]
    current_max2 <- metrics[2]
    
    data_test$col[ data_test$col == 'new' ] <- 'normal'
    
    data_test[nrow(data_test) + 1, ] <- c(metrics[1],
                                          metrics[2],
                                          'new')
    data_test$mu <- as.numeric(data_test$mu)
    data_test$p <- as.numeric(data_test$p)
    mycolours <- c("highlight" = "red", "normal" = "grey50", 'new' = 'green')
    
    #pic <- ggplot(data = data_test, aes(x = mu, y = p, color = col)) +
    #  geom_point() + 
    #  scale_colour_manual(name = "col", values = mycolours) 
    # print(metrics)
    #print(pic)
  }
  value <- list(max_exp_u = now_max_exp_u, p = current_max2, rest_final = rest_mod)
  return(value)
}

find_best_allocation <- function(max_time) {
  # Function to improve the allocation in both directions in a stochastic greedy way:
  
  start.time <- Sys.time()
  stop <- FALSE
  
  data_test <- data.frame(mu = 0, p = 0, col = 'highlight')
  data_test$col <- as.character(data_test$col)
  
  current_max1 <- 0
  current_max2 <- 0
  
  reordered_restaurants <- initial_restaurants
  n <- 1
  while (TRUE) {
    
    change <- 0
    n_move <- 2 
    while (!change) {
      list[rest_mod, metrics]  <- change_allocation(reordered_restaurants, 
                                                    n_move = n_move, 
                                                    quant_low = 0.1, 
                                                    quant_high = 0.9)
      m1 <- metrics[1] - current_max1
      m2 <- metrics[2] - current_max2
      change <- (m1 > 0) & (m2 > 0) 
      
      now.time <- Sys.time()
      if (as.numeric(difftime(now.time, start.time, units = 'secs')) > max_time) {
        stop <- TRUE
        break
      }
    }
    reordered_restaurants <- rest_mod
    current_max1 <- metrics[1]
    current_max2 <- metrics[2]
    
    data_test$col[ data_test$col == 'new' ] <- 'normal'
    
    data_test[nrow(data_test) + 1, ] <- c(metrics[1],
                                          metrics[2],
                                          'new')
    data_test$mu <- as.numeric(data_test$mu)
    data_test$p <- as.numeric(data_test$p)
    mycolours <- c("highlight" = "red", "normal" = "grey50", 'new' = 'green')
    
    #pic <- ggplot(data = data_test, aes(x = mu, y = p, color = col)) +
    #  geom_point() + 
    #  scale_colour_manual(name = "col", values = mycolours) 
    # print(metrics)
    #print(pic)
    print(as.numeric(difftime(now.time, start.time, units = 'secs')))
    if (stop) {break}
    n <- n + 1
  }
  value <- list(max_exp_u = current_max1, p = current_max2, rest_final = rest_mod, n = n)
  return(value)
}

# Function to remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 2 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

###############################################################################


################################ Test run #####################################
# 12420 45300
# cbsa <- '15380'
# 
# # Prepare the data
# list[pairs, initial_restaurants, locations] <- PrepareData(cbsa)
# 
# d_max_exp_u <- compute_maximum_expected_utility(initial_restaurants, rho1) 
# d_profits <- compute_profits(initial_restaurants, rho1) 
# d_r <- rho1
# 
# x <- seq(-0.5, -0.01, 0.01)
# y <- sapply(x, 
#             function(x){compute_maximum_expected_utility(initial_restaurants, x)})
# distance_cost_variation <- data_frame(rho = x, max_exp_u_default = y)
# 
# test_stats <- explore_direction(1, 1, 100)
# 
# find_rho(test_stats[1])

###############################################################################


# 
# 
# ######################## Create the list for Quest ############################
# 
# job_list_file <- file.path('/home/quser/project_dir/urban/', 
#                            'data/output/counterfactuals/reallocation', 
#                            'reallocation_list.csv')
# job_list <- read_csv(job_list_file, col_names = FALSE)
# cbsa_list <- c(15380,
#                17140,
#                17460,
#                18140,
#                25540,
#                26900,
#                27260,
#                28140,
#                29820,
#                31140,
#                32820,
#                33340,
#                34980,
#                35380,
#                36420,
#                39300,
#                39580,
#                40060,
#                41620,
#                41940,
#                46520,
#                47260)
# 
# job_list_all <- lapply(cbsa_list[1 : 10], function(x){job_list %>% mutate(X1 = x, X4 = 1000)})
# job_list_all <- do.call(rbind, job_list_all)
# 
# job_list_all_file <- file.path('/home/quser/project_dir/urban/', 
#                            'data/output/counterfactuals/reallocation', 
#                            'reallocation_list_all.csv')
# 
# write_csv(job_list_all, job_list_all_file, col_names = FALSE)
# ###############################################################################