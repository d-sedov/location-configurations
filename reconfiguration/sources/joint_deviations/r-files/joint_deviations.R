###############################################################################
###############################################################################
#
# BY: Dmitry Sedov 
#
# DESC: This code explores joint firm location changes. 
#       Comparing profit and welfare changes to the individual deviations
#       informs of the miscoordination issues scale.
#
# COMMENT:
#
###############################################################################
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

days <- 31
# Output file path
output_file_path <- file.path('/home/quser/project_dir/urban/data/output', 
                              'counterfactuals',
                              'sources',
                              'joint_deviations_summary.csv')

# Input paths
input_folder <- '/home/quser/project_dir/urban/data/output/spatial-demand/main_demand'
markup_folder <- '/home/quser/project_dir/urban/data/output/entry/markups'
population_folder <- '/home/quser/project_dir/urban/data/output/descriptive'
rent_folder_path <- '/home/quser/project_dir/urban/data/output/rent'

# Rho-minimization file path
rho_file_path <- file.path(input_folder, 'minimization_summary_optimized.csv')

###############################################################################


################################ Functions ####################################

# Function to remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 2 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}


# This function computes the summary statistics for a variable
simple_summary <- function(data, var) {
  require(dplyr)
  require(tibble)
  data %>% 
    select(!!sym(var)) %>% 
    summarise_all(list(q10 = ~quantile(., 0.1, na.rm = T),
                       q25 = ~quantile(., 0.25, na.rm = T), 
                       median = ~median(as.numeric(.), na.rm = T), 
                       q75 = ~quantile(., 0.75, na.rm = T), 
                       q90 = ~quantile(., 0.9, na.rm = T),
                       mean = ~mean(., na.rm = T), 
                       sd = ~sd(., na.rm = T)
    )
    ) %>%
    add_column(v = var, .before = 1)
}

PrepareData <- function(cbsa) {
  # Function to import and prepare single-CBSA dataset for estimation
  #
  # IN:
  #     cbsa - string with the cbsa number
  # OUT:
  #     pairs, restaurants, locations data tables
  #     rho1 set in the global environment
  
  # Read in the distance cost parameter for the current CBSA
  rho_data <- read_csv(rho_file_path, col_names = FALSE)
  rho1 <<- rho_data$X2[rho_data$X1 == cbsa]
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
  initial_restaurants <- as.data.table(initial_restaurants)
  
  
  # Prepare datasets for output
  value <- list(pairs = pairs, 
                initial_restaurants = initial_restaurants, 
                locations = locations, 
                restaurant_features = restaurant_features,
                location_features = location_features)
  return(value)
}


compute_profits <- function(reordered_restaurants, rho1, return_type, firm_ids) {
  # IN:
  #    1. reordered_restaurants - data frame of firm restaurants
  #    2. firm_ids - moved firm ids
  #    3. rho1 - distance cost parameter
  # OUT: 
  #    1. firm_profits - firm's profits
  #    2. total_profits - total profits for firms with markup estimates
  
  # Compute the predicted number of visits
  rho2 <- 0
  # Match pairs and deltas
  # NOTICE all.y = FALSE
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
  
  # Compute firm profits and total profits
  visits <- visits %>% 
    mutate(profit = visits * markup - 10.7 * area_m2 * rent) 
  
  total_profits <- visits %>% 
    summarise(total_profits = sum(profit, na.rm = TRUE)) %>%
    pull(total_profits)
  
  if (return_type == 'all') {
    value <- list(all_profits = visits %>% select(rest_id, profit), 
                  total_profits = total_profits)
  } else {
    firm_profits <- visits %>% 
      filter(rest_id %in% firm_ids) %>%
      pull(profit)
    firm_profits <- sum(firm_profits)
    value <- list(firm_profits = firm_profits, 
                  total_profits = total_profits)
  }
  
  return(value)
  
}

compute_maximum_expected_utility <- function(reordered_restaurants, rho1) {
  # IN:
  #    1. reordered_restaurants - data frame of firm characteristics
  #    2. rho1 - distance cost parameter
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

joint_deviation <- function(firm_ids, new_location_ids) {
  # This function evaluates firm profits, welfare and total industry profits
  # as firm_ids switch locations.
  
  # Switch the firm's location:
  reordered_restaurants <- copy(initial_restaurants)
  
  reordered_restaurants[rest_id == firm_ids[1], 
                          location_id := new_location_ids[2]]
  reordered_restaurants[rest_id == firm_ids[2], 
                        location_id := new_location_ids[1]]
  
  # Compute metrics: welfare, firm profits, total industry profits:
  welfare <- compute_maximum_expected_utility(reordered_restaurants, rho1)
  list[firm_profits, total_profits] <- compute_profits(reordered_restaurants,
                                                       rho1,
                                                       'firm',
                                                       firm_ids
  ) 
  value <- list(welfare = welfare, 
                firm_profits = firm_profits, 
                total_profits = total_profits)
  
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

###############################################################################


############################### Main code #####################################

# Take the input parameters
args <- commandArgs(trailingOnly = TRUE)
cbsa <- as.character(args[1])
firm_sample_size <- as.integer(args[2])

# Prepare the data
list[pairs, initial_restaurants, locations, restaurant_features, location_features] <- PrepareData(cbsa)

# Compute the status-quo welfare and total profits
default_max_exp_u <- compute_maximum_expected_utility(initial_restaurants, rho1) 
list[default_profits, default_total_profits] <- compute_profits(initial_restaurants, rho1, 'all', c(0, 0)) 
default_rho <- rho1
default_profits <- default_profits %>% rename(default_profit = profit)

# Area threshold
area_threshold <- quantile(initial_restaurants$area_m2, 0.8)
# Markup threshold
markup_threshold <- quantile(initial_restaurants$markup, 0.8)
# Delta threshold
delta_threshold <- quantile(initial_restaurants$delta, 0.8)

# Sample firms to switch locations
sample_firms1 <- initial_restaurants %>% 
  filter(area_m2 <= area_threshold) %>%
  filter(markup <= markup_threshold) %>%
  filter(delta <= delta_threshold) 

firm_sample_size <- min(firm_sample_size, nrow(sample_firms1))
sample_firms1 <- sample_firms1 %>%
  sample_n(firm_sample_size)

sample_firms2 <- initial_restaurants %>% 
  filter(area_m2 <= area_threshold) %>%
  filter(markup <= markup_threshold) %>%
  filter(delta <= delta_threshold) 

firm_sample_size <- min(firm_sample_size, nrow(sample_firms2))
sample_firms2 <- sample_firms2 %>%
  sample_n(firm_sample_size)

firm_pairs <- mapply(c, sample_firms1$rest_id, sample_firms2$rest_id, SIMPLIFY = F)
location_pairs <- mapply(c, sample_firms1$location_id, sample_firms2$location_id, SIMPLIFY = F)

# Iterate through firm pairs, record location switched consequences
joint_deviations <- lapply(1 : firm_sample_size, 
                     function(i) {
                       print(i)
                       list[w, p, t] <- joint_deviation(firm_pairs[[i]], location_pairs[[i]])
                       data.frame(f1 = firm_pairs[[i]][1],
                                  f2 = firm_pairs[[i]][2],
                                  welfare = w,
                                  profits = p,
                                  total_profits = t)
                     })
joint_deviations <- bind_rows(joint_deviations)

# Get profits
joint_deviations <- joint_deviations %>% 
  rename(rest_id = f1) %>%
  left_join(default_profits) %>%
  rename(f1_profit = default_profit) %>% 
  rename(f1 = rest_id) %>% 
  rename(rest_id = f2) %>%
  left_join(default_profits) %>%
  rename(f2_profit = default_profit) 

# Record consequences of joint deviations
joint_deviations <- joint_deviations %>% 
  mutate(default_welfare = default_max_exp_u,
         default_total_profits = default_total_profits) %>%
  mutate(profitable_deviation = (profits - f1_profit - f2_profit > 0), 
         profit_change = profits - f1_profit - f2_profit,
         welfare_change = welfare - default_welfare, 
         total_profits_change = total_profits - default_total_profits) 

# Select profitable deviations
joint_profitable_deviations <- joint_deviations %>% 
  filter(profitable_deviation)

# Select profitable welfare-increasing deviations
joint_profitable_welfare_increasing_deviations <- joint_deviations %>%
  filter(profitable_deviation & welfare_change > 0)

# Summarize results by joint deviation class

# Select variables for summary statistics construction
vars <- list('profitable_deviation',
             'welfare',
             'profit_change',
             'total_profits_change'
)

joint_deviations_summary <- bind_rows(lapply(vars,
                                             function(x) {simple_summary(joint_deviations, x)}
                                             )
                                      )

joint_profitable_deviations_summary <- bind_rows(lapply(vars,
                                                        function(x) {simple_summary(joint_profitable_deviations, x)}
                                                        )
                                                 )

joint_profitable_welfare_increasing_deviations_summary <- bind_rows(lapply(vars, 
                      function(x) {simple_summary(joint_profitable_welfare_increasing_deviations, x)}
                      )
                      )

# Add cbsa and type columns
joint_deviations_summary$cbsa <- cbsa
joint_deviations_summary$type <- 'all'

joint_profitable_deviations_summary$cbsa <- cbsa
joint_profitable_deviations_summary$type <- 'profitable'

joint_profitable_welfare_increasing_deviations_summary$cbsa <- cbsa
joint_profitable_welfare_increasing_deviations_summary$type <- 'profitable_welfare_increasing'

# Find the rho-equivalent of the welfare changes
x <- seq(-0.5, -0.01, 0.01)
y <- sapply(x, 
            function(x){compute_maximum_expected_utility(initial_restaurants, x)})
distance_cost_variation <- data_frame(rho = x, max_exp_u_default = y)

# Translate welfare metrics into rho-related
welfare_metrics <- joint_deviations_summary[joint_deviations_summary$v == 'welfare',
                                      c('q10', 'q25', 'median', 'q75', 'q90', 'mean')]
welfare_metrics <- lapply(welfare_metrics, function(x){(find_rho(x) - rho1) /abs(rho1)})
joint_deviations_summary[joint_deviations_summary$v == 'welfare', 
                   c('q10', 'q25', 'median', 'q75', 'q90', 'mean')] <- welfare_metrics

welfare_metrics <- joint_profitable_deviations_summary[joint_profitable_deviations_summary$v == 'welfare',
                                            c('q10', 'q25', 'median', 'q75', 'q90', 'mean')]
welfare_metrics <- lapply(welfare_metrics, function(x){(find_rho(x) - rho1) /abs(rho1)})
joint_profitable_deviations_summary[joint_profitable_deviations_summary$v == 'welfare', 
                         c('q10', 'q25', 'median', 'q75', 'q90', 'mean')] <- welfare_metrics

welfare_metrics <- joint_profitable_welfare_increasing_deviations_summary[joint_profitable_welfare_increasing_deviations_summary$v == 'welfare',
                                            c('q10', 'q25', 'median', 'q75', 'q90', 'mean')]
welfare_metrics <- lapply(welfare_metrics, function(x){(find_rho(x) - rho1) /abs(rho1)})
joint_profitable_welfare_increasing_deviations_summary[joint_profitable_welfare_increasing_deviations_summary$v == 'welfare', 
                         c('q10', 'q25', 'median', 'q75', 'q90', 'mean')] <- welfare_metrics


# Append deviations summaries
joint_deviations_summary <- joint_deviations_summary %>%
  bind_rows(joint_profitable_deviations_summary) %>%
  bind_rows(joint_profitable_welfare_increasing_deviations_summary)

# Save summary of the results
if (file.exists(output_file_path)) {
  write_csv(joint_deviations_summary, output_file_path, append = TRUE)
} else {
  write_csv(joint_deviations_summary, output_file_path, append = FALSE)
}

###############################################################################