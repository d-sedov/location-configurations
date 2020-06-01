###############################################################################
#
# FILE: estimate_deltas_rho.R
#
# BY: Dmitry Sedov 
#
# DATE: Sun Apr 26 2020
#
# DESC: This code contains the code for BLP-like estimation of spatial demand 
#       for a single CBSA.
#
# IN:
#     0. String [cbsa] with the number of cbsa.
#     1. Table [cbg_choices] with cbg-level choice counts.
#     2. Table [pairs] with cbg-restaurant pairs to compute the market shares.
#     3. Table [restaurants] with all restaurants in the cbsa.
#
###############################################################################


################################ Constants ####################################

delta_tolerance = 1e-14
max_iteration = 500
days = 31
input_folder = '/home/quser/project_dir/urban/data/output/spatial-demand/main_demand'
output_folder = '/home/quser/project_dir/urban/data/output/spatial-demand/main_demand'
summary_file_path = file.path(output_folder, 'minimization_summary_optimized.csv')

###############################################################################


################################ Libraries ####################################

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(gsubfn))

###############################################################################


################################# Functions ###################################

PrepareData <- function(cbsa) {
    # Function to import and prepare single-CBSA dataset for estimation
    #
    # IN:
    #     cbsa - string with the cbsa number
    # OUT:
    #     pairs, restaurants, choices data tables
    
    # Set the cbsa folder name
    cbsa_folder_name <- paste0('cbsa', cbsa)
    
    # Import the cbg-restaurant pairs
    pairs_file_name <- paste0('pairs', cbsa, '.csv')
    pairs_file_path <- file.path(input_folder, cbsa_folder_name, pairs_file_name)
    pairs <- read_csv(pairs_file_path, col_types = cols(cbg = col_character()))
    
    # Import the restaurants
    restaurants_file_name <- paste0('restaurants', cbsa, '.csv')
    restaurants_file_path <- file.path(input_folder, cbsa_folder_name, restaurants_file_name)
    suppressMessages(restaurants <- read_csv(restaurants_file_path))

    # Import the choices
    choices_file_name <- paste0('choices', cbsa, '.csv')
    choices_file_path <- file.path(input_folder, cbsa_folder_name, choices_file_name)
    choices_data <- read_csv(choices_file_path, col_types = cols(home_cbg = col_character()))
    
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
    
    # Get the visits (not visitors) count in the choice data
    choices_data <- choices_data %>% 
        mutate(visits_from_home_cbg = raw_visit_counts * (visitors_from_home_cbg / raw_visitor_counts))
    
    # Conversion to data tables
    pairs <- as.data.table(pairs)
    restaurants <- as.data.table(restaurants)
    choices_data <- as.data.table(choices_data)
    setkey(pairs, sname_place_id)
    setkey(restaurants, sname_place_id)
    setkey(choices_data, home_cbg, sname_place_id)
    # Precompute variables
    restaurants[, share := raw_visit_counts / cbsa_total_choices]
    # Merge to get distances
    choices_data <- merge(choices_data, 
                          pairs[,.(sname_place_id,
                                   home_cbg,
                                   distance_km,
                                   distance_km_2)],
                          by = c('sname_place_id', 
                                 'home_cbg'))
    
    
    # Conversion to data tables and set the variables in the outer scope
    value <- list(pairs = pairs, 
                  restaurants = restaurants, 
                  choices_data = choices_data)
    return(value)
}


IterateDeltas <- function(rho1, rho2, share_delta_data) {
    # Function to perform a delta-update step.
    # IN: 
    #     rho1 - linear disutility of travel
    #     rho2 - quadratic disutilty of travel
    #     share_delta_data - dataset with total choices, product shares, market ids[, deltas]
    # OUT:
    #     new_data - dataset with updated restaurant deltas 
    #
    
    # Add the delta and exp-delta columns if not present in the input dataset
    if (!('expDelta' %in% colnames(share_delta_data))) {
        share_delta_data$expDelta <- exp(-7)
    }
    
    share_delta_data <- share_delta_data[, .(sname_place_id, 
                                             expDelta, 
                                             cbsa_total_choices,
                                             share
                                             )
                                         ]
    
    # Merge the cbg-restaurant pairs with mean restaurant utilities
    new_data <- merge(pairs, 
                      share_delta_data,
                      all.x = TRUE,
                      by = 'sname_place_id'
                      )
    # Compute restaurant utility on the cbg level
    new_data[, 
             `:=`(
                 choice_utility = expDelta * exp(rho1 * distance_km +
                                                     rho2 * distance_km_2)
             )
             ]
    # Compute total utility by home_cbg
    new_data[, 
             `:=`(
                 total_utility = 1 + sum(choice_utility)),
             by = .(home_cbg)
             ]
    # Compute the total visits made from cbg to each restaurant
    new_data[, 
             choices_made := number_devices_residing * days * (choice_utility / total_utility) 
             ]
    
    new_data <- new_data[,
                         .(predicted_choices = sum(choices_made),
                           share = first(share),
                           cbsa_total_choices = first(cbsa_total_choices),
                           expDelta = first(expDelta)
                         ),
                         by = .(sname_place_id)]
    new_data[, predicted_share:= predicted_choices / cbsa_total_choices]
    
    # Update the deltas
    new_data[, expDeltaNew := expDelta * (share / predicted_share)]
    max_change = new_data[,max(abs(log(expDeltaNew) - log(expDelta)))]
    #cat('Change in delta: ', max_change, '\n')
    new_data[, expDelta := expDeltaNew]
    new_data[, expDeltaNew := NULL]
    new_data[, predicted_choices := NULL]
    value <- list(max_change = max_change, new_data = new_data)
    return(value)
}

EvaluateMoments <- function(rho1, rho2, temp_deltas) {
    # Function to perform a delta-update step.
    # IN: 
    #     rho1 - linear disutility of travel
    #     rho2 - quadratic disutilty of travel
    #     temp_deltas - datatable with restaurant deltas
    # OUT:
    #     value - value of the likelihood function
    #
    
    restaurants_deltas <- temp_deltas[,
                                      .(sname_place_id,
                                        expDelta)
                                      ][,
                                        `:=`(
                                            delta = log(expDelta), 
                                            expDelta = NULL
                                            )
                                        ]
    
    choices_data_local <- merge(choices_data, 
                                restaurants_deltas[, .(sname_place_id, delta)],
                                by = 'sname_place_id')
    
    choices_data_local[, v:= exp(rho1 * distance_km + rho2 * distance_km_2 + delta)]
    
    choices_data_local[, `:=`(denominator = sum(v), 
                              total_choices = sum(visits_from_home_cbg) 
                              ), 
                       by = .(home_cbg)] 
    
    choices_data_local[, moment := ((visits_from_home_cbg / total_choices) - (v / denominator))^2]
    
    value <- choices_data_local[, sum(moment)]
    
    cat('rho1: ', rho1, ', rho2: ', rho2, '---- Moments: ', value, '\n')        
    return(value)
}

objective <- function(rho) {
    # Function that needs to be minimized in order to estimate the parameters
    max_change <- 1
    iteration <- 1
    temp_deltas_inner <- temp_deltas
    while ((max_change > delta_tolerance) && (iteration <= max_iteration)) {
        list[max_change, temp_deltas_inner] <- IterateDeltas(rho1 = rho, rho2 = 0, share_delta_data = temp_deltas_inner)
        iteration <- iteration + 1
    }
    value <- EvaluateMoments(rho1 = rho, rho2 = 0, temp_deltas = temp_deltas_inner)
    temp_deltas <<- temp_deltas_inner
    return(value)
}

###############################################################################


############################### Main code #####################################

args <- commandArgs(trailingOnly = TRUE)
cbsa <- as.character(args)
cbsa_folder_name <- paste0('cbsa', cbsa)

cat('cbsa: ', cbsa, '\n')

# Import the file with the precomputed results
minimization_file_name <- 'minimization_summary.csv'
minimization_file_path <- file.path(input_folder, 'minimization_summary.csv')
minimization_summary <- read_csv(minimization_file_path, 
                                 col_names = FALSE, 
                                 col_types = cols(X1 = col_character()
                                 )
)

# Set the optimization parameters with help of the precomputed results
if (!(cbsa %in% minimization_summary$X1)) {
    center <- -0.1
    low <- -0.5
    high <- -0.00001
} else {
    center <- minimization_summary$X2[minimization_summary$X1 == cbsa]
    low <- center - 0.01
    high <- center + 0.01
}

if (low <= -0.3075) {
    low <- -0.5
}

if (high >= -0.005) {
    high <- -0.00001
}

# Prepare the data
list[pairs, restaurants, choices_data] <- PrepareData(cbsa)

# Do the initial iteration to set reasonable deltas
max_change <- 1
iteration <- 1
list[max_change, temp_deltas] <- IterateDeltas(center, 0, restaurants)
while ((max_change > delta_tolerance) && (iteration <= max_iteration)) {
    list[max_change, temp_deltas] <- IterateDeltas(center, 0, temp_deltas)
    iteration <- iteration + 1
    }

# OLD: simple minimization
#x <- seq(from = -0.3, to = -0.01, by = 0.005)
#y <- c()
#for (r1 in x) {
#    max_change <- 1
#    iteration <- 1
#    while ((max_change > delta_tolerance) && (iteration <= max_iteration)) {
#        list[max_change, temp_deltas] <- IterateDeltas(rho1 = r1, rho2 = 0, share_delta_data = temp_deltas)
#        iteration <- iteration + 1
#        }
#    y <- c(y, EvaluateMoments(rho1 = r1, rho2 = 0, temp_deltas = temp_deltas))
#}

# Export the results
#moments_file_name <- paste0('results', cbsa, '.csv')
#moments_file_path <- file.path(output_folder, cbsa_folder_name, moments_file_name)
#moments_table <- data.frame(rho = x, moments_value = y)
#write_csv(moments_table, moments_file_path)

# Minimize the moments over rho1
minimization_result <- optimize(objective, interval = c(low, high))

# OLD: Preliminary minimization
# z <- moments_table %>% filter(y == min(y))
# minimizer <- z$rho[1]
# summary_line <- paste(cbsa, as.character(minimizer), sep = ',')
# write(summary_line, file = summary_file_path, append = TRUE)

# Save the minimizer, convergence of deltas to the common file
minimizer <- minimization_result$minimum
# Do the iteration for the minimizer
max_change <- 1
iteration <- 1
list[max_change, temp_deltas] <- IterateDeltas(minimizer, 0, restaurants)
while ((max_change > delta_tolerance) && (iteration <= max_iteration)) {
    list[max_change, temp_deltas] <- IterateDeltas(minimizer, 0, temp_deltas)
    iteration <- iteration + 1
}
# Save summary of the results
summary_line <- paste(cbsa, as.character(minimizer), as.character(max_change), sep = ',')
write(summary_line, file = summary_file_path, append = TRUE)

# Export the deltas
deltas_file_name <- paste0('deltas_optimized', cbsa, '.csv')
deltas_file_path <- file.path(output_folder, cbsa_folder_name, deltas_file_name)
export_deltas <- temp_deltas %>% 
    mutate(delta = log(expDelta)) %>%
    select(sname_place_id, delta)

write_csv(export_deltas, deltas_file_path)

###############################################################################
