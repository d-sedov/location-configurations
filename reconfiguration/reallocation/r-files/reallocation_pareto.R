###############################################################################
#
# FILE: reallocation_pareto.R
#
# BY: Dmitry Sedov 
#
# DATE: Tue Jul 7 2020
#
# DESC: This code contains the code to launch search for the 'Pareto'
#       frontier.
#
#
# COMMENT: Code to generate the input list follows:
# 
# default <- data.frame(V1 = c(0, 1, -1, 0, 0), V2 = c(0, 0, 0, 1, -1))
# n <- 64 
# circle <- t(sapply( 1 : n, function(r) c(cos(2*r*pi/n), sin(2*r*pi/n)) ))
# circle <- as.data.frame(circle)
# tasks <- rbind(pts.default, circle)
# tasks <- tasks %>% 
#   mutate(V0 = '15380', V3 = 1000) %>%
#   select(V0, everything())
# write_csv(tasks, path = file.path('/home/quser/project_dir/urban/data', 
#                                   'output/counterfactuals/reallocation',
#                                   'pareto_list.csv'),
#           col_names = FALSE)
#
###############################################################################


################################ Libraries ####################################

source('reallocation_counterfactual.R')

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
                              'reallocate_pareto.csv')

# Various input paths
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


############################### Main code #####################################

# Take the input parameters
args <- commandArgs(trailingOnly = TRUE)
cbsa <- as.character(args[1])
dir_u <- as.numeric(args[2])
dir_p <- as.numeric(args[3])
n_iterations <- as.numeric(args[4])

# Prepare the data
list[pairs, initial_restaurants, locations, restaurant_features, location_features] <- PrepareData(cbsa)

# Compute the status-quo outcomes
d_max_exp_u <- compute_maximum_expected_utility(initial_restaurants, rho1) 
d_profits <- compute_profits(initial_restaurants, rho1) 
d_r <- rho1


if ((dir_u != 0) || (dir_p != 0)) {
  x <- seq(-0.5, -0.01, 0.01)
  y <- sapply(x, 
              function(x){compute_maximum_expected_utility(initial_restaurants, x)})
  distance_cost_variation <- data_frame(rho = x, max_exp_u_default = y)
  
  stats <- explore_direction(dir_u, dir_p, n_iterations)
  rho_equivalent <- find_rho(stats[[1]])
} else {
  stats <- list()
  stats[[1]] <- d_r
  stats[[2]] <- 0
  stats[[3]] <- initial_restaurants
  rho_equivalent <- stats[[1]]
}

# Get the final mapping between restaurants and locations
rest_location_mapping_final <- stats[[3]] %>% 
  select(rest_id, location_id)

rest_location_final <- restaurant_features %>% 
  left_join(rest_location_mapping_final) %>% 
  left_join(location_features)

by_cbg <- rest_location_final %>% 
  group_by(r_cbg) %>%
  summarise(total_pop = first(total_pop), 
            restaurant_count = first(restaurant_count),
            delta = mean(delta, na.rm = T), 
            price = mean(price, na.rm = T), 
            rating = mean(rating, na.rm = T)
  )

# Compute correlations
delta_density <- with(by_cbg, cor(delta, total_pop))
delta_count <- with(by_cbg, cor(delta, restaurant_count))
delta_distance <- with(rest_location_final, cor(delta, weighted_distance))

price_density <- with(by_cbg, cor(price, total_pop, use = 'complete.obs'))
price_count <- with(by_cbg, cor(price, restaurant_count, use = 'complete.obs'))
price_distance <- with(rest_location_final, cor(price, weighted_distance, use = 'complete.obs'))

rating_density <- with(by_cbg, cor(rating, total_pop, use = 'complete.obs'))
rating_count <- with(by_cbg, cor(rating, restaurant_count, use = 'complete.obs'))
rating_distance <- with(rest_location_final, cor(rating, weighted_distance, use = 'complete.obs'))

output_data <- data.frame(cbsa = cbsa,
                          dir_u = dir_u, 
                          dir_p = dir_p, 
                          rho_equivalent = rho_equivalent,
                          change_profit = stats[[2]],
                          delta_density = delta_density, 
                          delta_count = delta_count, 
                          delta_distance = delta_distance, 
                          price_density = price_density, 
                          price_count = price_count, 
                          price_distance = price_distance, 
                          rating_density = rating_density,
                          rating_count = rating_count,
                          rating_distance = rating_distance)


#output_line <- paste(cbsa, 
#                     as.character(dir_u), 
#                     as.character(dir_p),
#                     as.character(rho_equivalent),
#                     as.character(stats[2]), 
#                     sep = ',')
# write(output_line, file = output_file_path, append = TRUE)

# Save summary of the results
if (file.exists(output_file_path)) {
  write_csv(output_data, output_file_path, append = TRUE)
} else {
  write_csv(output_data, output_file_path, append = FALSE)
}

###############################################################################