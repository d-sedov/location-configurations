###############################################################################
#
# FILE: delete_worst_counterfactual.R
#
# BY: Dmitry Sedov 
#
# DATE: Fri Jul 10 2020
#
# DESC: This code contains the code to construct welfare measures in case of 
#       deleting the worst firms in the market. 
#
#
###############################################################################


################################ Libraries ####################################

source('number_counterfactual.R')

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
# Output file path
output_file_path <- file.path('/home/quser/project_dir/urban/data/output', 
                              'counterfactuals', 
                              'number', 
                              'delete_worst_summary.csv')

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

# Prepare the data
list[pairs, initial_restaurants, locations, restaurant_features, location_features] <- PrepareData(cbsa)

# Determine the number of restaurants to be removed
total_restaurants <- nrow(initial_restaurants)
count_quantiles <- quantile(1 : total_restaurants, 
                            seq(0.05, 0.25, 0.05),
                            type = 1)

# Precompute average utilities for finding rho-equivalent later
x <- seq(-0.5, -0.01, 0.01)
y <- sapply(x, 
            function(x){compute_maximum_expected_utility(initial_restaurants, x)})
distance_cost_variation <- data_frame(rho = x, max_exp_u_default = y)

# Delete worst restaurants continuously
removed <- c()
for (j in 0 : max(count_quantiles)) {
  # Compute baseline stats before any restaurants are removed:
  if (j == 0) {

    perc_removed <- 0
    rhos <- rho1
    
    # Compute average quality
    average_quality <- mean(restaurant_features$delta, 
                            na.rm = TRUE)
    # Compute average number of restaurants across CBGs
    average_count <- location_features %>% 
      group_by(r_cbg) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      summarise(m = mean(restaurant_count, na.rm = TRUE)) %>%
      pull(m)
    # Compute average density across CBGs
    average_density <- location_features %>% 
      group_by(r_cbg) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      summarise(m = mean(total_pop, na.rm = TRUE)) %>%
      pull(m)
  } else {
    updated <- delete(removed, 10, 0.1, 0.9)
    removed <- updated$deleted
    if (j %in% count_quantiles) {
      perc_removed_j <- as.numeric(gsub('%',
                                        '',
                                        names(count_quantiles[count_quantiles == j])
                                        )
                                   )
      perc_removed <- c(perc_removed, perc_removed_j)
      
      # What is the loss in utility?
      removed_rho_equiv <- find_rho(updated$max_exp_u)
      rhos <- c(rhos, removed_rho_equiv)
      
      # What is the quality of the removed restaurants?
      removed_quality <- restaurant_features %>%
        filter(rest_id %in% removed) %>% 
        summarise(m = mean(delta, na.rm = TRUE)) %>%
        pull(m)
      average_quality <- c(average_quality, removed_quality)
      
      # From which CBGs were the restaurants removed?
      locations_with_removed <- initial_restaurants %>%
        filter(rest_id %in% removed) %>% 
        pull(location_id)
      
      # CBGs with a lot of restaurants?
      removed_count <- location_features %>% 
        filter(location_id %in% locations_with_removed) %>%
        group_by(r_cbg) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        summarise(m = mean(restaurant_count, na.rm = TRUE)) %>%
        pull(m)
      average_count <- c(average_count, removed_count)
      
      # CBGs with high population density?
      removed_density <- location_features %>% 
        filter(location_id %in% locations_with_removed) %>%
        group_by(r_cbg) %>%
        filter(row_number() == 1) %>%
        ungroup() %>%
        summarise(m = mean(total_pop, na.rm = TRUE)) %>%
        pull(m)
      average_density <- c(average_density, removed_density)
    }
  }
}

output_data <- data_frame(perc_removed = perc_removed, 
                          rhos = rhos, 
                          average_quality = average_quality,
                          average_count = average_count,
                          average_density = average_density)

output_data <- output_data %>% 
  mutate(cbsa = cbsa) %>%
  select(cbsa, everything())

# Save summary of the results
if (file.exists(output_file_path)) {
  write_csv(output_data, output_file_path, append = TRUE)
} else {
  write_csv(output_data, output_file_path, append = FALSE)
}

###############################################################################
