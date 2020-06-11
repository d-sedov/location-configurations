###############################################################################
#
# FILE: fc_estimation.R
#
# BY: Dmitry Sedov 
#
# DATE: Mon Jun 8 2020
#
# DESC: This code contains the code to estimate fixed costs off the zero-profit 
#       equilibrium.
#
# IN:
#     1. Dataset with the estimated markups.
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
input_folder <- '/home/quser/project_dir/urban/data/output/entry/markups'
population_folder <- '/home/quser/project_dir/urban/data/output/descriptive'
summary_file_path <- file.path(input_folder, 'minimization_summary_optimized.csv')

# Time discounting parameter
phi <- 0.965

# Population normalization
population_norm <- 10

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


############################ Data prepartation ################################

# Import
estimated_markups_path <- file.path(input_folder, 'restaurants_estimated_markups.csv')
restaurants <- read_csv(estimated_markups_path)
# Remove outliers
restaurants <- restaurants %>%  
  mutate_at(vars(estimated_markup), 
            funs(remove_outliers))

# Compute profits:
restaurants <- restaurants %>% 
  mutate(profit = ((population_norm * raw_visit_counts * estimated_markup) - 
                     10.7 * mean_rate * area_m2
                   )
         ) %>%
  mutate(profit = profit / (1 - phi))

fit <- felm(data = restaurants, profit ~ factor(price) | r_cbsa | 0 | r_cbsa)
fit_fe <- getfe(fit)

fc_estimate <- mean(fit_fe$effect, na.rm = T)

###############################################################################
