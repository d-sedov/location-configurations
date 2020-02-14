################################################################################
################################################################################
#
# FILE: proximity.R
#
# BY: Dmitry Sedov 
#
# CREATED: Fri Feb 14 2020
#
# DESC: This file creates the graphs on preliminary evidence of 
#       proximity-effects.
#
################################################################################
################################################################################


################################ Libraries #####################################

library(readr)
library(dplyr)
library(xtable)
library(kableExtra)
library(jsonlite)
library(tidyr)
library(ggplot2)
library(scales)
library(gtools)

################################################################################

################################# Options ######################################

# Set the tables properties
# Footnote font size
options(xtable.size = 'footnotesize')
# Page top placement
options(xtable.table.placement = 't')

# Input folder
input_folder_path = '/Users/muser/dfolder/Research/urban/data/output/descriptive'

# Output folders
tables_folder_path = '/Users/muser/dfolder/Research/urban/output/tables/descriptive'
plots_folder_path = '/Users/muser/dfolder/Research/urban/output/plots/descriptive'

# Visits data vintage
year = '2018'
month = 'October'

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 14),
                  legend.title = element_text(size= 16),
                  plot.title = element_text(hjust = 0.5, size = 18)
)
mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#ffe200', '#722ab5')

################################################################################

################################# Functions ####################################

# Function to remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}

################################################################################

############################## Prepare data ####################################

# Import data
cbgs <- read_csv(file.path(input_folder_path, 'data_cbg.csv'))

# Replace missing values with 0 (no restaurants / establishments in cbgs)
cbgs <- cbgs %>% 
    mutate(urban = !is.na(cbsa),
           rest_number = replace(rest_number, 
                                 which(is.na(rest_number)),
                                 0),
           est_number = replace(est_number,
                                which(is.na(est_number)),
                                0),
           rest_area = replace(rest_area, 
                               which(is.na(rest_area)),
                               0),
           est_area = replace(est_area,
                              which(is.na(est_area)),
                              0),
           area_km2 = area_m2 / 1000000
    )

cbgs <- cbgs %>% 
    mutate(rest_average_visits = rest_visits / rest_number) %>%
    filter(rest_number > 0) %>%
    mutate(rest_comp = quantcut(rest_number, 3))

levels(cbgs$rest_comp) <- c('Low', 'Medium', 'High')

# Remove outliers
cbgs <- cbgs %>% 
    group_by(rest_comp) %>% 
    mutate_at(vars(est_number, 
                   rest_average_visits, 
                   number_devices_residing), 
              funs(remove_outliers))

# CT alternative
# cts <- cbgs %>% 
#     group_by(ct) %>% 
#     summarise(number_devices_residing = sum(number_devices_residing, na.rm = T), 
#               rest_number = sum(rest_number, na.rm = T),
#               est_number = sum(est_number, na.rm = T),
#               rest_visits = sum(rest_visits, na.rm = T)
#               )
# cts <- cts %>% 
#     mutate(rest_average_visits = rest_visits / rest_number) %>%
#     filter(rest_number > 0) %>%
#     mutate(rest_comp = quantcut(rest_number, 3))
# 
# # Remove outliers
# cts <- cts %>% 
#     group_by(rest_comp) %>% 
#     mutate_at(vars(est_number, 
#                    rest_average_visits, 
#                    number_devices_residing), 
#               funs(remove_outliers))


################################################################################

################################# Plots ########################################

# Plot establishments vs average restaurant visits
p1 <- ggplot(data = cbgs, 
             aes(x = est_number, 
                 y = rest_average_visits, 
                 color = rest_comp, 
                 fill = rest_comp)
            ) + 
    geom_smooth() + 
    stat_summary_bin(fun.y = 'mean', bins = 30, size = 1, geom = 'point') +
    scale_color_manual(name = 'Restaurant competition', values = mycolorscheme3) + 
    scale_fill_manual(name = 'Restaurant competition', values = mycolorscheme3) +
    theme_bw(base_family = 'Times') +
    my_theme + 
    theme(text = element_text(size = 12)
          ) +
    xlab('Establishment number') +
    ylab('Average visits to restaurants')

ggsave(filename = file.path(plots_folder_path, 'proximity_establishments.pdf'),
       device = cairo_pdf,
       plot = p1, 
       width = 10,
       height = 6)

# Plot number devices residing vs average restaurant visits
p2 <- ggplot(data = cbgs, 
             aes(x = number_devices_residing, 
                 y = rest_average_visits, 
                 color = rest_comp, 
                 fill = rest_comp)
             ) + 
    geom_smooth() + 
    stat_summary_bin(fun.y = 'mean', bins = 30, size = 1, geom = 'point') +
    scale_color_manual(name = 'Restaurant competition', values = mycolorscheme3) + 
    scale_fill_manual(name = 'Restaurant competition', values = mycolorscheme3) +
    theme_bw(base_family = 'Times') +
    my_theme + 
    theme(text = element_text(size = 12)
    ) +
    xlab('Devices number') +
    ylab('Average visits to restaurants')

ggsave(filename = file.path(plots_folder_path, 'proximity_devices.pdf'),
       device = cairo_pdf,
       plot = p2, 
       width = 10,
       height = 6)

################################################################################