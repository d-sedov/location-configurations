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
library(ggpubr)

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
tables_folder_path = '/Users/muser/dfolder/Research/urban/output/tables/preliminary/proximity'
plots_folder_path = '/Users/muser/dfolder/Research/urban/output/plots/preliminary/proximity'

# Visits data vintage
year = '2018'
month = 'October'

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 6),
                  legend.title = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.text = element_text(size = 6),
                  axis.title = element_text(size = 8)
)

mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

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
    geom_smooth(size = 0.5) + 
    stat_summary_bin(fun.y = 'mean', bins = 30, size = 0.25, geom = 'point') +
    scale_color_manual(name = 'Restaurant competition', values = mycolorscheme3) + 
    scale_fill_manual(name = 'Restaurant competition', values = mycolorscheme3) +
    theme_bw(base_family = 'Times') +
    my_theme + 
    theme(legend.key.size = unit(0.125, 'inches'),         
          plot.margin = margin(5, 0, 5, 2, 'pt')) +
    xlab('Establishment number') +
    ylab('Average visits to restaurants')

ggsave(filename = file.path(plots_folder_path, 'proximity_establishments.pdf'),
       device = cairo_pdf,
       plot = p1, 
       width = 3.5,
       height = 2.1)
embed_fonts(file.path(plots_folder_path, 'proximity_establishments.pdf'))

# Plot number devices residing vs average restaurant visits
p2 <- ggplot(data = cbgs, 
             aes(x = number_devices_residing, 
                 y = rest_average_visits, 
                 color = rest_comp, 
                 fill = rest_comp)
             ) + 
    geom_smooth(size = 0.5) + 
    stat_summary_bin(fun.y = 'mean', bins = 30, size = 0.25, geom = 'point') +
    scale_color_manual(name = 'Restaurant competition', values = mycolorscheme3) + 
    scale_fill_manual(name = 'Restaurant competition', values = mycolorscheme3) +
    theme_bw(base_family = 'Times') +
    my_theme + 
    theme(legend.key.size = unit(0.125, 'inches'),
          plot.margin = margin(5, 2, 5, 0, 'pt')) + 
    xlab('Devices number') +
    ylab('Average visits to restaurants')

ggsave(filename = file.path(plots_folder_path, 'proximity_devices.pdf'),
       device = cairo_pdf,
       plot = p2, 
       width = 3.5,
       height = 2.1)
embed_fonts(file.path(plots_folder_path, 'proximity_devices.pdf'))

p3 <- ggarrange(p1, 
                p2 + theme(axis.title.y = element_blank()), 
                ncol = 2, nrow = 1, 
                common.legend = TRUE, legend = 'bottom',
                align = 'v')

ggsave(filename = file.path(plots_folder_path, 'proximity_establishments_devices.pdf'),
       device = cairo_pdf,
       plot = p3,
       width = 4.85,
       height = 2.5)
embed_fonts(file.path(plots_folder_path, 'proximity_establishments_devices.pdf'))

################################################################################