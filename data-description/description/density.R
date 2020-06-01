################################################################################
################################################################################
#
# FILE: density.R
#
# BY: Dmitry Sedov 
#
# CREATED: Wed May 13 2020
#
# DESC: This file creates the graphs on how population density is related to 
#       restaurant visits and rating. 
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
library(grid)

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
year = '2019'
month = 'July'

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
census_population <- read_csv(file.path(input_folder_path, 
                                        'cbg_population.csv')) %>% 
    rename(cbg = home_cbg)
restaurants <- read_csv(file.path(input_folder_path, 'data_restaurants.csv'))

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
    ) %>% 
    left_join(census_population)

cbgs <- cbgs %>% 
    mutate(rest_average_visits = rest_visits / rest_number) %>%
    filter(rest_number > 0)

# Remove outliers
cbgs <- cbgs %>%  
    mutate_at(vars(rest_average_visits, 
                   total_pop), 
              funs(remove_outliers))

restaurants_by_cbg <- restaurants %>% filter(!is.na(cbsa)) %>%
    group_by(cbg) %>% 
    summarise(rating = mean(rating,
                            na.rm = T))

# Join data to summarise on the cbg-level
cbgs <- left_join(cbgs, restaurants_by_cbg) %>% filter(rest_number > 0)
################################################################################


################################ Plots #########################################

# Set plot limits 
x_limits_d <- quantile(cbgs$total_pop, probs = c(0.1, 0.9), na.rm = TRUE)

# Plot average rating vs density
p0 <- ggplot(data = cbgs,
             aes(x = total_pop,
                 y = rest_number)) +
    geom_smooth(size = 0.3, color = mycolorscheme3[2], fill = mycolorscheme3[2]) +
    stat_summary_bin(fun.y = 'mean', 
                     bins = 30, 
                     size = 0.15, 
                     geom = 'point') +
    theme_bw(base_family = 'Times') +
    my_theme +
    xlab('CBG population (Census)') +
    ylab('Rest. count') +
    xlim(x_limits_d) +
    theme(plot.margin = margin(0, 0, 0, 2, 'pt'))

# Plot population vs average restaurant visits
p1 <- ggplot(data = cbgs, 
             aes(x = total_pop, 
                 y = rest_average_visits)
             ) + 
    geom_smooth(size = 0.5, color = mycolorscheme3[2], fill = mycolorscheme3[2]) + 
    stat_summary_bin(fun.y = 'mean', bins = 30, size = 0.25, geom = 'point') +
    theme_bw(base_family = 'Times') +
    my_theme + 
    xlab('CBG population (Census)') +
    ylab('Ave. visits') +
    xlim(x_limits_d) +
    theme(plot.margin = margin(0, 0, 0, 2, 'pt'))

# Plot average rating vs density
p2 <- ggplot(data = cbgs,
             aes(x = total_pop,
                 y = rating)) +
    geom_smooth(size = 0.3, color = mycolorscheme3[2], fill = mycolorscheme3[2]) +
    stat_summary_bin(fun.y = 'mean', 
                     bins = 30, 
                     size = 0.15, 
                     geom = 'point') +
    theme_bw(base_family = 'Times') +
    my_theme +
    xlab('CBG population (Census)') +
    ylab('Ave. rating') +
    xlim(x_limits_d) +
    theme(plot.margin = margin(0, 0, 0, 2, 'pt'))

pj <- ggarrange(p0 + theme(axis.title.x = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.text.x = element_blank()),
                p1 + theme(axis.title.x = element_blank(), 
                           axis.ticks.x = element_blank(), 
                           axis.text.x = element_blank()),
                p2 + theme(axis.title.x = element_blank()), 
                ncol = 1, nrow = 3, 
                common.legend = TRUE,
                align = 'hv')

axis_x <- textGrob('CBG population (Census)', gp = gpar(fontsize = 8, fontfamily = 'Times'))
pj_final <- pj + theme(plot.margin = unit(c(0,0,0.75,0), 'lines')) +
    annotation_custom(grob = axis_x, xmin = 0.55, xmax = 0.6, ymin = -0.02, ymax = -0.02)

ggsave(filename = file.path(plots_folder_path, 'density.pdf'),
       device = cairo_pdf,
       plot = pj_final,
       width = 2.25,
       height = 3)
embed_fonts(file.path(plots_folder_path, 'density.pdf'))


################################################################################