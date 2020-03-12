################################################################################
################################################################################
#
# FILE: sorting.R
#
# BY: Dmitry Sedov 
#
# CREATED: Fri Feb 14 2020
#
# DESC: This file creates the graphs on the preliminary evidence of sorting 
#       by rating, price type and categories. 
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
tables_folder_path = '/Users/muser/dfolder/Research/urban/output/tables/preliminary/sorting'
plots_folder_path = '/Users/muser/dfolder/Research/urban/output/plots/preliminary/sorting'

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
restaurants <- read_csv(file.path(input_folder_path, 'data_restaurants.csv'))
# Import cbg data
cbgs <- read_csv(file.path(input_folder_path, 'data_cbg.csv'))

# Set price to missing if it is -1
restaurants <- restaurants %>% mutate(price = ifelse(price == -1, NA, price))

# Replace missing (null) categories with NAs
restaurants <- restaurants %>% mutate(categories = ifelse(categories == 'null', 
                                                          NA, 
                                                          categories))

# Transform the total minutes open variable
restaurants <- restaurants %>%
    mutate(perc_time_open = 100 * total_minutes_open / (24 * 60 * 31)) %>%
    mutate(perc_time_open = replace(perc_time_open,  
                                    which(perc_time_open <= 0.1 | perc_time_open >= 100.1), 
                                    NA))

# Encode 'urban' restaurants
restaurants <- restaurants %>%
    mutate(urban = !is.na(cbsa)) 

# Count restaurant's categories
restaurants <- restaurants %>%
    mutate(n_categories = sapply(categories, 
                                 function(x) {
                                     ifelse(is.na(x), 
                                            NA,
                                            ifelse(x == "[]",
                                                   0,
                                                   nrow(fromJSON(x))
                                            )
                                     )
                                 }
    )
    )

# Summarise restaurants by cbg
restaurants$dummy <- 1
restaurants_by_cbg <- restaurants %>% 
    mutate(row = row_number()) %>% 
    pivot_wider(names_from = price, 
                names_prefix = 'price',
                values_from = dummy, 
                values_fill = list(dummy = 0)
                ) %>% 
    group_by(cbg) %>% 
    summarise(n = n(),
              price1 = sum(price1),
              price2 = sum(price2),
              price3 = sum(price3),
              price4 = sum(price4),
              priceNA = sum(priceNA),
              n_categories = mean(n_categories, 
                                  na.rm = T),
              rating = mean(rating,
                            na.rm = T))

# Replace missing values with 0 (no restaurants / establishments in cbgs)
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
           area_km2 = area_m2 / 1000000,
           number_devices_residing = replace(number_devices_residing,
                                             which(is.na(number_devices_residing)),
                                             0)
    )

# Select urban cbgs
# cbgs <- cbgs %>% filter(urban)

# Join data to summarise on the cbg-level
cbgs <- left_join(cbgs, restaurants_by_cbg) %>% filter(rest_number > 0)

# Remove outliers
# cbgs <- cbgs %>% 
#     mutate_at(vars(est_number, 
#                    number_devices_residing), 
#               funs(remove_outliers))

cbgs <- cbgs %>% mutate_at(.vars = vars(starts_with('price')), `/`, quote(n))
cbgs_price_categories <- cbgs %>% 
    select(cbg, est_number, number_devices_residing, starts_with('price')) %>% 
    pivot_longer(cols = starts_with('price'), 
                 names_to = 'price', 
                 values_to = 'share') 

# Datasets for areacharts
# cbgs_price_categories_establishments <- cbgs_price_categories %>%
#     group_by(est_number, price) %>% 
#     summarize(share = mean(share))
# 
# cbgs_price_categories_devices <- cbgs_price_categories %>%
#     group_by(number_devices_residing, price) %>% 
#     summarize(share = mean(share))
################################################################################

################################ Plots #########################################

# Set plot limits 
x_limits_e <- quantile(cbgs$est_number, probs = c(0.1, 0.9))
x_limits_d <- quantile(cbgs$number_devices_residing, probs = c(0.1, 0.9))

# Establishmets-price categories distribution plot
p1e <- ggplot(data = cbgs_price_categories, 
              aes(x = est_number,
                  y = share, 
                  color = price)) + 
    geom_smooth(size = 0.3, se = FALSE) +
    scale_color_manual(name = 'Price category', 
                       values = mycolorscheme3,
                       labels = c('price1' = '$',
                                  'price2' = '$$',
                                  'price3' = '$$$',
                                  'price4' = '$$$$',
                                  'priceNA' = 'NA'
                                  )) +
    theme_bw(base_family = 'Times') +
    my_theme + 
    theme(legend.key.size = unit(0.1, 'inches'),
          plot.margin = margin(5, 0, 5, 2, 'pt')) +
    xlab('Establishments') +
    ylab('Share of restaurants') +
    xlim(x_limits_e) + 
    coord_cartesian(xlim = x_limits_e, 
                    ylim = c(0, 0.5))
ggsave(filename = file.path(plots_folder_path, 'establishments_prices.pdf'),
       device = cairo_pdf,
       plot = p1e, 
       width = 3.5,
       height = 2.1)
embed_fonts(file.path(plots_folder_path, 'establishments_prices.pdf'))

# Devices-price categories distribution plot
p1d <- ggplot(data = cbgs_price_categories, 
              aes(x = number_devices_residing,
                  y = share,
                  color = price)) + 
    geom_smooth(size = 0.3, se = FALSE) +
    scale_color_manual(name = 'Price category', 
                       values = mycolorscheme3,
                       labels = c('price1' = '$',
                                  'price2' = '$$',
                                  'price3' = '$$$',
                                  'price4' = '$$$$',
                                  'priceNA' = 'NA'
                       )) +
    theme_bw(base_family = 'Times') +
    my_theme +
    theme(legend.key.size = unit(0.125, 'inches'),
          plot.margin = margin(5, 0, 5, 2, 'pt'))+
    xlab('Devices') +
    ylab('Share of restaurants') +
    xlim(x_limits_d) + 
    coord_cartesian(xlim = x_limits_d, 
                    ylim = c(0, 0.5))
ggsave(filename = file.path(plots_folder_path, 'devices_prices.pdf'),
       device = cairo_pdf,
       plot = p1d, 
       width = 3.5,
       height = 2.1)
embed_fonts(file.path(plots_folder_path, 'devices_prices.pdf'))

p1j <- ggarrange(p1e, 
                 p1d + theme(axis.title.y = element_blank()),
                 ncol = 2, nrow = 1, 
                 common.legend = TRUE, legend = 'bottom',
                 align = 'h')

ggsave(filename = file.path(plots_folder_path, 'establishments_devices_prices.pdf'),
       device = cairo_pdf,
       plot = p1j, 
       width = 4.85,
       height = 2.5)
embed_fonts(file.path(plots_folder_path, 'establishments_devices_prices.pdf'))

# Establishments-rating plot
p2e <- ggplot(data = cbgs,
              aes(x = est_number,
                  y = rating)) +
    geom_smooth(size = 0.3, color = mycolorscheme3[2], fill = mycolorscheme3[2]) +
    stat_summary_bin(fun.y = 'mean', 
                     bins = 30, 
                     size = 0.15, 
                     geom = 'point') +
    theme_bw(base_family = 'Times') +
    my_theme +
    theme(legend.key.size = unit(0.125, 'inches'),
          plot.margin = margin(5, 0, 5, 2, 'pt')) +
    xlab('Establishments') +
    ylab('Average rating') +
    xlim(x_limits_e) + 
    coord_cartesian(xlim = x_limits_e, ylim = c(3.25, 3.7))
ggsave(filename = file.path(plots_folder_path, 'establishments_rating.pdf'),
       device = cairo_pdf,
       plot = p2e, 
       width = 3.5,
       height = 2.1)
embed_fonts(file.path(plots_folder_path, 'establishments_rating.pdf'))

# Devices-rating plot
p2d <- ggplot(data = cbgs,
              aes(x = number_devices_residing,
                  y = rating)) +
    geom_smooth(size = 0.3, color = mycolorscheme3[2], fill = mycolorscheme3[2]) +
    stat_summary_bin(fun.y = 'mean', 
                     bins = 30, 
                     size = 0.15, 
                     geom = 'point') +
    theme_bw(base_family = 'Times') +
    my_theme +
    theme(legend.key.size = unit(0.125, 'inches'),
          plot.margin = margin(5, 0, 5, 2, 'pt')) +
    xlab('Devices') +
    ylab('Average rating') +
    xlim(x_limits_d) + 
    coord_cartesian(xlim = x_limits_d, ylim = c(3.25, 3.7))
ggsave(filename = file.path(plots_folder_path, 'devices_rating.pdf'),
       device = cairo_pdf,
       plot = p2d, 
       width = 3.5,
       height = 2.1)
embed_fonts(file.path(plots_folder_path, 'devices_rating.pdf'))

p2j <- ggarrange(p2e, 
                 p2d + theme(axis.title.y = element_blank()),
                 ncol = 2, nrow = 1, 
                 common.legend = TRUE, legend = 'bottom',
                 align = 'h')

ggsave(filename = file.path(plots_folder_path, 'establishments_devices_rating.pdf'),
       device = cairo_pdf,
       plot = p2j, 
       width = 4.85,
       height = 2.5)
embed_fonts(file.path(plots_folder_path, 'establishments_devices_rating.pdf'))

y_limits_categories <- c(1.85, 2.05)
# Establishments - categories plot
p3e <- ggplot(data = cbgs,
              aes(x = est_number,
                  y = n_categories)) +
    geom_smooth(size = 0.3, color = mycolorscheme3[2], fill = mycolorscheme3[2]) +
    stat_summary_bin(fun.y = 'mean', 
                     bins = 30, 
                     size = 0.15, 
                     geom = 'point') +
    theme_bw(base_family = 'Times') +
    my_theme +
    theme(legend.key.size = unit(0.125, 'inches'),
          plot.margin = margin(5, 0, 5, 2, 'pt')) +
    xlab('Establishments') +
    ylab('Average # categories') +
    xlim(x_limits_e) + 
    coord_cartesian(xlim = x_limits_e, ylim = y_limits_categories)
ggsave(filename = file.path(plots_folder_path, 'establishments_categories.pdf'),
       device = cairo_pdf,
       plot = p3e, 
       width = 3.5,
       height = 2.1)
embed_fonts(file.path(plots_folder_path, 'establishments_categories.pdf'))

# Devices - categories plot
p3d <- ggplot(data = cbgs,
              aes(x = number_devices_residing,
                  y = n_categories)) +
    geom_smooth(size = 0.3, color = mycolorscheme3[2], fill = mycolorscheme3[2]) +
    stat_summary_bin(fun.y = 'mean', 
                     bins = 30, 
                     size = 0.15, 
                     geom = 'point') +
    theme_bw(base_family = 'Times') +
    my_theme +
    theme(legend.key.size = unit(0.125, 'inches'),
          plot.margin = margin(5, 0, 5, 2, 'pt')) +
    xlab('Devices') +
    ylab('Average # categories') +
    xlim(x_limits_d) + 
    coord_cartesian(xlim = x_limits_d, ylim = y_limits_categories)
ggsave(filename = file.path(plots_folder_path, 'devices_categories.pdf'),
       device = cairo_pdf,
       plot = p3d, 
       width = 3.5,
       height = 2.1)
embed_fonts(file.path(plots_folder_path, 'devices_categories.pdf'))


p3j <- ggarrange(p3e, 
                 p3d + theme(axis.title.y = element_blank()),
                 ncol = 2, nrow = 1, 
                 common.legend = TRUE, legend = 'bottom',
                 align = 'h')

ggsave(filename = file.path(plots_folder_path, 'establishments_devices_categories.pdf'),
       device = cairo_pdf,
       plot = p3j, 
       width = 4.85,
       height = 2.5)
embed_fonts(file.path(plots_folder_path, 'establishments_devices_categories.pdf'))

p_all <- ggarrange(p2e + theme(axis.title.x = element_blank(), 
                               axis.text.x = element_blank(),
                               plot.margin = unit(c(0,0,-2,0), 'lines')) + 
                       ylab('Ave. rating'),
                   p2d + theme(axis.title.x = element_blank(), 
                               axis.text.x = element_blank(), 
                               axis.title.y = element_blank(),
                               axis.text.y = element_blank(),
                               plot.margin = unit(c(-2,0,-2,0), 'lines')) +
                       ylab('Ave. rating'),
                   p3e + theme(axis.title.x = element_blank(), 
                               axis.text.x = element_blank(),
                               plot.margin = unit(c(-2,0,-2,0), 'lines')) +
                       ylab('Ave. categ. #'),
                   p3d + theme(axis.title.x = element_blank(), 
                               axis.text.x = element_blank(),
                               axis.title.y = element_blank(),
                               axis.text.y = element_blank(),
                               plot.margin = unit(c(-2,0,-2,0), 'lines')) + 
                       ylab('Ave. categ. #'),
                   p1e + theme(legend.position = 'none',
                               axis.title.x = element_blank(),
                               plot.margin = unit(c(-2,0,0,0), 'lines')) +
                       ylab('Rest. share'), 
                   p1d + theme(axis.title.y = element_blank(),
                               axis.text.y = element_blank(),
                               axis.title.x = element_blank(),
                               legend.position = c(-0.105, 0.5),
                               legend.text = element_text(size = 4),
                               legend.title = element_blank(),
                               legend.direction = 'vertical',
                               legend.background=element_blank(),
                               legend.key.size = unit(0.05, 'inches'),
                               plot.margin = unit(c(-2,0,0,0), 'lines')) +
                       ylab('Rest. share'),
                   ncol = 2, nrow = 3, 
                   common.legend = FALSE,
                   align = 'hv')

axis1 <- textGrob('Establishments', gp = gpar(fontsize = 8, fontfamily = 'Times'))
axis2 <- textGrob('Devices', gp = gpar(fontsize = 8, fontfamily = 'Times'))
p_all_final <- p_all + theme(plot.margin = unit(c(0,0,0.75,0), 'lines')) +
    annotation_custom(grob = axis1, xmin = 0.285, xmax = 0.285, ymin = -0.02, ymax = -0.02) + 
    annotation_custom(grob = axis2, xmin = 0.785, xmax = 0.785, ymin = -0.02, ymax = -0.02)

ggsave(filename = file.path(plots_folder_path, 'sorting_all_final.pdf'),
       device = cairo_pdf,
       plot = p_all_final, 
       width = 4.25,
       height = 3)
embed_fonts(file.path(plots_folder_path, 'sorting_all_final.pdf'))

################################################################################