################################################################################
################################################################################
#
# FILE: restaurant_characteristics.R
#
# BY: Dmitry Sedov 
#
# CREATED: Mon Feb 17 2020
#
# DESC: This file creates the graphs depicting the relationships between 
#       restaurant characteristics and popularity. 
#
################################################################################
################################################################################

################################ Libraries #####################################

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggridges)

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
tables_folder_path = '/Users/muser/dfolder/Research/urban/output/tables/desciptive'
plots_folder_path = '/Users/muser/dfolder/Research/urban/output/plots/desciptive'

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
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

mycolorfunction1 <- colorRampPalette(colors = c('#5C75BE', '#001654'))
na_color <- '#BCBCBC'

################################################################################

############################## Prepare data ####################################

# Import data
restaurants <- read_csv(file.path(input_folder_path, 'data_restaurants.csv'))

# price_summary <- restaurants %>% 
#     mutate(price = factor(price, 
#                           levels = c(NA, 1, 2, 3, 4),
#                           exclude = '', 
#                           ordered = TRUE)
#     ) %>%
#     group_by(price) %>% 
#     summarise(avg = mean(raw_visit_counts, na.rm = T), 
#               sd = sd(raw_visit_counts, na.rm = T), 
#               n = n(),
#               se = sd / sqrt(n)
#     )

# Construct dataset for rating plots
restaurants_for_rating_plot <- restaurants %>% 
    filter(rating != 0 | is.na(rating)) %>% 
    mutate(rating = factor(rating,
                           levels = c(NA, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5),
                           exclude = '', 
                           ordered = TRUE)
           )

# Construct dataset for price plots
restaurants_for_price_plot <- restaurants %>% 
    mutate(price = factor(price, 
                          levels = c(NA, 1, 2, 3, 4),
                          exclude = '', 
                          ordered = TRUE)
    )

################################################################################


################################### Plots ######################################


# ggplot(data = price_summary) + 
#     geom_bar(aes(x = price, 
#                  y = avg),
#              stat="identity", 
#              fill= mycolorscheme3[2], 
#              alpha=0.7) + 
#     geom_errorbar(aes(x = price, 
#                       ymin = avg + qt(c(.025), n-1)*se, 
#                       ymax = avg + qt(c(.975), n-1)*se),
#                   width=0.15,
#                   colour= 'black', 
#                   alpha=1, 
#                   size=0.35) +
#     scale_x_discrete(limits = levels(price_summary$price))


# Rating-visits boxplot
rating_box_plot <- ggplot(data = restaurants_for_rating_plot,
                          aes(y = raw_visit_counts, 
                              x = as.character(rating), 
                              fill = as.character(rating), 
                              color = as.character(rating)
                              )
                          ) + 
    geom_boxplot(outlier.size = 0.25, 
                 alpha = 0.25) +
    theme_bw(base_family = 'Times') + 
    my_theme +
    theme(text = element_text(size = 12)) +
    xlab('Rating') +
    ylab('Visits') +
    scale_y_log10(label = comma) +
    scale_x_discrete(limits = levels(restaurants_for_rating_plot$rating)) + 
    scale_colour_manual(guide = FALSE,
                        na.value = na_color, 
                        values = mycolorfunction1(10)[2:10]) + 
    scale_fill_manual(guide = FALSE, 
                      na.value = na_color, 
                      values = mycolorfunction1(10)[2:10]) 
ggsave(filename = file.path(plots_folder_path, 'rating_visits_boxplot.pdf'),
       device = cairo_pdf,
       plot = rating_box_plot, 
       width = 10,
       height = 6)

# Price-visits boxplot
price_box_plot <- ggplot(data = restaurants_for_price_plot,
                         aes(y = raw_visit_counts, 
                             x = as.character(price), 
                             fill = as.character(price), 
                             color = as.character(price)
                             )
                         ) + 
    geom_boxplot(outlier.size=0.25, 
                 alpha = 0.25) +
    theme_bw(base_family = 'Times') + 
    my_theme +
    theme(text = element_text(size = 12)) +
    xlab('Price') +
    ylab('Visits') +
    scale_y_log10(label = comma) +
    scale_x_discrete(limits = levels(restaurants_for_price_plot$price)) +
    scale_colour_manual(guide = FALSE,
                        na.value = na_color, 
                        values = mycolorfunction1(10)[2:10]) + 
    scale_fill_manual(guide = FALSE, 
                      na.value = na_color, 
                      values = mycolorfunction1(10)[2:10]) 
ggsave(filename = file.path(plots_folder_path, 'price_visits_boxplot.pdf'),
       device = cairo_pdf,
       plot = price_box_plot, 
       width = 10,
       height = 6)

# Rating-visits ridgeplot
rating_ridge_plot <- ggplot(data = restaurants_for_rating_plot,
                            aes(x = raw_visit_counts, 
                                y = as.character(rating), 
                                fill = factor(stat(quantile))
                            )
) +   
    stat_density_ridges(geom = "density_ridges_gradient", 
                        calc_ecdf = TRUE,
                        quantiles = 4,
                        quantile_lines = TRUE) +
    scale_fill_viridis_d(name = "Quartiles") +
    theme_bw(base_family = 'Times') + 
    my_theme +
    theme(text = element_text(size = 12)) +
    xlab('Visits') +
    ylab('Rating') +
    scale_x_log10(label = comma) +
    scale_y_discrete(limits = levels(restaurants_for_rating_plot$rating))
ggsave(filename = file.path(plots_folder_path, 'rating_visits_ridgeplot.pdf'),
       device = cairo_pdf,
       plot = rating_ridge_plot, 
       width = 10,
       height = 6)

# Price-visits ridgeplot
price_ridge_plot <- ggplot(data = restaurants_for_price_plot,
                           aes(x = raw_visit_counts, 
                               y = as.character(price), 
                               fill = factor(stat(quantile))
                               )
                           ) +   
    stat_density_ridges(geom = "density_ridges_gradient", 
                        calc_ecdf = TRUE,
                        quantiles = 4,
                        quantile_lines = TRUE) +
    scale_fill_viridis_d(name = "Quartiles") +
    theme_bw(base_family = 'Times') + 
    my_theme +
    theme(text = element_text(size = 12)) +
    xlab('Visits') +
    ylab('Price') +
    scale_x_log10(label = comma) +
    scale_y_discrete(limits = levels(restaurants_for_price_plot$price))
ggsave(filename = file.path(plots_folder_path, 'price_visits_ridgeplot.pdf'),
       device = cairo_pdf,
       plot = price_ridge_plot, 
       width = 10,
       height = 6)

################################################################################