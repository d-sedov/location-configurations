################################################################################
################################################################################
#
# FILE: time_variation.R
#
# BY: Dmitry Sedov 
#
# CREATED: Tue Feb 5 2020
#
# DESC: This file creates the graphs visualizing the changes in location 
#       characteristics over time.
#
################################################################################
################################################################################


################################## Libraries ###################################

library(readr)
library(dplyr)
library(ggplot)
library(ggpubr)
library(zoo)
library(lubridate)

################################################################################

################################## Options #####################################

# Input folder
input_folder_path = '/Users/muser/dfolder/Research/urban/data/output/reduced-form'

# Output folders
plots_folder_path = '/Users/muser/dfolder/Research/urban/output/plots/reduced-form'

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

################## Visualize changes in the home panel #########################

# Import data
home <- read_csv(file.path(input_folder_path, 'home.csv'), 
                 col_types = cols(month = col_integer(),
                                  number_devices_residing = col_integer(), 
                                  year = col_integer()
                                  )
                 )

# Filter duplicate cbgs
home <- home %>% group_by(census_block_group, year, month) %>%
    arrange(state) %>% 
    top_n(1, state) %>% 
    ungroup()

# Compute the lags
home <- home %>% mutate(period = as.Date(as.yearmon(paste(year, month, sep = '-'))))
home <- home %>%
    group_by(census_block_group) %>%
    mutate(prev.number_devices_residing = lag(number_devices_residing, order_by = period))
home <- home %>% arrange(census_block_group, period)
# Compute the month-to-month change in devices
home <- home %>% mutate(devices_change = number_devices_residing - prev.number_devices_residing)

home_box_plot <- ggplot(data = home,
                        aes(y = devices_change, 
                            x = period,
                            group = period
                        )) + 
    geom_boxplot(outlier.shape = NA, 
                 fill = '#00279a',
                 color = '#00279a',
                 alpha = 0.25,
                 size = 0.15) + 
    geom_hline(yintercept = 0.0, 
               size = 0.3, 
               linetype = 'dotted') +
    coord_cartesian(ylim = c(-100, 150)) +
    theme_bw(base_family = 'Times') + 
    my_theme +
    theme(plot.margin = margin(5, 0, 0, 0, 'pt'), 
          axis.title.x = element_blank()) +
    ylab('Month-to-month change in device counts') +
    scale_x_date(date_labels = "%b %Y")

ggsave(filename = file.path(plots_folder_path, 'devices_variation.pdf'),
       device = cairo_pdf,
       plot = home_box_plot, 
       width = 3.5,
       height = 2.1)

################################################################################

################## Visualize conservative entry / exits ########################

entry_info_establishments <- read_csv(file.path(input_folder_path, 
                                                'entry_info_establishments.csv'), 
                                      col_types = cols(group = col_integer(), 
                                                       kind = col_integer()
                                                       )
                                      ) %>% filter(is_special & (group %in% c(0, 1)))

entry_info_establishments <- entry_info_establishments %>% group_by(group, kind) %>% summarise(n = n())

entry_info_establishments <- entry_info_establishments %>% mutate(period = as.Date('2017-06-01') + months(kind))

entry_exit_plot_all <- ggplot(data = entry_info_establishments, 
                              aes(x = period, y = n, group = factor(group), color = factor(group))) + 
    geom_line(size = 0.3) + 
    geom_vline(xintercept = as.Date('2017-06-01', format = '%Y-%m-%d') + months(3), size = 0.25, linetype = 'dotted') +
    geom_vline(xintercept = as.Date('2017-06-01', format = '%Y-%m-%d') + months(20), size = 0.25, linetype = 'dotted') +
    theme_bw(base_family = 'Times') + 
    scale_color_manual(values = mycolorscheme3[1:2], labels = c('Exit', 'Entry')) + 
    my_theme +
    theme(legend.key.size = unit(0.125, 'inches'),
          legend.direction = 'horizontal',
          plot.margin = margin(5, 0, 0, 0, 'pt'), 
          axis.title.x = element_blank(), 
          legend.title = element_blank()) +
    ylab('Count') + 
    scale_x_date(date_labels = "%b %Y") +
    scale_y_continuous(label=comma) + 
    coord_cartesian(xlim = c(as.Date('2017-06-01'), as.Date('2019-07-01')))

entry_exit_plot_conservative <- ggplot(data = entry_info_establishments %>% filter(kind %in% 3:20), 
                              aes(x = period, y = n, group = factor(group), color = factor(group))) + 
    geom_line(size = 0.3) + 
    geom_vline(xintercept = as.Date('2017-06-01', format = '%Y-%m-%d') + months(3), size = 0.25, linetype = 'dotted') +
    geom_vline(xintercept = as.Date('2017-06-01', format = '%Y-%m-%d') + months(20), size = 0.25, linetype = 'dotted') + 
    theme_bw(base_family = 'Times') + 
    scale_color_manual(values = mycolorscheme3[1:2], labels = c('Exit', 'Entry')) + 
    my_theme +
    theme(legend.key.size = unit(0.125, 'inches'),
          legend.direction = 'horizontal',
          plot.margin = margin(0, 0, 0, 0, 'pt'), 
          axis.title.x = element_blank(), 
          legend.title = element_blank()) +
    ylab('Count') + 
    scale_x_date(date_labels = "%b %Y") + 
    coord_cartesian(xlim = c(as.Date('2017-06-01'), as.Date('2019-07-01')))

entry_exit_variation <- ggarrange(entry_exit_plot_all, 
                                  entry_exit_plot_conservative , 
                                  ncol = 1,
                                  align = 'hv',
                                  common.legend = TRUE,
                                  legend = 'bottom')

ggsave(filename = file.path(plots_folder_path, 'entry_exit_variation.pdf'),
       device = cairo_pdf,
       plot = entry_exit_variation, 
       width = 3.5,
       height = 2.5)

################################################################################