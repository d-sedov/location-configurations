################################################################################
################################################################################
#
# FILE: census_comparison_summary.R
#
# BY: Dmitry Sedov 
#
# CREATED: Wed May 13 2020
#
# DESC: This file creates two graphs with the of the relationship between CBG 
#       population according to Census and to Sname.
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
library(extrafont)
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
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#ffe200', '#722ab5')

################################################################################


############################## Prepare data ####################################

# Import data
cbgs <- read_csv(file.path(input_folder_path, 'data_cbg.csv'))
census_population <- read_csv(file.path(input_folder_path, 'cbg_population.csv'))

# Join by CBG ID
census_population <- cbgs %>% 
    select(cbg, number_devices_residing) %>% 
    right_join(census_population, by = c('cbg' = 'home_cbg'))

cbg_population_graph <- ggplot(data = census_population, aes(x = total_pop)) +
    geom_histogram(color = mycolorscheme3[1], fill = mycolorscheme3[1]) +
    xlim(0, 10000) + 
    theme_bw(base_family = 'Times') + 
    my_theme +
    xlab('CBG population (Census)') +
    ylab('CBG count') + 
    theme(plot.margin = unit(c(0,0.8,0,0.2), "lines"))

census_sname_graph <- ggplot(data = census_population, 
                                 aes(x = total_pop, 
                                     y = number_devices_residing)) + 
    geom_smooth(method = 'lm', 
                size = 0.3,
                color = mycolorscheme3[2], 
                fill = mycolorscheme3[2]) + 
    stat_summary_bin(fun.y = 'mean', 
                     bins = 30, 
                     size = 0.15, 
                     geom = 'point') +
    xlim(0, 10000) +
    theme_bw(base_family = 'Times') + 
    my_theme +
    xlab('CBG population (Census)') +
    ylab('# devices in Sname home panel') + 
    theme(plot.margin = unit(c(0,0,0,0), "lines"))

fit_simple <- lm(data = census_population, number_devices_residing ~ total_pop)
slope <- format(round(fit_simple$coefficients[2], 3), nsmall = 3)

census_sname_graph <- census_sname_graph + 
    annotate('text', 
             8125, 635, 
             label = paste0("Slope: ", slope), 
             family = 'Times', size = 6 * 0.35 / 1)

plot_both <- ggarrange(cbg_population_graph, 
                census_sname_graph, 
                ncol = 2, nrow = 1, 
                common.legend = TRUE,
                align = 'v')

ggsave(filename = file.path(plots_folder_path, 'census_sname.pdf'),
       device = cairo_pdf,
       plot = plot_both,
       width = 5.1,
       height = 2.2)
embed_fonts(file.path(plots_folder_path, 'census_sname.pdf'))

################################################################################