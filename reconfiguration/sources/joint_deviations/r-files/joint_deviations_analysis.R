###############################################################################
#
# FILE: joint_deviations_analysis.R
#
# BY: Dmitry Sedov 
#
# DATE: Thu Sep 10 2020
#
# DESC: This code contains the code to analyze the individual deviations.
#
# COMMENT: 
#
###############################################################################


################################ Libraries ####################################

library(dplyr)
library(readr)
library(extrafont)
library(extrafontdb)
library(scales)
library(ggplot2)
library(lfe)
library(stargazer)
library(sandwich)
library(ggpubr)

###############################################################################


################################# Functions ###################################

# This function computes the summary statistics for a variable
simple_summary <- function(data, var) {
  require(dplyr)
  require(tibble)
  data %>% 
    select(!!sym(var)) %>% 
    summarise_all(list(q10 = ~quantile(., 0.1, na.rm = T),
                       q25 = ~quantile(., 0.25, na.rm = T), 
                       median = ~median(as.numeric(.), na.rm = T), 
                       q75 = ~quantile(., 0.75, na.rm = T), 
                       q90 = ~quantile(., 0.9, na.rm = T),
                       mean = ~mean(., na.rm = T), 
                       sd = ~sd(., na.rm = T)
    )
    ) %>%
    add_column(v = var, .before = 1)
}

###############################################################################


################################# Options ######################################

options(scipen = 999)

output_file_path <- file.path('/home/quser/project_dir/urban/data/output', 
                              'counterfactuals',
                              'sources',
                              'joint_deviations_summary.csv')
plots_folder_path <- file.path('/home/quser/project_dir/urban/output', 
                               'plots/model')
tables_folder_path <- file.path('/home/quser/project_dir/urban/output', 
                                'tables/model')
input_folder_path <- '/home/quser/project_dir/urban/data/output/descriptive'

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 6),
                  legend.title = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.text = element_text(size = 6),
                  axis.title = element_text(size = 8)
)
my_theme_large <- theme(legend.text = element_text(size = 10),
                        legend.title = element_text(size = 12),
                        plot.title = element_text(hjust = 0.5, size = 20),
                        axis.text = element_text(size = 10),
                        axis.title = element_text(size = 12)
)

mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

###############################################################################


##################### Individual deviation metrics ############################

# output_file_path <- file.path('/home/quser/project_dir/urban/data/output', 
#                               'counterfactuals',
#                               'sources',
#                               'individual_deviations_summary.csv')
# 
# # Read in the data
# individual_deviations <- read_csv(output_file_path)
# 
# # Select variables for summary statistics construction
# vars <- list('share_increasing',
#              'profitable_deviation',
#              'unprofitable_and_welfare_increasing',
#              'profitable_and_welfare_increasing',
#              'welfare',
#              'profit_change',
#              'total_profits_change'
# )
# 
# # Create across-market summary statistics on deviations
# deviation_means <- individual_deviations %>% 
#   filter(v %in% c('share_increasing', 
#                   'profitable_deviation',
#                   'unprofitable_and_welfare_increasing', 
#                   'profitable_and_welfare_increasing')) %>% 
#   pivot_wider(id_cols = c('cbsa', 'type'), names_from = v, values_from = mean)
# 
# deviation_medians <- individual_deviations %>% 
#   filter(v %in% c('welfare', 
#                   'profit_change', 
#                   'total_profits_change')) %>% 
#   pivot_wider(id_cols = c('cbsa', 'type'), names_from = v, values_from = median) %>%
#   mutate(welfare = 1e5 * welfare)
# 
# deviations_summary <- deviation_means %>% left_join(deviation_medians)
# 
# # Summarize all deviations
# deviations_summary_all <- bind_rows(lapply(vars,
#                                            function(x) {
#                                              simple_summary(deviations_summary %>% filter(type == 'all'), x)
#                                            }
# )
# )
# deviations_summary_all$v <- c('Market share $\\uparrow$', 
#                               'Profit $\\uparrow$',
#                               'Profit $\\downarrow$, CW $\\uparrow$', 
#                               'Profit $\\uparrow$, CW $\\uparrow$',
#                               '\\% CW change $\\times 10^5$', 
#                               'Firm profit change', 
#                               'Industry profit change')
# colnames(deviations_summary_all) <- str_to_title(colnames(deviations_summary_all))
# 
# # Summarize profitable deviations
# deviations_summary_profitable <- bind_rows(lapply(vars,
#                                                   function(x) {
#                                                     simple_summary(deviations_summary %>% 
#                                                                      filter(type == 'profitable'), x)
#                                                   }
# )
# )
# deviations_summary_profitable$v <- c('Market share $\\uparrow$', 
#                                      'Profit $\\uparrow$',
#                                      'Profit $\\downarrow$, CW $\\uparrow$', 
#                                      'Profit $\\uparrow$, CW $\\uparrow$',
#                                      '\\% CW change $\\times 10^5$', 
#                                      'Firm profit change', 
#                                      'Industry profit change')
# colnames(deviations_summary_profitable) <- str_to_title(colnames(deviations_summary_profitable))
# 
# # Summarize profitable welfare-increasing
# deviations_summary_profitable_welfare <- bind_rows(lapply(vars,
#                                                           function(x) {
#                                                             simple_summary(deviations_summary %>%
#                                                                              filter(type == 'profitable_welfare_increasing'), x)
#                                                           }
# )
# )
# deviations_summary_profitable_welfare$v <- c('Market share $\\uparrow$', 
#                                              'Profit $\\uparrow$',
#                                              'Profit $\\downarrow$, CW $\\uparrow$', 
#                                              'Profit $\\uparrow$, CW $\\uparrow$',
#                                              '\\% CW change $\\times 10^5$', 
#                                              'Firm profit change', 
#                                              'Industry profit change')
# colnames(deviations_summary_profitable_welfare) <- str_to_title(colnames(deviations_summary_profitable_welfare))

###############################################################################


############################### Main code #####################################

# Read in the data
joint_deviations <- read_csv(output_file_path)

# Select variables for summary statistics construction
vars <- list('welfare',
             'profit_change',
             'total_profits_change'
)

# Create across-market summary statistics on joint deviations
joint_deviation_medians <- joint_deviations %>% 
  filter(v %in% c('welfare', 
                  'profit_change', 
                  'total_profits_change')) %>% 
  pivot_wider(id_cols = c('cbsa', 'type'), names_from = v, values_from = median) %>%
  mutate(welfare = 1e5 * welfare)

joint_deviations_summary <- joint_deviation_medians

# Summarize all deviations
joint_deviations_summary_all <- bind_rows(lapply(vars,
                                           function(x) {
                                             simple_summary(joint_deviations_summary %>% filter(type == 'all'), x)
                                           }
)
)
joint_deviations_summary_all$v <- c('\\% CW change $\\times 10^5$', 
                              'Firm profit change', 
                              'Industry profit change')
colnames(joint_deviations_summary_all) <- str_to_title(colnames(joint_deviations_summary_all))

# Summarize profitable deviations
joint_deviations_summary_profitable <- bind_rows(lapply(vars,
                                                  function(x) {
                                                    simple_summary(joint_deviations_summary %>% 
                                                                     filter(type == 'profitable'), x)
                                                  }
)
)
joint_deviations_summary_profitable$v <- c('\\% CW change $\\times 10^5$', 
                                     'Firm profit change', 
                                     'Industry profit change')
colnames(joint_deviations_summary_profitable) <- str_to_title(colnames(joint_deviations_summary_profitable))

# Summarize profitable welfare-increasing
joint_deviations_summary_profitable_welfare <- bind_rows(lapply(vars,
                                                          function(x) {
                                                            simple_summary(joint_deviations_summary %>%
                                                                             filter(type == 'profitable_welfare_increasing'), x)
                                                          }
)
)
joint_deviations_summary_profitable_welfare$v <- c('\\% CW change $\\times 10^5$', 
                                             'Firm profit change', 
                                             'Industry profit change')
colnames(joint_deviations_summary_profitable_welfare) <- str_to_title(colnames(joint_deviations_summary_profitable_welfare))

# Append tables
deviations_summary_all <- deviations_summary_all[5:7,]
deviations_summary_all$deviation <- 'Shifts'
joint_deviations_summary_all$deviation <- 'Switches'
deviations_summary_all <- deviations_summary_all %>% 
  bind_rows(joint_deviations_summary_all)
deviations_summary_all$type_deviation <- 'All shifts / switches'

deviations_summary_profitable_welfare <- deviations_summary_profitable_welfare[5:7,]
deviations_summary_profitable_welfare$deviation <- 'Shifts'
joint_deviations_summary_profitable_welfare$deviation <- 'Switches'
deviations_summary_profitable_welfare <- deviations_summary_profitable_welfare %>% 
  bind_rows(joint_deviations_summary_profitable_welfare)
deviations_summary_profitable_welfare$type_deviation <- 'Profitable \\& CW-increasing shifts / switches'

deviations_summary_full_table <- deviations_summary_all %>%
  bind_rows(deviations_summary_profitable_welfare)

deviations_summary_full_table <- deviations_summary_full_table %>%
  arrange(type_deviation, V, deviation)

# Rearrange columns 
deviations_summary_full_table <- deviations_summary_full_table %>%
  select(type_deviation, V, deviation, everything())

colnames(deviations_summary_full_table)[1:2] <- ''
colnames(deviations_summary_full_table)[3] <- 'Pert. type'
colnames(deviations_summary_full_table)[10] <- 'SD'

# Specify cell colors
colnames(deviations_summary_full_table)[1:2] <- c('a', 'b')
deviations_summary_full_table <- deviations_summary_full_table %>% 
  mutate_if(is.numeric, function(x) {format(round(x, 1), nsmall = 1)})
colnames(deviations_summary_full_table)[1:2] <- ''

deviations_summary_full_table[2, 3:9] <- as.list(cell_spec(deviations_summary_full_table[2, 3:9], 
                                                    'latex', color = '#009500'))
deviations_summary_full_table[4, 3:9] <- as.list(cell_spec(deviations_summary_full_table[4, 3:9], 
                                                    'latex', color = '#009500'))
deviations_summary_full_table[6, 3:9] <- as.list(cell_spec(deviations_summary_full_table[6, 3:9], 
                                                    'latex', color = '#009500'))
deviations_summary_full_table[7, 3:9] <- as.list(cell_spec(deviations_summary_full_table[7, 3:9], 
                                                    'latex', color = '#009500'))
deviations_summary_full_table[9, 3:9] <- as.list(cell_spec(deviations_summary_full_table[9, 3:9], 
                                                    'latex', color = '#009500'))
deviations_summary_full_table[11, 3:9] <- as.list(cell_spec(deviations_summary_full_table[11, 3:9], 
                                                    'latex', color = '#009500'))

kable(deviations_summary_full_table, format = 'latex', 
      caption = paste('Comparison of configuration perturbation types (shifts and switches) consequences.',
                       'Summary statistics report variation of median perturbation consequence across markets.',
                       'Consumer welfare (CW) is measured by percent reduction in alternative-equivalent', 
                       'distance costs versus the status-quo distance costs.'),
      label = 'perturbation_comparison',
      booktabs = T, align = c(rep('l', 3), rep('r', 7)), linesep = '', escape = F, digits = 1) %>%
  kable_styling(latex_options = 'scale_down') %>% 
  collapse_rows(1:2, 
                row_group_label_position = 'stack', 
                row_group_label_fonts = list(list(escape = FALSE), 
                                             list(escape = FALSE))) %>%
  row_spec(0, bold = T, italic = T) %>%
  cat(., file = file.path(tables_folder_path, 'perturbation_comparison_summary.tex'))

###############################################################################