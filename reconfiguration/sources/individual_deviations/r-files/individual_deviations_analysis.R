###############################################################################
#
# FILE: individual_deviations_analysis.R
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

output_file_path <- file.path('/home/quser/project_dir/urban/data/output', 
                              'counterfactuals',
                              'sources',
                              'individual_deviations_summary.csv')
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


############################### Main code #####################################

# Read in the data
individual_deviations <- read_csv(output_file_path)

# Filter for all deviations summary statistics
deviations_all <- individual_deviations %>% filter(type == 'all') 

# Compare the shares of profitble and consumer-welfare-increasing deviations
# vs unprofitble and consumer-welfare-increasing deviations:
pic_comparing_deviations <-  ggplot(data = deviations_all %>% 
                                      filter(v %in% c('unprofitable_and_welfare_increasing',
                                                      'profitable_and_welfare_increasing')), 
                                    aes(x = mean, fill = v), 
                                    color = v) + 
  geom_histogram(bins = 40, 
                 alpha = 0.85, 
                 position = 'identity') + 
  theme_bw(base_family = 'Times') + 
  my_theme_large + 
  xlab('Share of deviations') + 
  ylab('Market count') + 
  scale_fill_manual(breaks = c('unprofitable_and_welfare_increasing',
                               'profitable_and_welfare_increasing'), 
                    labels = c('Unprofitable & CW-increasing', 
                               'Profitable & CW-increasing'), 
                    values = mycolorscheme3[2:1]) +
  theme(legend.position = c(0.01, 0.99),
        legend.justification = c("left", "top"),
        legend.direction="vertical",
        legend.title = element_blank(), 
        legend.box.just = "left",
        legend.margin = margin(0,0,0,0), 
        legend.box.margin = margin(0,0,0,0),
        legend.spacing.y = unit(0, 'cm'),
        legend.key.width = unit(0.3, "cm"),
        legend.key.height = unit(0.3, "cm")) +
  scale_x_continuous(labels = scales::percent) +
  theme(plot.margin = unit(c(0,0.15,0,0), "in"))

ggsave(filename = file.path(plots_folder_path, 
                            'comparing_deviations.pdf'), 
       device = cairo_pdf, plot = pic_comparing_deviations, width = 4.0, height = 2.5)
embed_fonts(file = path.expand(file.path(plots_folder_path,
                                         'comparing_deviations.pdf')))

profit_change_comparison <- individual_deviations %>% 
  filter(type == 'profitable') %>%
  filter(v %in% c('profit_change', 'total_profits_change')) %>% 
  select(v, median, cbsa) %>% 
  pivot_wider(id_cols = cbsa, names_from = v, values_from = median)

pic_comparing_profits <- ggplot(profit_change_comparison %>% 
         filter(total_profits_change < 100000 & total_profits_change > -10000) %>% filter(profit_change < 10000),
       aes(x = profit_change, y = total_profits_change)) + 
  geom_point(aes(shape = 21), 
             fill = "lightgray", 
             color = 'black', size = 2) + 
  geom_abline(aes(intercept = 0, slope = 1, color = 'Identity line'), linetype = 'dashed') + 
  theme_bw(base_family = 'Times') + 
  my_theme_large + 
  scale_shape_identity() +
  scale_x_continuous(labels = scales::comma) +
  scale_y_continuous(labels = scales::comma) +
  xlab('Median profit change of shifted firm') + 
  ylab('Median industry profit change') + 
  theme(legend.title = element_blank()) +
  scale_colour_manual(values = "black") + 
  theme(legend.position = c(0.95, 0.01),
        legend.justification = c("right", "bottom")) +
  theme(plot.margin = unit(c(0,0.15,0,0), "in"))


ggsave(filename = file.path(plots_folder_path, 
                            'comparing_profits.pdf'), 
       device = cairo_pdf, plot = pic_comparing_profits, width = 4.0, height = 2.5)
embed_fonts(file = path.expand(file.path(plots_folder_path,
                                         'comparing_profits.pdf')))


# Select variables for summary statistics construction
vars <- list('share_increasing',
             'profitable_deviation',
             'unprofitable_and_welfare_increasing',
             'profitable_and_welfare_increasing',
             'welfare',
             'profit_change',
             'total_profits_change'
)

# Create across-market summary statistics on deviations
deviation_means <- individual_deviations %>% 
  filter(v %in% c('share_increasing', 
                  'profitable_deviation',
                  'unprofitable_and_welfare_increasing', 
                  'profitable_and_welfare_increasing')) %>% 
  pivot_wider(id_cols = c('cbsa', 'type'), names_from = v, values_from = mean)

deviation_medians <- individual_deviations %>% 
  filter(v %in% c('welfare', 
                  'profit_change', 
                  'total_profits_change')) %>% 
  pivot_wider(id_cols = c('cbsa', 'type'), names_from = v, values_from = median) %>%
  mutate(welfare = 1e5 * welfare)

deviations_summary <- deviation_means %>% left_join(deviation_medians)

# Summarize all deviations
deviations_summary_all <- bind_rows(lapply(vars,
                                           function(x) {
                                             simple_summary(deviations_summary %>% filter(type == 'all'), x)
                                             }
                                           )
                                    )
deviations_summary_all$v <- c('Market share $\\uparrow$', 
                              'Profit $\\uparrow$',
                              'Profit $\\downarrow$, CW $\\uparrow$', 
                              'Profit $\\uparrow$, CW $\\uparrow$',
                              '\\% CW change $\\times 10^5$', 
                              'Firm profit change', 
                              'Industry profit change')
colnames(deviations_summary_all) <- str_to_title(colnames(deviations_summary_all))

# Summarize profitable deviations
deviations_summary_profitable <- bind_rows(lapply(vars,
                                                  function(x) {
                                                    simple_summary(deviations_summary %>% 
                                                                     filter(type == 'profitable'), x)
                                                  }
)
)
deviations_summary_profitable$v <- c('Market share $\\uparrow$', 
                              'Profit $\\uparrow$',
                              'Profit $\\downarrow$, CW $\\uparrow$', 
                              'Profit $\\uparrow$, CW $\\uparrow$',
                              '\\% CW change $\\times 10^5$', 
                              'Firm profit change', 
                              'Industry profit change')
colnames(deviations_summary_profitable) <- str_to_title(colnames(deviations_summary_profitable))

# Summarize profitable welfare-increasing
deviations_summary_profitable_welfare <- bind_rows(lapply(vars,
                                           function(x) {
                                             simple_summary(deviations_summary %>%
                                                              filter(type == 'profitable_welfare_increasing'), x)
                                           }
)
)
deviations_summary_profitable_welfare$v <- c('Market share $\\uparrow$', 
                                     'Profit $\\uparrow$',
                                     'Profit $\\downarrow$, CW $\\uparrow$', 
                                     'Profit $\\uparrow$, CW $\\uparrow$',
                                     '\\% CW change $\\times 10^5$', 
                                     'Firm profit change', 
                                     'Industry profit change')
colnames(deviations_summary_profitable_welfare) <- str_to_title(colnames(deviations_summary_profitable_welfare))

deviations_summary_joint <- bind_rows(deviations_summary_all, 
                                      deviations_summary_profitable,
                                      deviations_summary_profitable_welfare)
colnames(deviations_summary_joint)[1] <- ''
colnames(deviations_summary_joint)[8] <- 'SD'

kable(deviations_summary_joint, 
      format = 'latex',
      label = 'individual_deviations_summary',
      escape = F, 
      digits = 3,
      booktabs = T, 
      caption = paste('Across-market summary statistics on the consequences of median individual deviations.', 
                       'Consumer welfare (CW) is measured by percent reduction in alternative-equivalent', 
                       'distance costs versus the status-quo distance costs.')) %>%
  kable_styling(latex_options = 'scale_down') %>% 
  pack_rows("All deviations", 1, 7, bold = T, italic = T) %>%
  pack_rows("Profitable deviations", 8, 14, latex_gap_space = "1em", bold = T, italic = T) %>%
  pack_rows("Profitable \\\\& CW-increasing deviations", 15, 21, latex_gap_space = "1em", bold = T, italic = T, escape = F) %>% 
  row_spec(0, bold = T, italic = T) %>%
  cat(., file = file.path(tables_folder_path, 'across_market_deviations_summary.tex'))

###############################################################################