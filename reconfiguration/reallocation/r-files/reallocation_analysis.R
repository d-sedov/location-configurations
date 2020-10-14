###############################################################################
#
# FILE: reallocation_analysis.R
#
# BY: Dmitry Sedov 
#
# DATE: Tue Sep 10 2020
#
# DESC: This code contains the code to interpret the consequences of
#       firm reconfiguration.
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

###############################################################################


################################### Options ###################################

output_file_path <- file.path('~/dfolder/Research/urban/data/output', 
                              'counterfactuals',
                              'reallocation',
                              'reallocate_shift_switch.csv')
plots_folder_path <- file.path('~/dfolder/Research/urban/output', 
                               'plots/model')
tables_folder_path <- file.path('~/dfolder/Research/urban/output', 
                                'tables/model')
input_folder_path <- '/Users/muser/dfolder/Research/urban/data/output/descriptive'

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 6),
                  legend.title = element_text(size = 8),
                  plot.title = element_text(hjust = 0.5, size = 10),
                  axis.text = element_text(size = 6),
                  axis.title = element_text(size = 8)
)
my_theme_large <- theme(legend.text = element_text(size = 8),
                        legend.title = element_text(size = 10),
                        plot.title = element_text(hjust = 0.5, size = 20),
                        axis.text = element_text(size = 8),
                        axis.title = element_text(size = 10)
)

###############################################################################


########################## Main code: graphs and tables #######################

# Import data
restaurants <- read_csv(file.path(input_folder_path, 'data_restaurants.csv'))
cbgs <- read_csv(file.path(input_folder_path, 'data_cbg.csv'))
census_population <- read_csv(file.path(input_folder_path,
                                        'cbg_population.csv')) %>%
    rename(cbg = home_cbg)
shift_switch <- read_csv(output_file_path)

# Prepare data by joining market characteristics and market outcomes
rest_by_cbsa <- restaurants %>% group_by(cbsa) %>% summarize(market_size = n())
pop_area_by_cbsa <- cbgs %>%
    left_join(census_population) %>% 
    group_by(cbsa) %>% 
    summarize(market_pop = sum(total_pop),
              market_area = sum(area_m2))
                                        
shift_switch <- shift_switch %>%
    group_by(cbsa) %>%
    mutate(change_profits = (profits - profits[type == 'initial']) / profits[type == 'initial'])

shift_switch <- shift_switch %>%
    group_by(cbsa) %>%
    mutate(change_rho = (rho_equivalent - rho_equivalent[type == 'initial']) / abs(rho_equivalent[type == 'initial']))

shift_switch_final <- shift_switch %>% filter(type == 'final')

shift_switch_final <- shift_switch_final %>% left_join(rest_by_cbsa)
shift_switch_final <- shift_switch_final %>% left_join(pop_area_by_cbsa)

shift_switch <- shift_switch %>% left_join(rest_by_cbsa)
shift_switch <- shift_switch %>% left_join(pop_area_by_cbsa)

quantile(shift_switch_final$change_profits, probs = c(0.1, 0.9))
quantile(shift_switch_final$change_rho, probs = c(0.1, 0.9))

# Plot the join distribution of profits and welfare increases
pic <- ggplot(data = shift_switch_final %>% filter(change_profits > 0),
              aes(x = change_rho, y = change_profits)) + 
    geom_smooth(data = shift_switch_final %>% filter(change_profits > 0),
                aes(x = change_rho, y = change_profits),
                method = 'lm', color = 'black', alpha = 0.15, size = 0.25) + 
    geom_point(aes(size = market_size), shape = 1) + 
    geom_vline(aes(xintercept = median(change_rho)), 
               col = 'black',
               size = 0.25,
               linetype = 'dashed') + 
    geom_hline(aes(yintercept = median(change_profits)), 
               col = 'black', 
               size = 0.25, 
               linetype = 'dashed') + 
    geom_rug(col = 'black', alpha = 0.25) + 
    scale_x_continuous(labels = scales::percent) + 
    scale_y_continuous(labels = scales::percent) +
    theme_bw(base_family = 'Times') +
    my_theme_large +
    ylab('Increase in profits') +
    xlab('Reduction in distance costs') + 
    annotate('text',
             x = median(shift_switch_final$change_rho), 
             y = max(shift_switch_final$change_profits), 
             vjust = 0.5,
             hjust = -0.065,
             label = paste0('Median: ',
                            percent(median(shift_switch_final$change_rho), 
                                    accuracy = 0.01)), 
             family = 'Times', 
             size = 3.15) + 
    annotate('text', 
             y = median(shift_switch_final$change_profits), 
             x = max(shift_switch_final$change_rho), 
             vjust = -0.5,
             hjust = 0.8,
             label = paste0('Median: ', 
                            percent(median(shift_switch_final$change_profits), 
                                    accuracy = 0.01)), 
             family = 'Times', 
             size = 3.15) + 
    guides(size = guide_legend(title = 'Market size (firm count)')) + 
    theme(legend.position = 'top', 
          legend.justification="left",
          legend.margin=margin(0.5,0.5,0.5,0), 
          legend.box.margin=margin(-5,-10,-10, 0))

ggsave(filename = file.path(plots_folder_path, 
                            'best_alternative_configurations.pdf'), 
       device = cairo_pdf, plot = pic, width = 5, height = 3.5)
embed_fonts(file = path.expand(file.path(plots_folder_path,
                             'best_alternative_configurations.pdf')))


shift_switch_final <- shift_switch_final %>% mutate(shift_share = n_shifts / iterations)

shift_switch_final <- shift_switch_final %>%
    mutate(change_profits_perc = 100 * change_profits,
           change_rho_perc = 100 * change_rho,
           shift_share_perc = 100 * shift_share,
           l_market_size = log(market_size),
           l_market_area = log(market_area),
           l_market_pop = log(market_pop))

shift_switch <- shift_switch %>%
    mutate(l_market_size = log(market_size),
           l_market_area = log(market_area),
           l_market_pop = log(market_pop))

fit_profits <- felm(data = shift_switch_final,
                    formula = change_profits_perc ~ l_market_size + I(l_market_size^2) + l_market_area + I(l_market_area^2) + l_market_pop + I(l_market_pop^2) | 0 | 0 | 0) 
fit_rho <- felm(data = shift_switch_final,
                formula = change_rho_perc ~ l_market_size + I(l_market_size^2) + l_market_area + I(l_market_area^2) + l_market_pop + I(l_market_pop^2) | 0 | 0 | 0) 
fit_iterations <- felm(data = shift_switch_final,
                    formula = iterations ~ l_market_size + I(l_market_size^2) + l_market_area + I(l_market_area^2) + l_market_pop + I(l_market_pop^2) | 0 | 0 | 0) 
fit_share <- felm(data = shift_switch_final,
                formula = shift_share_perc ~ l_market_size + I(l_market_size^2) + l_market_area + I(l_market_area^2) + l_market_pop + I(l_market_pop^2) | 0 | 0 | 0) 

outfile_path <- file.path(tables_folder_path, 'execution_summary.tex')
latex_table <- stargazer(fit_profits, 
          fit_rho,
          fit_iterations,
          fit_share,
          se=list(coef(summary(fit_profits, robust = TRUE))[, 2],
                  coef(summary(fit_rho, robust = TRUE))[, 2],
                  coef(summary(fit_iterations, robust = TRUE))[, 2],
                  coef(summary(fit_share, robust = TRUE))[, 2]), 
          dep.var.labels = c('Profits change (\\%)', 
                             'CW change (\\%)', 
                             'Iterations', 
                             'Share of shifts (\\%)'),
          covariate.labels = c('Log firm count', 
                               'Log firm count$^2$',
                               'Log market area', 
                               'Log market area$^2$',
                               'Log market pop.', 
                               'Log market pop.$^2$'),
          omit = c("Constant"), 
          omit.stat = c('ser'),
          label = 'table:execution_summary',
          title = paste0('Relationship between market characteristics', 
                         'welfare improvements and optimization procedure execution features.',
                         'Robust standard errors reported in parentheses.'),
          type = 'latex', align = TRUE, digits = 2, digits.extra = 0)
cat(paste0(latex_table, collapse = '\n'),  file = outfile_path)

shift_switch <- shift_switch %>% 
    mutate(type = relevel(factor(type), ref = 'initial'))

# Report changes in market characteristics:
fit_markup_count <- felm(data = shift_switch,
                         formula = markup_count ~ type | cbsa | 0 | cbsa) 
fit_markup_density <- felm(data = shift_switch,
                           formula = markup_density ~ type | cbsa | 0 | cbsa)
fit_markup_distance <- felm(data = shift_switch,
                            formula = markup_distance ~ type | cbsa | 0 | cbsa)

fit_count_density0 <- felm(data = shift_switch,
                           formula = count_density ~ type | cbsa | 0 | cbsa) 
fit_count_density1 <- felm(data = shift_switch,
                           formula = count_density ~ type * l_market_area + type * l_market_pop + type * l_market_size | cbsa | 0 | cbsa) 

fit_delta_count <- felm(data = shift_switch,
                          formula = delta_count ~ type | cbsa | 0 | cbsa) 
fit_delta_density <- felm(data = shift_switch,
                           formula = delta_density ~ type | cbsa | 0 | cbsa)
fit_delta_distance <- felm(data = shift_switch,
                            formula = delta_distance ~ type | cbsa | 0 | cbsa)

outfile_path <- file.path(tables_folder_path, 'market_changes_summary.tex')
latex_table <- stargazer(fit_delta_count, 
                         fit_delta_density,
                         fit_delta_distance,
                         fit_markup_count,
                         fit_markup_density,
                         fit_markup_distance,
                         covariate.labels = c('Alt. config [vs status-quo]'),
                         dep.var.labels = c('Rest. count', 'Pop. dens.', 'W. distance',
                                            'Rest. count', 'Pop. dens.', 'W. distance',
                                            'Pop. dens.'),
                         omit.stat = c('ser'),
                         label = 'table:best_alternative_market_changes',
                         title = paste('Changes in market characteristics associated with the switch', 
                                        'from status-quo to best alternative configurations.',
                                       'W. distance refers to average distance from a given restaurant to consumers.',
                                        'Standard errors robust to heteroskedasticity and',
                                        'CBSA clustering reported in parentheses.'),
                         type = 'latex', align = TRUE, digits = 3, digits.extra = 0, 
                         add.lines=list(c("CBSA FE", rep('\\multicolumn{1}{c}{\\checkmark}', 6))))

latex_table <- c(latex_table[1:7],
                 '\\resizebox{\\textwidth}{!}{',
                 latex_table[8:11],
                 '& \\multicolumn{3}{c}{Correlation between quality and} & \\multicolumn{3}{c}{Correlation between markups and} \\\\',
                 '\\cmidrule(rr){2-4}\\cmidrule(rr){5-7}',
                 latex_table[12:25], 
                 '}',
                 latex_table[26])

' & \\multicolumn{1}{c}{Corr. between rest. count and}'
cat(paste0(latex_table, collapse = '\n'),  file = outfile_path)


outfile_path <- file.path(tables_folder_path, 'count_density_summary.tex')
latex_table <- stargazer(fit_count_density0,
                         fit_count_density1,
                         dep.var.caption = c('Dep. var.: corr. between rest. count and pop. density'),
                         omit.stat = c('ser'),
                         covariate.labels = c('Alt. config [vs status-quo]', 
                                              '1',
                                              '2',
                                              '3',
                                              '\\quad $\\times$ Log market area',
                                              '\\quad $\\times$ Log market pop.',
                                              '\\quad $\\times$ Log firm count'),
                         label = 'table:count_density_market_changes',
                         title = paste('Change in correlation between population density and restaurant count associated with the switch', 
                                       'from status-quo to best alternative configurations.',
                                       'Standard errors robust to heteroskedasticity and',
                                       'CBSA clustering reported in parentheses.'),
                         type = 'latex', align = TRUE, digits = 3, digits.extra = 0,
                         add.lines=list(c("CBSA FE", rep('\\multicolumn{1}{c}{\\checkmark}', 2))))
latex_table <- c(latex_table[1:11], latex_table[14:18], latex_table[28:length(latex_table)])
cat(paste0(latex_table, collapse = '\n'),  file = outfile_path)

###############################################################################