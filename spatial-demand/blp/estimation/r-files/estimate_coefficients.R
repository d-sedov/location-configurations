###############################################################################
#
# FILE: estimate_coefficients.R
#
# BY: Dmitry Sedov 
#
# DATE: Tue May 5 2020
#
# DESC: This code contains the code for the estimation of mean coefficients in 
#       the delta-regression.
#
# IN:
#     0. Estimated deltas.
#     1. Data frame of instruments.
#
###############################################################################


################################ Libraries ####################################

library(readr)
library(dplyr)
library(lfe)
library(dplyr)
library(ggplot2)
library(gsubfn)
library(data.table)
library(stargazer)

###############################################################################


################################ Constants ####################################

days = 31
input_folder = '/home/quser/project_dir/urban/data/output/spatial-demand/main_demand'
descriptive_folder_path <- '/home/quser/project_dir/urban/data/output/descriptive'
tables_output_folder = '/home/quser/project_dir/urban/output/tables/model'
plot_output_folder = '/home/quser/project_dir/urban/output/plots/model'
markup_output_folder = '/home/quser/project_dir/urban/data/output/entry/markups'
summary_file_path = file.path(input_folder, 'minimization_summary_optimized.csv')

# My theme for plots 
my_theme <- theme(legend.text = element_text(size = 10),
                  legend.title = element_text(size = 12),
                  plot.title = element_text(hjust = 0.5, size = 14),
                  axis.text = element_text(size = 10),
                  axis.title = element_text(size = 12)
)
mycolorscheme1 <- c('black', 'orange', 'purple')
mycolorscheme2 <- c('blue', 'red', 'darkgreen')
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

###############################################################################


################################# Functions ###################################

deltas_file_path <- function(x) {
  folder_name <- paste0('cbsa', x)
  file_name <- paste0('deltas_optimized', x, '.csv')
  file_path <- file.path(input_folder, folder_name, file_name)
  return(file_path)
}

restaurants_file_path <- function(x) {
  folder_name <- paste0('cbsa', x)
  file_name <- paste0('restaurants', x, '.csv')
  file_path <- file.path(input_folder, folder_name, file_name)
  return(file_path)
}

###############################################################################


################################# Main code ###################################

# Read the minimization summary file
minimization_summary_optimized <- read_csv(summary_file_path, col_names = FALSE)

# Get deltas for all restaurants
all_delta_paths <- sapply(minimization_summary_optimized$X1, deltas_file_path)
delta_list <- lapply(all_delta_paths, read_csv)
deltas <- bind_rows(delta_list) 

# CHECK LATER
deltas <- deltas %>% filter(delta <= 0)

# Get restaurant feautres
all_rest_paths <- sapply(minimization_summary_optimized$X1, restaurants_file_path)
rest_list <- lapply(all_rest_paths, read_csv, 
                    col_types = cols(zip_code = col_character(), 
                                     r_cbg = col_character(), 
                                     r_cbsa = col_character()))
restaurants <- bind_rows(rest_list)

# Get instruments
restaurants_neighbors_features <- read_csv(file.path(input_folder, 
                                                     'restaurants_neighbors_features.csv')
)

# Clean variables
restaurants <- restaurants %>% mutate(brands = replace(brands, is.na(brands), 'none'))
restaurants <- restaurants %>% mutate(category1 = replace(category1, is.na(category1), 'none'))

# Join with instruments
restaurants <- left_join(restaurants, restaurants_neighbors_features)
restaurants <- left_join(restaurants, deltas)

sample <- restaurants

# Group missing price together; group high-price restaurants together
#sample <- sample %>% 
#  mutate(price = replace(price, is.na(price), 0)) %>%
#  mutate(price = replace(price, price == -1, 0))
sample <- sample %>% 
  mutate(price = replace(price, is.na(price), 0)) %>%
  mutate(price = replace(price, price == -1, 0)) %>%
  mutate(price = replace(price, price %in% c(3, 4), 2))
# Relevel the price category factor
sample <- sample %>% 
  mutate(price = as.character(price)) %>%
  mutate(price = factor(price)) %>% 
  mutate(price = relevel(price, ref = '1')) 

# Group small brands an categories into one
sample <- sample %>% group_by(brands) %>% 
  mutate(n_brands = n()) %>% 
  ungroup() %>%
  mutate(brands = replace(brands, n_brands <= 100, 'none')) %>%
  select(-n_brands)

sample <- sample %>% group_by(category1) %>% 
  mutate(n_category1 = n()) %>% 
  ungroup() %>%
  mutate(category1 = replace(category1, n_category1 <= 100, 'none')) %>%
  select(-n_category1)

sample <- sample %>% mutate(rating_2 = rating ^ 2)

# Create the missing indicators for time and rating
sample <- sample %>% 
  mutate(time_not_avail = is.na(total_minutes_open)) %>% 
  mutate(total_minutes_open = replace(total_minutes_open, time_not_avail, 0))
sample <- sample %>% 
  mutate(rating_not_avail = is.na(rating)) %>% 
  mutate(rating = replace(rating, rating_not_avail, 0), 
         rating_2 = replace(rating_2, rating_not_avail, 0))

sample <- sample %>% mutate(area_m2_2 = area_m2 ^ 2, neighbor_rating_2 = neighbor_rating ^ 2)
sample <- sample %>% mutate(sqrt_area_m2 = sqrt(area_m2))
sample <- sample %>% mutate(log_area = log(area_m2))

# Options to consider: look or not look at the non-matched restaurants
# Logs instead of levels
# Instruments
# Regressors: include / exclude
# Reporting coefficients: price, rating, rating^2, area, n_categories, total_minutes_open

# fit_ols <- felm(data = sample, delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby + area_m2 | r_cbsa + brands + category1 )
# fit_iv <- felm(data = sample, delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby | r_cbsa + brands + category1 | (area_m2 ~ neighbor_rating), exactDOF = TRUE)

# fit_ols <- felm(data = sample, delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby + log_area | r_cbsa + brands + category1 | 0 | r_cbsa + brands + category1, exactDOF = TRUE)
# fit_iv <- felm(data = sample, delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby | r_cbsa + brands + category1 | (log_area ~ neighbor_rating + neighbor_n_categories + neighbor_category1_equal) | r_cbsa + brands + category1, exactDOF = TRUE)
# cluster_f_stat<- condfstat(fit_iv)

# Get robust se
options(lfe.robust = TRUE)
fit_ols_no_brand_category <- felm(data = sample, delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby + log_area | r_cbsa | 0,
                                  exactDOF = TRUE)
fit_ols_main <- felm(data = sample, delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby + log_area | r_cbsa + brands + category1 | 0, 
                     exactDOF = TRUE)
fit_ols_main_iv_sample <- felm(data = sample %>% filter(!(is.na(neighbor_rating) | is.na(neighbor_n_categories) | is.na(neighbor_category1_equal))), 
                               delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby + log_area | r_cbsa + brands + category1 | 0, 
                               exactDOF = TRUE)
fit_iv <- felm(data = sample, delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby | r_cbsa + brands + category1 | (log_area ~ neighbor_rating + neighbor_n_categories + neighbor_category1_equal) | 0,
               exactDOF = TRUE)
robust_f_stat <- condfstat(fit_iv, 'robust')
area_coefficient <- fit_iv$coefficients[length(fit_iv$coefficients)]

# Get cluster se
fit_ols_no_brand_category_cluster <- felm(data = sample, delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby + log_area | r_cbsa | 0 | r_cbsa + brands,
                                  exactDOF = TRUE)
fit_ols_main_cluster <- felm(data = sample, delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby + log_area | r_cbsa + brands + category1 | 0 | r_cbsa + brands, 
                     exactDOF = TRUE)
fit_ols_main_iv_sample_cluster <- felm(data = sample %>% filter(!(is.na(neighbor_rating) | is.na(neighbor_n_categories) | is.na(neighbor_category1_equal))), 
                               delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby + log_area | r_cbsa + brands + category1 | 0 | r_cbsa + brands,  
                               exactDOF = TRUE)
fit_iv_cluster <- felm(data = sample, delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby | r_cbsa + brands + category1 | (log_area ~ neighbor_rating + neighbor_n_categories + neighbor_category1_equal) | r_cbsa + brands,
               exactDOF = TRUE)
cluster_f_stat <- condfstat(fit_iv_cluster, 'cluster')

# Write files for markup estimation
extra_cbsa_path <- file.path(markup_output_folder, 'markup_list_extra.csv')
large_cbsa_path <- file.path(markup_output_folder, 'markup_list_large.csv')
small_cbsa_path <- file.path(markup_output_folder, 'markup_list_small.csv')
minimization_summary_optimized <- minimization_summary_optimized %>%
  mutate(X4 = area_coefficient) %>% select(-X3)
small_cbsa_out <- minimization_summary_optimized %>%
  slice(1 : (n() - 60)) # %>% filter(X1 != '16980')
large_cbsa_out <- minimization_summary_optimized %>% 
  slice((n() - 59) : (n() - 25)) # %>% filter(X1 != '16980')
extra_cbsa_out <- minimization_summary_optimized %>% 
  slice((n() - 24) : (n())) # %>% filter(X1 != '16980')

write_csv(small_cbsa_out, small_cbsa_path, col_names = FALSE)
write_csv(large_cbsa_out, large_cbsa_path, col_names = FALSE)
write_csv(extra_cbsa_out, extra_cbsa_path, col_names = FALSE)

# Rename variables in iv for stargazer output
rownames(fit_iv$coefficients)[rownames(fit_iv$coefficients) == "`log_area(fit)`"] <- 'log_area'
rownames(fit_iv$beta)[rownames(fit_iv$beta) == "`log_area(fit)`"] <- 'log_area'
rownames(fit_iv_cluster$coefficients)[rownames(fit_iv_cluster$coefficients) == "`log_area(fit)`"] <- 'log_area'
rownames(fit_iv_cluster$beta)[rownames(fit_iv_cluster$beta) == "`log_area(fit)`"] <- 'log_area'

# Options for lfe output
rows_to_omit <- c(1, 3, 7, 8, 9, 10, 11)
# Create the regression table
latex_table_robust <- 
  stargazer(fit_ols_no_brand_category,
          fit_ols_main,
          fit_ols_main_iv_sample,
          fit_iv,
          title = paste('Estimates of restaurant characteristics coefficients.', 
                        'Robust standard errors in parentheses.',
                        'IV column instruments for log restaurant area with neighbor-restaurant characteristics.', 
                        'Lower number of observations in the IV column due to missing instrument.',
                        'Column (3) estimated on the IV sample.', 
                        'Coefficient on the missing price category dummy is omitted from the output,', 
                        'being slightly negative vs the \\$-category baseline.'),
          label = 'tab:demand_characteristics_robust',
          type = 'latex',
          table.placement = "!t",
          omit = rows_to_omit, 
          omit.stat = c('rsq', 'adj.rsq', 'ser'),
          add.lines = list(`CBSA FE` = c('\\rowfont{\\footnotesize}CBSA FE', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                           `Brand FE` = c('\\rowfont{\\footnotesize}Brand FE', '', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                           `Category FE` = c('\\rowfont{\\footnotesize}Category FE', '', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                           `Open` = c('\\rowfont{\\footnotesize}Time controls', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                           `Nearby` = c('\\rowfont{\\footnotesize}Location controls', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                           `Frob` = c('\\rowfont{\\footnotesize} F-stat (robust)', '', '', '', paste0('\\multicolumn{1}{c}{', as.character(round(robust_f_stat, 3)), '}')),
                           `Fclus` = c('\\rowfont{\\footnotesize} F-stat (cluster)', '', '', '', paste0('\\multicolumn{1}{c}{', as.character(round(cluster_f_stat, 3)), '}'))
          ),
          digits = 3, 
          digits.extra = 0,
          align = TRUE,
          column.labels = c('FE OLS', 'IV'),
          column.separate = c(3, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = '',
          covariate.labels = c('Price [\\$\\$ vs \\$]', 
                               'Rating', 
                               'Rating$^2$', 
                               '\\# of categories', 
                               'Log area (sq. m.)'),
          star.cutoffs = c(0.1, 0.05, 0.01)
          )
          
latex_table_cluster <- 
  stargazer(fit_ols_no_brand_category_cluster,
            fit_ols_main_cluster,
            fit_ols_main_iv_sample_cluster,
            fit_iv_cluster,
            type = 'latex',
            table.placement = "!t",
            omit = rows_to_omit, 
            omit.stat = c('rsq', 'adj.rsq', 'ser'),
            add.lines = list(`CBSA FE` = c('\\rowfont{\\footnotesize}CBSA FE', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Brand FE` = c('\\rowfont{\\footnotesize}Brand FE', '', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Category FE` = c('\\rowfont{\\footnotesize}Category FE', '', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Open` = c('\\rowfont{\\footnotesize}Time controls', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Nearby` = c('\\rowfont{\\footnotesize}Location controls', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Frob` = c('\\rowfont{\\footnotesize} F-stat (robust)', '', '', '', paste0('\\multicolumn{1}{c}{', as.character(round(robust_f_stat, 3)), '}')),
                             `Fclus` = c('\\rowfont{\\footnotesize} F-stat (cluster)', '', '', '', paste0('\\multicolumn{1}{c}{', as.character(round(cluster_f_stat, 3)), '}'))
            ),
            digits = 3, 
            digits.extra = 0,
            align = TRUE,
            column.labels = c('FE OLS', 'IV'),
            column.separate = c(3, 1),
            report = 'vcs*',
            dep.var.labels.include = FALSE,
            dep.var.caption = '',
            covariate.labels = c('Price [\\$\\$ vs \\$]', 
                                 'Rating', 
                                 'Rating$^2$', 
                                 '\\# of categories', 
                                 'Log area (sq. m.)'),
            star.cutoffs = c(0.1, 0.05, 0.01)
  )

latex_table_robust <- gsub('tabular', 'tabu', latex_table_robust, fixed = T)
latex_table_robust <- gsub('Observations', '\\rowfont{\\footnotesize} Observations', latex_table_robust, fixed = T)
latex_table_robust <- c(latex_table_robust[1:10],
                        '\\cmidrule(l){2-4} \\cmidrule(l){5-5}', 
                        latex_table_robust[11:40])
# Output robust-only SEs
outfile_robust <- file.path(tables_output_folder, 
                            'demand_characterstics_robust.tex')
cat(paste0(latex_table_robust[c(1:34,36:40)], collapse = '\n'), 
    file = outfile_robust)

# Output cluster SEs
latex_table_robust_cluster <- 
  stargazer(fit_ols_no_brand_category,
            fit_ols_main,
            fit_ols_main_iv_sample,
            fit_iv,
            title = paste('Estimates of restaurant characteristics coefficients.', 
                          'Robust standard errors in round parentheses,', 
                          'standard errors robust to market and brand clustering in square parentheses.',
                          'IV column instruments for log restaurant area with neighbor-restaurant characteristics.', 
                          'Lower number of observations in the IV column due to missing instrument.',
                          'Column (3) estimated on the IV sample.',
                          'Coefficient on the missing price category dummy is omitted from the output,', 
                          'being slightly negative vs the \\$-category baseline and insignificant with clustered standard errors.'),
            label = 'tab:demand_characteristics_cluster',
            type = 'latex',
            table.placement = "!t",
            omit = rows_to_omit, 
            omit.stat = c('rsq', 'adj.rsq', 'ser'),
            add.lines = list(`CBSA FE` = c('\\rowfont{\\footnotesize}CBSA FE', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Brand FE` = c('\\rowfont{\\footnotesize}Brand FE', '', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Category FE` = c('\\rowfont{\\footnotesize}Category FE', '', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Open` = c('\\rowfont{\\footnotesize}Time controls', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Nearby` = c('\\rowfont{\\footnotesize}Location controls', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Frob` = c('\\rowfont{\\footnotesize} F-stat (robust)', '', '', '', paste0('\\multicolumn{1}{c}{', as.character(round(robust_f_stat, 3)), '}')),
                             `Fclus` = c('\\rowfont{\\footnotesize} F-stat (cluster)', '', '', '', paste0('\\multicolumn{1}{c}{', as.character(round(cluster_f_stat, 3)), '}'))
            ),
            digits = 3, 
            digits.extra = 0,
            align = TRUE,
            report = 'vcs*',
            column.labels = c('FE OLS', 'IV'),
            column.separate = c(3, 1),
            dep.var.labels.include = FALSE,
            dep.var.caption = '',
            covariate.labels = c('Price [\\$\\$ vs \\$]', 
                                 'Rating', 
                                 'Rating$^2$', 
                                 '\\# of categories', 
                                 'Log area (sq. m.)'),
            star.cutoffs = c(0.1, 0.05, 0.01)
  )
cluster_se <- latex_table_cluster[seq(14, 26, by = 3)]
cluster_se <- gsub('(', '[', cluster_se, fixed = T)
cluster_se <- gsub(')', ']', cluster_se, fixed = T)
latex_table_robust_cluster[seq(15, 27, by = 3)] <- paste(cluster_se, '\\addlinespace', sep = '\n')

latex_table_robust_cluster <- gsub('tabular', 'tabu', latex_table_robust_cluster, fixed = T)
latex_table_robust_cluster <- gsub('Observations', '\\rowfont{\\footnotesize} Observations', latex_table_robust_cluster, fixed = T)
latex_table_robust_cluster <- c(latex_table_robust_cluster[1:10], 
                                '\\cmidrule(l){2-4} \\cmidrule(l){5-5}', 
                                latex_table_robust_cluster[11:40])
outfile_cluster <- file.path(tables_output_folder, 
                            'demand_characterstics_cluster.tex')
cat(paste0(latex_table_robust_cluster, collapse = '\n'), 
    file = outfile_cluster)

first_stage_robust <- 
  stargazer(fit_iv$stage1,
            title = paste('First stage estimates of coefficients on excluded instruments',
                          'in the IV regression of restaurant quality on characteristics.', 
                          'Robust standard errors in parentheses.'),
            label = 'tab:demand_characteristics_fs_robust',
            type = 'latex',
            dep.var.labels = 'Log area (sq. m.)',
            table.placement = "!t",
            omit = 1:11, 
            omit.stat = c('rsq', 'adj.rsq', 'ser'),
            add.lines = list(`CBSA FE` = c('\\rowfont{\\footnotesize}CBSA FE', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Brand FE` = c('\\rowfont{\\footnotesize}Brand FE', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Category FE` = c('\\rowfont{\\footnotesize}Category FE', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Open` = c('\\rowfont{\\footnotesize}Time controls', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Nearby` = c('\\rowfont{\\footnotesize}Location controls', '\\multicolumn{1}{c}{\\checkmark}')
                             ),
            digits = 3, 
            digits.extra = 0,
            align = TRUE,
            dep.var.labels.include = FALSE,
            dep.var.caption = 'Log area (sq. m.)',
            covariate.labels = c('Average neighor rating', 
                                 'Average neighor \\# of categories', 
                                 'Share of neighbors with same cuisine'),
            star.cutoffs = c(0.1, 0.05, 0.01)
  )
first_stage_cluster <- 
  stargazer(fit_iv_cluster$stage1,
            title = paste('First stage estimates of coefficients on excluded instruments',
                          'in the IV regression of restaurant quality on characteristics.', 
                          'Robust standard errors in parentheses.'),
            label = 'tab:demand_characteristics_fs_robust',
            type = 'latex',
            table.placement = "!t",
            omit = 1:11, 
            omit.stat = c('rsq', 'adj.rsq', 'ser'),
            add.lines = list(`CBSA FE` = c('\\rowfont{\\footnotesize}CBSA FE', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Brand FE` = c('\\rowfont{\\footnotesize}Brand FE', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Category FE` = c('\\rowfont{\\footnotesize}Category FE', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Open` = c('\\rowfont{\\footnotesize}Time controls', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Nearby` = c('\\rowfont{\\footnotesize}Location controls', '\\multicolumn{1}{c}{\\checkmark}')
            ),
            digits = 3, 
            digits.extra = 0,
            align = TRUE,
            dep.var.labels.include = FALSE,
            dep.var.caption = 'Log area (sq. m.)',
            covariate.labels = c('Average neighor rating', 
                                 'Average neighor \\# of categories', 
                                 'Share of neighbors with same cuisine'),
            star.cutoffs = c(0.1, 0.05, 0.01)
  )

first_stage_robust <- gsub('tabular', 'tabu', first_stage_robust, fixed = T)
first_stage_robust <- gsub('Observations', '\\rowfont{\\footnotesize} Observations', first_stage_robust, fixed = T)
# Output robust-only FS SEs
outfile_robust_fs <- file.path(tables_output_folder, 
                            'demand_characterstics_fs_robust.tex')
cat(paste0(first_stage_robust, collapse = '\n'), file = outfile_robust_fs)


# Output cluster FS SEs
first_stage_robust_cluster <- 
  stargazer(fit_iv$stage1,
            title = paste('First stage estimates of coefficients on excluded instruments',
                          'in the IV regression of restaurant quality on characteristics.', 
                          'Robust standard errors in round parentheses,', 
                          'standard errors robust to market and brand clustering in square parentheses.'),
            label = 'tab:demand_characteristics_fs_cluster',
            type = 'latex',
            table.placement = "!t",
            omit = 1:11, 
            omit.stat = c('rsq', 'adj.rsq', 'ser'),
            add.lines = list(`CBSA FE` = c('\\rowfont{\\footnotesize}CBSA FE', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Brand FE` = c('\\rowfont{\\footnotesize}Brand FE', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Category FE` = c('\\rowfont{\\footnotesize}Category FE', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Open` = c('\\rowfont{\\footnotesize}Time controls', '\\multicolumn{1}{c}{\\checkmark}'), 
                             `Nearby` = c('\\rowfont{\\footnotesize}Location controls', '\\multicolumn{1}{c}{\\checkmark}')
            ),
            digits = 3, 
            digits.extra = 0,
            align = TRUE,
            dep.var.labels.include = FALSE,
            dep.var.caption = 'Log area (sq. m.)',
            covariate.labels = c('Average neighor rating', 
                                 'Average neighor \\# of categories', 
                                 'Share of neighbors with same cuisine'),
            star.cutoffs = c(0.1, 0.05, 0.01)
  )
fs_cluster_se <- first_stage_cluster[seq(14, 20, by = 3)]
fs_cluster_se <- gsub('(', '[', fs_cluster_se, fixed = T)
fs_cluster_se <- gsub(')', ']', fs_cluster_se, fixed = T)
first_stage_robust_cluster[seq(15, 21, by = 3)] <- paste(fs_cluster_se, '\\addlinespace', sep = '\n')

first_stage_robust_cluster <- gsub('tabular', 'tabu', first_stage_robust_cluster, fixed = T)
first_stage_robust_cluster <- gsub('Observations', '\\rowfont{\\footnotesize} Observations', first_stage_robust_cluster, fixed = T)
outfile_cluster_fs <- file.path(tables_output_folder, 
                             'demand_characterstics_fs_cluster.tex')
cat(paste0(first_stage_robust_cluster, collapse = '\n'), 
    file = outfile_cluster_fs)

# Plot the distribution of rhos
rhos_plot <- ggplot(minimization_summary_optimized, 
                    aes(X2)) + 
  geom_density(color = mycolorscheme3[1], 
               fill = mycolorscheme3[1]) + 
  theme_bw(base_family = 'Times') + 
  my_theme + xlab('') +
  ylab('Density')

deltas_plot <- ggplot(deltas,
                      aes(delta)) +
  geom_density(color = mycolorscheme3[2], 
               fill = mycolorscheme3[2]) + 
  theme_bw(base_family = 'Times') + 
  my_theme + xlab('') + ylab('Density')

ggsave(filename = file.path(plot_output_folder, 'rhos.pdf'),
       device = cairo_pdf,
       plot = rhos_plot,
       width = 4.5,
       height = 2.5)

ggsave(filename = file.path(plot_output_folder, 'deltas.pdf'),
       device = cairo_pdf,
       plot = deltas_plot,
       width = 4.5,
       height = 2.5)

rhos_only <- minimization_summary_optimized %>% 
  select(X2) %>% 
  rename(Rho = X2) %>%
  mutate(rn = row_number())
deltas_only <- deltas %>% 
  select(delta) %>% 
  rename(Delta = delta) %>%
  mutate(rn = row_number())
rhos_deltas <- merge(deltas_only, rhos_only, by = 'rn', all = TRUE) %>% select(-rn)
latex_table <- stargazer(as.data.frame(rhos_deltas), 
          title = 'Summary statistics on $\\rho_m$ and $\\delta_r$',
          label = 'tab:rhos_deltas_summary',
          covariate.labels = c('$\\delta_r$', '$-\\rho_m$'),
          type = 'latex',
          iqr = T, median = T, digits = 2, table.placement = "!t",
          align = TRUE)

latex_table[11] <- paste0('\\rowfont{\\bfseries\\itshape}', latex_table[11])
latex_table[11] <- gsub('Pctl(25)', 'Q25', latex_table[11], fixed = T)
latex_table[11] <- gsub('Pctl(75)', 'Q75', latex_table[11], fixed = T)
latex_table[11] <- gsub('St. Dev.', 'SD', latex_table[11], fixed = T)
latex_table[8] <- gsub('-2', '2.2', latex_table[8])
latex_table[8] <- sub('D{.}{.}{2.2}', 'l', latex_table[8], fixed = T)
latex_table[8] <- gsub('tabular', 'tabu', latex_table[8])
latex_table[16] <- gsub('tabular', 'tabu', latex_table[16])

cat(paste0(latex_table, collapse = '\n'), 
    file = file.path(tables_output_folder,
                     'rhos_deltas_summary.tex'))

# Projecting distance coefficients on market characteristics

restaurants_desc <- read_csv(file.path(descriptive_folder_path, 'data_restaurants.csv'))
cbgs_desc <- read_csv(file.path(descriptive_folder_path, 'data_cbg.csv'))
census_population_desc <- read_csv(file.path(descriptive_folder_path,
                                        'cbg_population.csv')) %>%
  rename(cbg = home_cbg)

rest_by_cbsa <- restaurants_desc %>% group_by(cbsa) %>% summarize(market_size = n())
pop_area_by_cbsa <- cbgs_desc %>%
  left_join(census_population_desc) %>% 
  group_by(cbsa) %>% 
  summarize(market_pop = sum(total_pop),
            market_area = sum(area_m2))

distance_cost_across_markets <- minimization_summary_optimized %>%
  select(X1, X2) %>% 
  rename(cbsa = X1, rho = X2) %>% 
  left_join(rest_by_cbsa) %>% 
  left_join(pop_area_by_cbsa) %>%
  mutate(l_market_size = log(market_size),
         l_market_area = log(market_area),
         l_market_pop = log(market_pop))

distance_cost_across_markets <- distance_cost_across_markets %>%
  mutate(rho = -rho)

fit_rho1 <- felm(data = distance_cost_across_markets,
                 formula = rho ~ l_market_area + I(l_market_area^2) | 0 | 0 | 0)
fit_rho2 <- felm(data = distance_cost_across_markets,
                 formula = rho ~ l_market_size + I(l_market_size^2) | 0 | 0 | 0) 
fit_rho3 <- felm(data = distance_cost_across_markets,
                 formula = rho ~ l_market_pop + I(l_market_pop^2) | 0 | 0 | 0)
fit_rho4 <- felm(data = distance_cost_across_markets,
                 formula = rho ~  l_market_area + I(l_market_area^2) + l_market_size + I(l_market_size^2) + l_market_pop + I(l_market_pop^2) | 0 | 0 | 0)


outfile_path <- file.path(tables_output_folder, 'rho_decomposition.tex')
latex_table <- stargazer(fit_rho1, fit_rho2, fit_rho3, fit_rho4, 
                         se=list(coef(summary(fit_rho1, robust = TRUE))[, 2],
                                 coef(summary(fit_rho2, robust = TRUE))[, 2],
                                 coef(summary(fit_rho3, robust = TRUE))[, 2],
                                 coef(summary(fit_rho4, robust = TRUE))[, 2]),  
                         dep.var.labels = c('$\\rho_m$'),
                         covariate.labels = c('Log market area', 
                                              'Log market area$^2$',
                                              'Log firm count', 
                                              'Log firm count$^2$',
                                              'Log market pop.', 
                                              'Log market pop.$^2$'),
                         omit = c("Constant"), 
                         omit.stat = c('ser'),
                         label = 'tab:rho_decomposition',
                         title = paste('Relationship between market characteristics', 
                                        'and the estimated distance costs.',
                                        'Robust standard errors reported in parentheses.'),
                         type = 'latex', align = TRUE, digits = 3, digits.extra = 0)
cat(paste0(latex_table, collapse = '\n'),  file = outfile_path)
###############################################################################