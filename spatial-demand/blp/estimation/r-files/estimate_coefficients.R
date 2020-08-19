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
fit_iv <- felm(data = sample, delta ~ price + factor(rating_not_avail) + rating + rating_2 + n_categories + factor(time_not_avail) + total_minutes_open + est_nearby + is_part + devices_nearby | r_cbsa + brands + category1 | (log_area ~ neighbor_rating + neighbor_n_categories + neighbor_category1_equal) | 0,
               exactDOF = TRUE)
robust_f_stat <- condfstat(fit_iv, 'robust')
area_coefficient <- fit_iv$coefficients[length(fit_iv$coefficients)]

# Redo with robust standard errors

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

# Options for lfe output
rows_to_omit <- c(1, 3, 7, 8, 9, 10, 11)
# Create the regression table
stargazer(fit_ols_no_brand_category,
          fit_ols_main,
          fit_iv,
          type = 'latex',
          omit = rows_to_omit, 
          omit.stat = c('rsq', 'adj.rsq', 'ser'),
          add.lines = list(`CBSA FE` = c('CBSA FE', 'Yes', 'Yes', 'Yes'), 
                           `Brand FE` = c('Brand FE', 'No', 'Yes', 'Yes'), 
                           `Category FE` = c('Category FE', 'No', 'Yes', 'Yes'), 
                           `Open` = c('Time controls', 'Yes', 'Yes', 'Yes'), 
                           `Nearby` = c('Location controls', 'Yes', 'Yes', 'Yes'), 
                           `F` = c('F-stat', '', '', as.character(round(robust_f_stat, 3)))
                           ),
          digits = 3, 
          digits.extra = 0,
          align = TRUE,
          out = file.path(tables_output_folder,
                          'demand_characteristics_estimates.tex'),
          notes = 'Robust standard errors in parentheses.',
          notes.append = TRUE,
          column.labels = c('FE OLS', 'IV'),
          column.separate = c(2, 1),
          dep.var.labels.include = FALSE,
          dep.var.caption = '',
          covariate.labels = c('Price [$$ vs $]', 
                               'Rating', 
                               'Rating2', 
                               '# of categories', 
                               'Log area (sq. m.)'),
          star.cutoffs = c(0.1, 0.05, 0.01)
          )
          
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
stargazer(as.data.frame(rhos_deltas), 
          type = 'latex',
          align = TRUE, 
          out = file.path(tables_output_folder,
                          'rhos_deltas_summary.tex'))

###############################################################################