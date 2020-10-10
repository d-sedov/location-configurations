###############################################################################
#
# FILE: markup_estimation.R
#
# BY: Dmitry Sedov 
#
# DATE: Sun May 10 2020
#
# DESC: This code contains the code to estimate markup off area-optimization.
#
# IN:
#     0. CBSA - the identifier of the urban area.
#     1. Rho - the disutility of distance.
#     2. c_a - the coefficient on the area.
#
###############################################################################

################################ Libraries ####################################

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(gsubfn))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(stargazer))
library(kableExtra)
library(ggridges)
library(extrafont)

###############################################################################


################################ Constants ####################################

days <- 31
input_folder <- '/home/quser/project_dir/urban/data/output/spatial-demand/main_demand'
population_folder <- '/home/quser/project_dir/urban/data/output/descriptive'
output_folder <- '/home/quser/project_dir/urban/data/output/entry/markups'
summary_file_path <- file.path(input_folder, 'minimization_summary_optimized.csv')
rent_folder_path <- '/home/quser/project_dir/urban/data/output/rent'

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
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#ffe200', '#722ab5')

###############################################################################


################################ Functions ####################################

import_altered_visits <- function(cbsa){
  cbsa_folder_name <- paste0('cbsa', cbsa)
  main_file_path <- file.path(output_folder, 
                              cbsa_folder_name, 
                              paste0('all_visits_altered', cbsa, '.csv')
                              )
  cbsa_parts_path <- file.path(output_folder, 
                               cbsa_folder_name,
                               'parts'
                               )
  main_altered_visits <- read_csv(main_file_path, col_names = TRUE) 
    # %>% rename(sname_place_id = X1, visits = X2, altered_visits_p1 = X3, diff = X4)
  if (dir.exists(cbsa_parts_path)) {
    online_file_list <- list.files(cbsa_parts_path, pattern = 'online')
    parts_altered_visits <- lapply(online_file_list, 
                                   function(x) {
                                     part_path <- file.path(cbsa_parts_path, x)
                                     temp <- read_csv(part_path, col_names = FALSE) %>%
                                       rename(sname_place_id = X1, 
                                              visits = X2, 
                                              altered_visits_p1 = X3, 
                                              diff = X4)
                                   })
    parts_altered_visits <- bind_rows(parts_altered_visits)
    main_altered_visits <- bind_rows(main_altered_visits, parts_altered_visits)
  }
  main_altered_visits <- main_altered_visits %>% distinct(sname_place_id, 
                                                          .keep_all = TRUE)
  return(main_altered_visits)
}

restaurants_file_path <- function(x) {
  folder_name <- paste0('cbsa', x)
  file_name <- paste0('restaurants', x, '.csv')
  file_path <- file.path(input_folder, folder_name, file_name)
  return(file_path)
}


# Function to remove outliers
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 2 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

###############################################################################


############################ Data prepartation ################################

# Import data
loopnet_data <- read_csv(file.path(rent_folder_path, 
                                   'loopnet_listings.csv'))
crexi_data <- read_csv(file.path(rent_folder_path, 
                                 'crexi_listings.csv')) %>% 
  select(-X1) %>% mutate(source = 'Crexi')
commercialexchange_data <- read_csv(file.path(rent_folder_path, 
                                              'commercialexchange_listings.csv')) %>% 
  select(-X1) %>% mutate(source = 'CommercialExchange')
commercialcafe_data <- read_csv(file.path(rent_folder_path, 
                                          'commercialcafe_listings.csv')) %>% 
  select(-X1) %>% mutate(source = 'CommercialCafe')

all_rent_data <- loopnet_data %>% 
  select(-subtype) %>% 
  bind_rows(crexi_data) %>%
  bind_rows(commercialexchange_data) %>%
  mutate(source = 'All w/o CommercialCafe')

rent_by_zip <- all_rent_data %>% 
  group_by(zip_code) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE))

# Read the minimization summary file
minimization_summary_optimized <- read_csv(summary_file_path, col_names = FALSE)
# Get restaurant feautres
all_rest_paths <- sapply(minimization_summary_optimized$X1, restaurants_file_path)
rest_list <- lapply(all_rest_paths, read_csv, 
                    col_types = cols(zip_code = col_character(), 
                                     r_cbg = col_character(), 
                                     r_cbsa = col_character()))
restaurants <- bind_rows(rest_list)
# Get the altered visits
altered_visits_list <- lapply(minimization_summary_optimized$X1, 
                              import_altered_visits)
altered_visits <- bind_rows(altered_visits_list)

restaurants <- restaurants %>% left_join(altered_visits) 
restaurants <- restaurants %>% left_join(rent_by_zip, by = 'zip_code')
restaurants <- restaurants %>% mutate(estimated_markup = (10.7 * mean_rate) / diff)
restaurants <- restaurants %>% 
  mutate(price = replace(price, price %in% c(3, 4), 2))
restaurants <- restaurants %>% mutate(price = factor(price))

# Output the estimated markups
estimated_markups_outfile <- file.path(output_folder, 'restaurants_estimated_markups.csv')
write_csv(restaurants, estimated_markups_outfile)

# Remove outliers
restaurants <- restaurants %>%  
  mutate_at(vars(estimated_markup), 
            funs(remove_outliers))

p1 <- ggplot(data = restaurants %>% filter((!is.na(price)) & (price != -1)), 
       aes(x = estimated_markup, y = factor(price), fill = factor(stat(quantile)))) + 
  stat_density_ridges(geom = "density_ridges_gradient", 
                      calc_ecdf = TRUE, 
                      quantiles = 4,
                      size = 0.25,
                      quantile_lines = FALSE) + xlim(0, 30) + 
  scale_fill_manual(values = c('#00025a', '#1a2c8f', '#8081c1', '#CCC0E6'), name = 'Quartiles') +
  theme_bw(base_family = 'Times') + 
  scale_y_discrete(expand = expansion(mult = c(0.2, 1.25))) + 
  theme(legend.key.size = unit(0.125, 'inches'),
        plot.margin = margin(5, 5, 5, 5, 'pt')) + 
  my_theme +
  ylab('Price category') +
  xlab('Estimated markup')

ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model', 
                            'markups_distribution.pdf'), 
       device = cairo_pdf, plot = p1, width = 4.25, height = 2.25)
embed_fonts(file.path('/home/quser/project_dir/urban/output/plots/model', 
                      'markups_distribution.pdf'))

markups_summary <- restaurants %>% 
  filter(as.character(price) %in% c('1', '2')) %>% 
  group_by(price) %>%
  summarise(Mean = mean(estimated_markup, na.rm = TRUE),
            Q10 = quantile(estimated_markup, probs = 0.1, na.rm = TRUE),
            Q25 = quantile(estimated_markup, probs = 0.25, na.rm = TRUE),
            Q50 = quantile(estimated_markup, probs = 0.5, na.rm = TRUE),
            Q75 = quantile(estimated_markup, probs = 0.75, na.rm = TRUE),
            Q90 = quantile(estimated_markup, probs = 0.9, na.rm = TRUE)
           )

outfile = file.path('/home/quser/project_dir/urban/output/tables/model/', 
                    'markups_summary.tex')
kable(markups_summary, 'latex', booktabs = T, digits = 3) %>%
    cat(file = outfile)

###############################################################################


######################### Second iteration ####################################

# Import iterated markups
estimated_markups <- read_csv(file.path(output_folder, 'restaurants_estimated_markups.csv'))

# Remove outliers
estimated_markups <- estimated_markups %>%  
  mutate_at(vars(estimated_markup), 
            funs(remove_outliers))

p1 <- ggplot(data = estimated_markups %>% filter((!is.na(price)) & (price != -1)), 
             aes(x = estimated_markup, y = factor(price), fill = factor(stat(quantile)))) + 
  stat_density_ridges(geom = "density_ridges_gradient", 
                      calc_ecdf = TRUE, 
                      quantiles = 4,
                      size = 0.25,
                      quantile_lines = FALSE) + xlim(0, 30) + 
  scale_fill_manual(values = sapply(seq(0.25, 1, by = 0.25), function(x) {adjustcolor("#00279a", alpha.f = x)}), name = 'Quartiles') +
  theme_bw(base_family = 'Times') + 
  scale_y_discrete(expand = expansion(mult = c(0.2, 1.25)), labels = c('$', '$$')) + 
  theme(legend.key.size = unit(0.125, 'inches'),
        plot.margin = margin(5, 5, 5, 5, 'pt')) + 
  my_theme +
  ylab('Price category') +
  xlab('Estimated per-visit markup in dollars') + theme(legend.position = c(0.99, 0.99),
                                                        legend.justification = c("right", "top"),
                                                        legend.direction="vertical",
                                                        legend.box.just = "left",
                                                        legend.key.width = unit(0.3, "cm"),
                                                        legend.key.height = unit(0.3, "cm")) +
  theme(plot.margin = unit(c(0,0.15,0,0), "in"))

ggsave(filename = file.path('/home/quser/project_dir/urban/output/plots/model', 
                            'markups_distribution.pdf'), 
       device = cairo_pdf, plot = p1, width = 4.0, height = 2.5)
embed_fonts(file.path('/home/quser/project_dir/urban/output/plots/model', 
                      'markups_distribution.pdf'))


markups_summary <- estimated_markups %>% 
  filter(as.character(price) %in% c('1', '2')) %>% 
  group_by(price) %>%
  summarise(Mean = mean(estimated_markup, na.rm = TRUE),
            Q10 = quantile(estimated_markup, probs = 0.1, na.rm = TRUE),
            Q25 = quantile(estimated_markup, probs = 0.25, na.rm = TRUE),
            Med = quantile(estimated_markup, probs = 0.5, na.rm = TRUE),
            Q75 = quantile(estimated_markup, probs = 0.75, na.rm = TRUE),
            Q90 = quantile(estimated_markup, probs = 0.9, na.rm = TRUE)
  ) %>%
  mutate(price = strrep('$', price)) %>%
  rename(`Price` = price)

outfile = file.path('/home/quser/project_dir/urban/output/tables/model/', 
                    'markups_summary.tex')
kable(markups_summary, 'latex', booktabs = T, digits = 2) %>%
  row_spec(0, bold = T, italic = T) %>%
  cat(file = outfile)

estimated_markups <- estimated_markups %>%
  left_join(deltas)

sample <- estimated_markups %>% 
  mutate(price = replace(price, is.na(price), 0)) %>%
  mutate(price = replace(price, price == -1, 0)) %>%
  mutate(price = replace(price, price %in% c(3, 4), 2))
# Relevel the price category factor
sample <- sample %>% 
  mutate(price = as.character(price)) %>%
  mutate(price = factor(price)) %>% 
  mutate(price = relevel(price, ref = '1')) 

# Clean variables
sample <- sample %>% mutate(brands = replace(brands, is.na(brands), 'none'))
sample <- sample %>% mutate(category1 = replace(category1, is.na(category1), 'none'))

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

sample <- sample %>% mutate(log_area = log(area_m2))


fit_delta_markup <- felm(data = sample, delta ~ estimated_markup | r_cbsa + brands + category1 | 0 | r_cbsa + brands, 
                                   exactDOF = TRUE)

fit_markup_characteristics <- felm(data = sample, estimated_markup ~ price + rating + rating_2 | r_cbsa + brands + category1 | 0 | r_cbsa + brands, 
                                   exactDOF = TRUE)

outfile <- file.path('/home/quser/project_dir/urban/output/tables/model', 
                     'markups_quality_characteristics.tex')
latex_table <-
  stargazer(fit_delta_markup, 
          fit_markup_characteristics, 
          type = 'latex', 
          title = paste('Relationship between (1) restaurant quality and estimated markups;', 
                        '(2) estimated markups and restaurant characteristics.',
                        'Standard errors robust to market and brand clusterting in parentheses.'),
          label = 'tab:markups_quality_characteristics',
          table.placement = "!t",
          omit.stat = c('ser'), 
          omit = 2,
          dep.var.labels = c('$\\delta_r$', '$(p^*-mc)_r$'),
          covariate.labels = c('$(p^*-mc)_r$', 
                               'Price [\\$\\$ vs \\$]', 
                               'Rating', 
                               'Rating$^2$'),
          add.lines = list(`CBSA FE` = c('\\rowfont{\\footnotesize}CBSA FE', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                           `Brand FE` = c('\\rowfont{\\footnotesize}Brand FE', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}'), 
                           `Category FE` = c('\\rowfont{\\footnotesize}Category FE', '\\multicolumn{1}{c}{\\checkmark}', '\\multicolumn{1}{c}{\\checkmark}')
                           ),
          digits = 3, 
          digits.extra = 0,
          align = TRUE,
          star.cutoffs = c(0.1, 0.05, 0.01)
  )

latex_table <- gsub('tabular', 'tabu', latex_table, fixed = T)
latex_table <- gsub('Observations', '\\rowfont{\\footnotesize} Observations', latex_table, fixed = T)
latex_table <- gsub('R$^{2}$', '\\rowfont{\\footnotesize} R$^{2}$', latex_table, fixed = T)
latex_table <- gsub('Adjusted', '\\rowfont{\\footnotesize} Adjusted', latex_table, fixed = T)
cat(paste(latex_table, collapse = '\n'), file = outfile)

###############################################################################