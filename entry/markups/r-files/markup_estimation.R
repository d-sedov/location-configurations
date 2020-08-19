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
       aes(x = estimated_markup, y = price, fill = factor(stat(quantile)))) + 
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
