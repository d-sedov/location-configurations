################################################################################
################################################################################
#
# FILE: locations-summary.R
#
# BY: Dmitry Sedov 
#
# CREATED: Thu Feb 13 2020
#
# DESC: This file creates a table with locations (CBGs / CTs) summary statistics.
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
year = '2018'
month = 'October'

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

################################# Functions ####################################

# This function computes the summary statistics for a variable
simple_summary <- function(data, var) {
    require(dplyr)
    require(tibble)
    data %>% 
        select(!!sym(var)) %>% 
        summarise_all(list(perc_na = function(x){100 * 
                sum(is.na(x)) / length(x)},
                q10 = ~quantile(., 0.1, na.rm = T),
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

################################################################################

############################## Prepare data ####################################

# Import data
cbgs <- read_csv(file.path(input_folder_path, 'data_cbg.csv'))

# Replace missing values with 0 (no restaurants / establishments in cbgs)
cbgs <- cbgs %>% 
    mutate(urban = !is.na(cbsa),
           rest_number = replace(rest_number, 
                                 which(is.na(rest_number)),
                                 0),
           est_number = replace(est_number,
                                which(is.na(est_number)),
                                0),
           rest_area = replace(rest_area, 
                               which(is.na(rest_area)),
                               0),
           est_area = replace(est_area,
                              which(is.na(est_area)),
                              0),
           area_km2 = area_m2 / 1000000,
           number_devices_residing = replace(number_devices_residing,
                                             which(is.na(number_devices_residing)),
                                             0)
    )

# Import demographics info
census_cbg_extract <- read_csv(file.path(input_folder_path, 'census_cbg_extract.csv'),
                               col_types = cols_only(cbg_id = col_guess(),
                                                     median_hh_income = col_guess(), 
                                                     somecoll_over25y_pop = col_guess()
                                                     )
                               )

cbgs <- cbgs %>% left_join(census_cbg_extract, by = c('cbg' = 'cbg_id'))

# Select variables for summary statistics construction
vars <- list('area_km2',
             'rest_number',
             'rest_area',
             'est_number',
             'est_area',
             'number_devices_residing',
             'median_hh_income'
)

# Variable names for presentation
vars_names <- c('Area (sq. km.)',
                'Restaurant number', 
                'Total rest. area (sq. m.)',
                'Establishment number', 
                'Total estab. area (sq. m.)',
                'Devices',
                'Median HH income'
                )

cbgs_urban <- cbgs %>% filter(urban)
# Create urban-locations summary table
cbgs_urban_summary_table <- bind_rows(lapply(vars,
                                              function(x) {
                                                  simple_summary(cbgs_urban, x)
                                              }
                                              )
                                       ) %>%
    add_column(variable = vars_names,
               .before = 1) %>%
    select(-c(v, perc_na)) %>% 
    add_column(type = 'Urban',
               .before = 1) 

cbgs_rural <- cbgs %>% filter(!urban)
# Create rural-locations summary table
cbgs_rural_summary_table <- bind_rows(lapply(vars,
                                             function(x) {
                                                 simple_summary(cbgs_rural, x)
                                             }
                                             )
                                      ) %>%
    add_column(variable = vars_names,
               .before = 1) %>%
    select(-c(v, perc_na)) %>% 
    add_column(type = 'Rural',
               .before = 1)

cbgs_summary_table <- rbind(cbgs_urban_summary_table, 
                            cbgs_rural_summary_table) %>%
    arrange(desc(type)) %>%
    select(-type)
    
# Rename columns:
colnames(cbgs_summary_table) <- c('',
                                  'Q10',
                                  'Q25',
                                  'Med', 
                                  'Q75',
                                  'Q90',
                                  'Mean',
                                  'SD')

# Save the summary statistics table
outfile = file.path(tables_folder_path, 'locations_summary_table.tex')

# Create table
cbgs_summary_table_latex <- kable(cbgs_summary_table,
                                  format = 'latex',
                                  caption = 'Locations (CBGs) summary statistics.',
                                  digits = 2,
                                  booktabs = TRUE) %>%
    row_spec(row = 0, bold = TRUE) %>%
    pack_rows(index = c('Urban' = 7, 'Rural' = 7)) %>%
    add_footnote(paste('\\scriptsize{\\emph{Note}: Subset of data for',
                       month,
                       paste0(year, '.}')),
                 notation = 'none',
                 escape = FALSE) 
# Fix caption
add_to_beginning <- '\\begingroup\\footnotesize\n'
add_to_end <- '\\endgroup\n\\caption{Locations (CBGs) summary statistics.}\n\\label{location_summary_table}\n'
cbgs_summary_table_latex <- gsub('\n\n\\\\caption\\{[^\n]*\\}\n', '\n', cbgs_summary_table_latex)
cbgs_summary_table_latex <- gsub('\\centering\n', 
                                 paste0('\\centering\n',
                                        add_to_beginning), 
                                 cbgs_summary_table_latex,
                                 fixed = TRUE)
cbgs_summary_table_latex <- gsub('\\end{tabular}\n',
                                 paste0('\\end{tabular}\n',
                                        add_to_end
                                        ),
                                 cbgs_summary_table_latex,
                                 fixed = TRUE)
cbgs_summary_table_latex %>% cat(file = outfile)

# Within CBSA distribution of restaurant availability
percent_of_cbgs_with_restaurants <- cbgs %>%
    replace_na(list(rest_number = 0)) %>%
    mutate(rest_group = case_when(rest_number == 0 ~ '0',
                                  rest_number %in% c(1,2) ~ '1-2', 
                                  rest_number %in% c(3,4) ~ '3-4', 
                                  rest_number %in% c(4,5) ~ '4-5', 
                                  rest_number > 5 ~ '>5')) %>% 
    group_by(cbsa, rest_group) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(cbsa) %>% 
    mutate(share = n / sum(n)) %>% 
    select(-n) %>% 
    pivot_wider(names_from = rest_group, 
                values_from = share, 
                values_fill = list(share = 0.0)) %>% 
    pivot_longer(names_to = 'rest_group', 
                 cols = -cbsa, 
                 values_to = 'share')

# Produce a plot
location_rest_count <- ggplot(data = percent_of_cbgs_with_restaurants, 
                              aes(y = rest_group, x = share, fill = rest_group, color = rest_group)) +
    geom_density_ridges(alpha = 0.9, stat = "binline", bins = 30) + 
    theme_bw(base_family = 'Times') + 
    scale_fill_manual(guide = FALSE,
                      values = mycolorscheme3) + 
    scale_color_manual(guide = FALSE,
                      values = mycolorscheme3) +
    my_theme +
    xlab('Share of CBGs within CBSAs') +
    ylab('Restaurant number') 

ggsave(filename = file.path(plots_folder_path, 'location_rest_count.pdf'),
       device = cairo_pdf,
       plot = location_rest_count,
       width = 3.5,
       height = 2.1)

################################################################################
