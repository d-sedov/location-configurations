################################################################################
################################################################################
#
# FILE: restaurants-summary.R
#
# BY: Dmitry Sedov 
#
# CREATED: Wed Feb 12 2020
#
# DESC: This file creates a table with restaurant summary statistics and 
#       graphs with top brands / categories.
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
mycolorscheme3 <- c('#e70300', '#00279a', '#009500', '#722ab5', '#ffe200')

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
restaurants <- read_csv(file.path(input_folder_path, 'data_restaurants.csv'))

# Set price to missing if it is -1
restaurants <- restaurants %>% mutate(price = ifelse(price == -1, NA, price))

# Replace missing (null) categories with NAs
restaurants <- restaurants %>% mutate(categories = ifelse(categories == 'null', 
                                                          NA, 
                                                          categories))

# Transform the total minutes open variable
restaurants <- restaurants %>%
    mutate(perc_time_open = 100 * total_minutes_open / (24 * 60 * 31)) %>%
    mutate(perc_time_open = replace(perc_time_open,  
                               which(perc_time_open <= 0.1 | perc_time_open >= 100.1), 
                               NA))

# Encode 'urban' restaurants
restaurants <- restaurants %>%
    mutate(urban = !is.na(cbsa)) 

# Count restaurant's categories
restaurants <- restaurants %>%
    mutate(n_categories = sapply(categories, 
                                 function(x) {
                                     ifelse(is.na(x), 
                                            NA,
                                            ifelse(x == "[]",
                                                   0,
                                                   nrow(fromJSON(x))
                                                   )
                                            )
                                     }
                                 )
           )

# Extract restaurant's first category
restaurants <- restaurants %>%
    mutate(category1 = sapply(categories, 
                                 function(x) {
                                     ifelse(is.na(x) || x == "[]",
                                            NA,
                                            fromJSON(x)[1, 'title']
                                     )
                                 }
                              )
           )

################################################################################

#################### Construct summary stats table #############################

# Select variables for summary statistics construction
vars <- list('urban',
             'area_m2',
             'n_categories',
             'price',
             'rating',
             'perc_time_open',
             'raw_visit_counts'
)

# Variable names for presentation
vars_names <- c('Urban', 
                'Area (sq. m.)',
                '# Categories',
                'Price',
                'Rating',
                '% Time open',
                'Visits')

# Create summary table
restaurants_summary_table <- bind_rows(lapply(vars,
                                              function(x) {
                                                  simple_summary(restaurants, x)
                                                  }
                                              )
                                       )

restaurants_summary_table <- add_column(restaurants_summary_table, 
                                        variable = vars_names, 
                                        .before = 1) %>% 
    select(-v)

# Rename columns:
colnames(restaurants_summary_table) <- c('',
                                         '% NA', 
                                         'Q10', 
                                         'Q25',
                                         'Med',
                                         'Q75',
                                         'Q90',
                                         'Mean', 
                                         'SD')

# Save the summary statistics table
outfile = file.path(tables_folder_path, 'restaurants_summary_table.tex')
xtable(restaurants_summary_table,
       caption = 'Restaurants dataset summary statistics.',
       label = "restaurants_summary_table") %>%
    xtable2kable(include.rownames = FALSE,
                 booktabs = TRUE) %>%
    row_spec(row = 0, bold = TRUE) %>%
    add_footnote(paste('\\scriptsize{\\emph{Note}: Subset of data for',
                       month,
                       paste0(year, '.}')),
                 notation = 'none',
                 escape = FALSE) %>%
    cat(file = outfile)

################################################################################

################# Construct brands / categories graphs #########################

## Brands
# Select top brands by restaurant quantity
top_brands <- restaurants %>% 
    group_by(brands) %>% 
    summarise(Quantity = n(), 
              `Visits (thsd.)` = sum(raw_visit_counts, na.rm = T) / 1000) %>% 
    filter(!is.na(brands)) %>% 
    arrange(-Quantity) %>%
    top_n(10, Quantity)

# Transform top brands to long form
top_brands <- top_brands %>% 
    rename(Brand = brands) %>% 
    pivot_longer(-Brand, names_to = 'Type', 'values_to' = 'N')

# Plot
p1 <- ggplot(top_brands, aes(x = reorder(Brand, -N),
                            y = N, 
                            fill = Type)) + 
    geom_bar(stat = 'identity', 
             position = 'dodge', 
             width = 0.8) + 
    theme_bw(base_family = 'Times') +
    my_theme + 
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     vjust = 1)) +
    theme(legend.key.size = unit(0.125, 'inches'),
          legend.title=element_blank(),
          plot.margin = margin(5, 0, 5, 2, 'pt')) +
    xlab('') +
    ylab('') +
    scale_y_continuous(label = comma) +
    scale_fill_manual('Count', values = mycolorscheme3)

ggsave(filename = file.path(plots_folder_path, 'top_brands.pdf'),
       device = cairo_pdf,
       plot = p1,
       width = 3.5,
       height = 2.1)
embed_fonts(file.path(plots_folder_path, 'top_brands.pdf'))

## Categories
# Select top categories by restaurant quantity
top_categories <- restaurants %>% 
    group_by(category1) %>% 
    summarise(Quantity = n(), 
              `Visits (thsd.)` = sum(raw_visit_counts, na.rm = T) / 1000) %>% 
    filter(!is.na(category1)) %>% 
    arrange(-Quantity) %>%
    top_n(10, Quantity)

# Transform top categories to long form
top_categories <- top_categories %>% 
    rename(Category = category1) %>% 
    pivot_longer(-Category, names_to = 'Type', 'values_to' = 'N')

# Plot
p2 <- ggplot(top_categories, aes(x = reorder(Category, -N),
                            y = N, 
                            fill = Type)) + 
    geom_bar(stat = 'identity', 
             position = 'dodge',
             width = 0.8) + 
    theme_bw(base_family = 'Times') +
    my_theme +
    theme(axis.text.x = element_text(angle = 45,
                                     hjust = 1,
                                     vjust = 1)) +
    theme(legend.key.size = unit(0.125, 'inches'),
          legend.title=element_blank(),
          plot.margin = margin(5, 2, 5, 0, 'pt')) +
    xlab('') +
    ylab('') +
    scale_y_continuous(label = comma) +
    scale_fill_manual('Count', values = mycolorscheme3)

ggsave(filename = file.path(plots_folder_path, 'top_categories.pdf'),
       device = cairo_pdf,
       plot = p2,
       width = 3.5,
       height = 2.1)
embed_fonts(file.path(plots_folder_path, 'top_categories.pdf'))

p3 <- ggarrange(p1, 
                p2, 
                ncol = 2, nrow = 1, 
                common.legend = TRUE, legend = 'right',
                align = 'h')

ggsave(filename = file.path(plots_folder_path, 'top_brands_categories.pdf'),
       device = cairo_pdf,
       plot = p3,
       width = 4.85,
       height = 2.5)
embed_fonts(file.path(plots_folder_path, 'top_brands_categories.pdf'))

################################################################################
