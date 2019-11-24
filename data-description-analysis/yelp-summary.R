################################################################################
#
# FILE: yelp-summary.R
#
# BY: Dmitry Sedov 
#
# CREATED: Wed Nov 20 2019
#
# DESC: This file creates a table with Yelp summary statistics. 
#
################################################################################

# Import libraries
library(readr)
library(dplyr)
library(xtable)
library(kableExtra)
library(stringr)

# Set the tables properties
# Footnote font size
options(xtable.size = 'footnotesize')
# Page top placement
options(xtable.table.placement = 't')

# Import data
yelp_description <- read_csv('/Users/muser/dfolder/Research/urban/data/yelp_description.csv')

# Count the number of categories corresponding to a restaurant
yelp_description <- yelp_description %>%
    mutate(categories = str_count(categories, 
                                  pattern = 'alias'),
           price = ifelse(price == -1, NA, price),
           phone_available = as.integer(phone_available)
           )

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

# Select variables for summary statistics construction
vars <- list('phone_available',
             'categories',
             'review_count', 
             'rating',
             'price')

# Variable names for presentation
vars_names <- c('Phone avail.',
                '# Categories', 
                'Review Count',
                'Rating',
                'Price')


# Create summary table
yelp_summary_table <- bind_rows(lapply(vars, 
                                       function(x) {simple_summary(yelp_description, x)}
                                       )
                                )


# Add the variable names column
yelp_summary_table <- add_column(yelp_summary_table, 
                                 variable = vars_names, 
                                 .before = 1) %>% 
    select(-v)


# Rename columns:
colnames(yelp_summary_table) <- c('Variable',
                                  'NA, %', 
                                  'Q10', 
                                  'Q25',
                                  'Med',
                                  'Q75',
                                  'Q90',
                                  'Mean', 
                                  'SD')

# Save the summary statistics table
outfile = '/Users/muser/dfolder/Research/urban/output/tables/yelp_summary_table.tex'
xtable(yelp_summary_table,
       caption = "Yelp summary statistics",
       label = "yelp_summary_table") %>%
    xtable2kable(include.rownames = FALSE,
                 booktabs = TRUE) %>%
    kable_styling(latex_options = "striped") %>%
    cat(file = outfile)