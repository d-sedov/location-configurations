################################################################################
#
# FILE: pois-summary.R
#
# BY: Dmitry Sedov 
#
# CREATED: Mon Oct 07 2019 09:47:16 GMT+0200 (CEST)
#
# DESC: This file creates a table with POI summary statistics. 
#
################################################################################

# Import libraries
library(readr)
library(dplyr)
library(xtable)
library(kableExtra)

# Set the tables properties
# Footnote font size
options(xtable.size = 'footnotesize')
# Page top placement
options(xtable.table.placement = 't')


# Import data
pois_full <- read_csv('/Users/muser/dfolder/Research/urban/data/pois_full.csv')


# Transform the total minutes open variable
pois_full <- pois_full %>% 
    mutate(days_open = ifelse(time_ok & total_minutes_open > 0, total_minutes_open / (24 * 60), NA))


# Create the iOS share variable:
pois_full <- pois_full %>% 
    mutate(ios_share = ios / (ios + android))


# Create the distance from home (km) variable
pois_full <- pois_full %>% 
    mutate(distance_from_home_km = distance_from_home / 1000)


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
vars <- list('msa_bool',
             'days_open', 
             'area_m2',
             'phone_available',
             'raw_visit_counts',
             'raw_visitor_counts', 
             'distance_from_home_km', 
             'from_cbgs', 
             'median_dwell',
             'ios_share'
             )

# Variable names for presentation
vars_names <- c('Urban', 
                'Days open',
                'Area (sq. m.)',
                'Phone avail.',
                'Visits',
                'Visitors',
                'Dist. from home (km)',
                'From CBGs',
                'Median dwell',
                'iOS share')


# Create summary table
pois_summary_table <- bind_rows(lapply(vars, 
                                  function(x) {simple_summary(pois_full, x)}
                                  )
                           )


# Add the variable names column
pois_summary_table <- add_column(pois_summary_table, 
                                 variable = vars_names, 
                                 .before = 1) %>% 
    select(-v)


# Rename columns:
colnames(pois_summary_table) <- c('Variable',
                                  'NA, %', 
                                  'Q10', 
                                  'Q25',
                                  'Med',
                                  'Q75',
                                  'Q90',
                                  'Mean', 
                                  'SD')

# Save the summary statistics table
outfile = '/Users/muser/dfolder/Research/urban/output/tables/pois_summary_table.tex'
xtable(pois_summary_table,
       caption = "POIs summary statistics",
       label = "pois_summary_table") %>%
    xtable2kable(include.rownames = FALSE,
                 booktabs = TRUE) %>%
    kable_styling(latex_options = "striped") %>%
    cat(file = outfile)