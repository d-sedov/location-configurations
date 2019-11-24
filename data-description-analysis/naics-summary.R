################################################################################
#
# FILE: naics-summary.R
#
# BY: Dmitry Sedov 
#
# CREATED: Mon Oct 07 2019 14:29:39 GMT+0200 (CEST)
#
# DESC: This file creates a summary statistics table by NAICS 2 digit codes.
#
################################################################################

# Import libraries
library(readr)
library(dplyr)
library(xtable)
library(kableExtra)
library(knitr)

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

# Variables to summarise
vars <- c('days_open',
          'raw_visit_counts',
          'raw_visitor_counts',
          'distance_from_home_km',
          'from_cbgs'
          )

# Variable names for presentation
vars_names <- c('Days open', 
                'Visits',
                'Visitors',
                'Dist. from home (km)',
                'Origin CBGs')

# NAICs industry names data frame
naics_codes <- c(11, 21, 22, 23, 
                 31, 32, 33, 
                 42, 44, 45, 
                 48, 49, 
                 51, 52, 53,
                 54, 55, 56, 
                 61, 62, 
                 71, 72, 
                 81, 92)
naics_names <- c("Agriculture", "Mining", "Utilities", "Construction",
                 "Manufacturing (1)", "Manufacturing (2)", "Manufacturing (3)",
                 "Wholesale", "Retail (1)", "Retail (2)",
                 "Transportation", "Warehousing",
                 "Information", "Finance", "R. Estate",
                 "Professional", "Management", "Administrative",
                 "Education", "Health",
                 "Entertainment","Food + Accom.",
                 "Services", "Pub. Adm."
                 )
naics_codes_names <- data.frame(naics_first2 = naics_codes, 
                                Industry = naics_names)

# Compute totals by NAICS categories
naics_basic <- pois_full %>% 
    group_by(naics_first2) %>%
    summarise(`POIs` = n() / 1000, 
              `Visits` = sum(raw_visit_counts, na.rm = T) / 1000)

# Compute averages by NAICS categories
naics_extended <- pois_full %>% 
    group_by(naics_first2) %>% 
    summarise_at(vars, mean, na.rm = T)

# Compute missing visit percentages
naics_missing <- pois_full %>% 
    group_by(naics_first2) %>%
    summarise(visits_missing = 100 * sum(is.na(raw_visit_counts)) / n())

# Join summaries
naics_summary_table <- right_join(naics_codes_names, naics_basic) %>%
    inner_join(naics_extended) %>%
    inner_join(naics_missing) %>%
    arrange(-`Visits`) %>%
    select(-naics_first2)

# Column names change
cols <- c(`Days open` = 'days_open',
          `Visits` = 'raw_visit_counts',
          `Visitors` = 'raw_visitor_counts',
          `Km` = 'distance_from_home_km',
          `CBGs` = 'from_cbgs',
          `Visits` = 'visits_missing')
naics_summary_table <- naics_summary_table %>%
    rename(!!cols)

# Produce the LaTeX table
outfile = '/Users/muser/dfolder/Research/urban/output/tables/naics_summary_table.tex'
xtable(naics_summary_table,
       caption = 'NAICS 2-digit codes summary statistics',
       label = 'naics_summary_table',
       digits = c(0,0,2,0,1,1,1,2,2,2)) %>% 
    xtable2kable(include.rownames = FALSE,
                 booktabs = TRUE) %>%
    add_header_above(c(' ', 'Totals (thsd.)' = 2, 'Means' = 3, 'From' = 2, 'Missing (%)' = 1)) %>%
    kable_styling(latex_options = c('striped')) %>%
    cat(file = outfile)



